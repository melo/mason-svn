# Copyright (c) 1998 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::ApacheHandler;
require 5.004;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw(handler);

use strict;
use vars qw($AUTOLOAD $INTERP);
#JS - 6/30 - seems to infinite loop when using debug...help?!
#use Apache::Constants qw(OK DECLINED SERVER_ERROR NOT_FOUND);
sub OK { return 0 }
sub DECLINED { return -1 }
sub SERVER_ERROR { return 500 }
sub NOT_FOUND { return 404 }
use Data::Dumper;
use File::Basename;
use File::Path;
use File::Recurse;
use HTML::Mason::Interp;
use HTML::Mason::Commands;
use HTML::Mason::FakeApache;
use HTML::Mason::Tools qw(html_escape url_unescape);
use HTML::Mason::Utils;
use Apache::Status;

my @used = ($HTML::Mason::Commands::r);

my %fields =
    (
     interp => undef,
     output_mode => 'batch',
     error_mode => 'html',
     top_level_predicate => sub { return 1 },
     decline_dirs => 1,
     debug_mode => 'none',
     debug_perl_binary => '/usr/bin/perl',
     debug_handler_script => undef,
     debug_handler_proc => undef,
     debug_dir_config_keys => [],
     );
# Minor speedup: create anon. subs to reduce AUTOLOAD calls
foreach my $f (keys %fields) {
    no strict 'refs';
    *{$f} = sub {my $s=shift; return @_ ? ($s->{$f}=shift) : $s->{$f}};
}

sub new
{
    my $class = shift;
    my $self = {
	_permitted => \%fields,
	request_number => 0,
	%fields,
    };
    my (%options) = @_;
    while (my ($key,$value) = each(%options)) {
	if (exists($fields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::ApacheHandler::new: invalid option '$key'\n";
	}
    }
    die "HTML::Mason::ApacheHandler::new: must specify value for interp" if !$self->{interp};
    bless $self, $class;
    $self->_initialize;
    return $self;
}

sub _initialize {
    my ($self) = @_;

    my $interp = $self->interp;

    # ----------------------------
    # Add an HTML ::Mason menu item to the /perl-status page. Things we report:
    # -- Interp properties
    # -- loaded (cached) components
    Apache::Status->menu_item(
	'mason' => "HTML::Mason status", #item for HTML::Mason module
	 sub {
	     my($r,$q) = @_; #request and CGI objects
	     my(@strings);
	     
	     push (@strings,
		'<DL><DT><FONT SIZE="+1"><B>Interp object properties (startup options)</B></FONT><DD>',
		map("$_ = ".$interp->{$_}."<BR>\n", grep (ref $interp->{$_} eq '', sort keys %{$interp->{_permitted}})),
		'</DL>');
	     
	     push (@strings,
		'<DL><DT><FONT SIZE="+1"><B>Cached components</B></FONT><DD>',
		map("$_<BR>\n", sort keys %{$interp->{code_cache}}),
		'</DL>');
	     
	     return \@strings;     #return an array ref
	 }
    ) if $Apache::Status::VERSION; #only if Apache::Status is loaded

    
    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(debug preview)) {
	my @newdirs = mkpath($interp->data_dir."/$subdir",0,0775);
	$interp->push_files_written(@newdirs);
    }
    
    #
    # Send HTTP headers when the primary section is reached.
    #
    $interp->remove_hooks(name=>'http_header');
    $interp->add_hook(name=>'http_header',type=>'start_primary',order=>75,code=>\&send_http_header_hook);

    #
    # Allow global $r in components
    #
    my $line = "use vars qw(\$r);\n";
    my $pre = $interp->parser->preamble;
    $pre =~ s/$line//g;
    $interp->parser->preamble($pre."use vars qw(\$r);\n");
}

sub send_http_header_hook
{
    my ($interp) = @_;
    my $r = $interp->vars('server');
    $r->send_http_header if (!http_header_sent($r));
    $interp->suppress_hooks(name=>'http_header');
}

#
# Standard entry point for handling request
#
sub handle_request {
    my ($self,$r) = @_;
    my ($outsub, $retval, $argString, $debugMsg);
    my $outbuf = '';
    my $interp = $self->interp;

    #
    # construct (and truncate if necessary) the request to log at start
    #
    my $rstring = $r->server->server_hostname . $r->uri;
    $rstring .= "?".scalar($r->args) if defined(scalar($r->args));
    $rstring = substr($rstring,0,150).'...' if length($rstring) > 150;
    $interp->write_system_log('REQ_START', ++$self->{request_number},
			      $rstring);

    #
    # If output mode is 'batch', collect output in a buffer and
    # print at the end. If output mode is 'stream', send output
    # to client as it is produced.
    #
    if ($self->output_mode eq 'batch') {
        $outsub = sub { $outbuf .= $_[0] if defined($_[0]) };
	$interp->out_method($outsub);
    } elsif ($self->output_mode eq 'stream') {
	$outsub = sub { $r->print($_[0]) };
	$interp->out_method($outsub);
    }

    #
    # Get argument string
    #
    if ($r->method() eq 'GET') {
	$argString = $r->args();
    } elsif ($r->method() eq 'POST') {
	$argString = $r->content();
    }
    
    my $debugMode = $self->debug_mode;
    $debugMode = 'none' if (ref($r) eq 'HTML::Mason::FakeApache');
    my $debugState = $self->capture_debug_state($r,$argString)
	if ($debugMode eq 'all' or $debugMode eq 'error');
    $debugMsg = $self->write_debug_file($r,$debugState) if ($debugMode eq 'all');
    
    eval('$retval = handle_request_1($self, $r, $argString)');
    my $err = $@;
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# Take out date stamp and (eval nnn) prefix
	# Add server name, uri, referer, and agent
	#
	$err =~ s@^\[[^\]]*\] \(eval [0-9]+\): @@mg;
	$err = html_escape($err);
	my $referer = $r->header_in('Referer') || '<none>';
	my $agent = $r->header_in('User-Agent') || '';
	$err = sprintf("while serving %s %s (referer=%s, agent=%s)\n%s",$r->server->server_hostname,$r->uri,$referer,$agent,$err);

	if ($self->error_mode eq 'fatal') {
	    die ("System error:\n$err\n");
	} elsif ($self->error_mode eq 'html') {
	    if (!http_header_sent($r)) {
		$r->content_type('text/html');
		$r->send_http_header();
	    }
	    $r->print("<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n");
	    $debugMsg = $self->write_debug_file($r,$debugState) if ($debugMode eq 'error');
	    $r->print("<pre><font size=-1>\n$debugMsg\n</font></pre>\n") if defined($debugMsg);
	}
    } else {
	$r->print("\n<!--\n$debugMsg\n-->\n") if defined($debugMsg) && http_header_sent($r) && $r->header_out("Content-type") =~ /text\/html/;
	$r->print($outbuf) if $self->output_mode eq 'batch';
    }

    $interp->write_system_log('REQ_END', $self->{request_number}, $err_status);
    return ($err) ? &OK : $retval;
}

#
# Shorthand for various data subdirectories and files.
#
sub debug_dir { return shift->interp->data_dir . "/debug" }
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub write_debug_file
{
    my ($self, $r, $dref) = @_;
    my $user = $r->cgi_var('REMOTE_USER') || 'anon';
    my $outFile = sprintf("%d",int(rand(20))+1);
#$outFile =~ s/\d+/100/ if ($r->the_request =~ m@Reference/travel@);
    my $outDir = $self->debug_dir . "/$user";
    if (!-d $outDir) {
	mkpath($outDir,0,0755) or die "cannot create debug directory '$outDir'";
    }
    my $outPath = "$outDir/$outFile";
    my $outfh = new IO::File ">$outPath";
    if (!$outfh) {
	$r->warn("cannot open debug file '$outPath' for writing");
	return;
    }

    my $d = new Data::Dumper ([$dref],['dref']);
    my $o = '';
    $o .= "#!".$self->debug_perl_binary."\n";
    $o .= <<'PERL';
# -----------------------------
# Read command-line options for repeat counts (-rX) and profiling via
# Devel::DProf (-p). As component runs in profile mode, component
# coderefs are accumulated in %CODEREF_NAME
# -----------------------------
BEGIN {
    use IO::File;
    use File::Copy;
    use Getopt::Std;
    getopt('r');   # r=repeat count, p=user profile req, P=re-entrant profile call
    $opt_r ||= 1;
    
    if ($opt_p) {
	print STDERR "Profiling request ...";
	# re-enter with different option (no inf. loops, please)
	system ("perl", "-d:DProf", $0, "-P", "-r$opt_r");
    
# -----------------------------
# When done, merge named coderefs in tmon.mason with those in tmon.out,
# then run dprofpp
# -----------------------------
	my $fh = new IO::File '< ./tmon.mason' or die "Missing file: tmon.mason";
	foreach (<$fh>) { chomp;  my ($k,$v) = split(/\t/);  $::CODEREF_NAME{$k} = $v; }
	$fh->close;
    
	my $tmonout = new IO::File '< ./tmon.out' or die "Missing file: tmon.out";
	my $tmontmp = new IO::File '> ./tmon.tmp' or die "Couldn't write file: tmon.tmp";
	my $regex = quotemeta(join('|', keys %::CODEREF_NAME));
	$regex =~ s/\\\|/|/g;   #un-quote the pipe chars
	while (<$tmonout>) {
	    s/HTML::Mason::Commands::($regex)/$::CODEREF_NAME{$1}/;
	    print $tmontmp $_
	}
	$tmonout->close; $tmontmp->close;
	copy('tmon.tmp' => 'tmon.out') or die "$!";
	unlink('tmon.tmp');
        print STDERR "\nRunning dprofpp ...\n";
	exec('dprofpp') or die "Couldn't execute dprofpp";
    }
}

PERL
    $o .= "BEGIN { \$HTML::Mason::IN_DEBUG_FILE = 1; require '".$self->debug_handler_script."' }\n\n";
    $o .= <<'PERL';
if ($opt_P) {
    open SAVEOUT, ">&STDOUT";    # stifle component output while profiling
    open STDOUT, ">/dev/null";
}
for (1 .. $opt_r) {
print STDERR '.' if ($opt_P and $opt_r > 1);
PERL
    $o .= "my ";
    $o .= $d->Dumpxs;
    $o .= 'my $r = HTML::Mason::ApacheHandler::simulate_debug_request($dref);'."\n";
    $o .= 'my $status = '.$self->debug_handler_proc."(\$r);\n";
    $o .= 'print "return status: $status\n";'."\n}\n\n";
    $o .= <<'PERL';
if ($opt_P) {
    my $fh = new IO::File '>./tmon.mason' or die "Couldn't write tmon.mason";
    print $fh map("$_\t$HTML::Mason::CODEREF_NAME{$_}\n", keys %HTML::Mason::CODEREF_NAME);
    $fh->close;
}
PERL
    $outfh->print($o);
    $outfh->close();
    chmod(0775,$outPath);

    my $debugMsg = "Debug file is '$outFile'.\nFull debug path is '$outPath'.\n";
    return $debugMsg;
}

sub capture_debug_state
{
    my ($self, $r, $argString) = @_;
    my (%d,$expr);

    $expr = '';
    foreach my $field (qw(method method_number bytes_sent the_request proxyreq header_only protocol uri filename path_info requires auth_type auth_name document_root allow_options content_type content_encoding content_language status status_line args)) {
	$expr .= "\$d{$field} = \$r->$field;\n";
    }
    eval($expr);

    $d{headers_in} = {$r->headers_in};
    $d{headers_out} = {$r->headers_out};
    $d{cgi_env} = {$r->cgi_env};
    $d{dir_config} = {};
    foreach my $key (@{$self->debug_dir_config_keys}) {
	$d{dir_config}->{$key} = $r->dir_config($key);
    }
    
    if ($r->method eq 'POST') {
	$d{content} = $argString;
    }

    $expr = '';
    $d{server} = {};
    foreach my $field (qw(server_admin server_hostname port is_virtual names)) {
	$expr .= "\$d{server}->{$field} = \$r->server->$field;\n";
    }
    eval($expr);
    
    $expr = '';
    $d{connection} = {};
    foreach my $field (qw(remote_host remote_ip local_addr remote_addr remote_logname user auth_type aborted)) {
	$expr .= "\$d{connection}->{$field} = \$r->connection->$field;\n";
    }
    eval($expr);

    return {%d};
}

sub handle_request_1
{
    my ($self,$r,$argString) = @_;
    my $interp = $self->interp;
    my $compRoot = $interp->comp_root;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    if (-d $r->filename) {
	if ($self->decline_dirs) {
	    return DECLINED;
	} else {
	    $r->content_type(undef);
	}
    }
    
    #
    # Compute the component path by deleting the component root
    # directory from the front of Apache's filename.  If the
    # substitute fails, we must have an URL outside Mason's component
    # space; return not found.
    #
    my $compPath = $r->filename;
    if (!($compPath =~ s/^$compRoot//)) {
	$r->warn("Mason: filename (\"".$r->filename."\") is outside component root (\"$compRoot\"); returning 404.");
	return NOT_FOUND;
    }
    $compPath =~ s@/$@@ if $compPath ne '/';
    while ($compPath =~ s@//@/@) {}

    #
    # Try to load the component; if not found, try dhandlers
    # ("default handlers"); otherwise returned not found.
    #
    my @info;
    if (!(@info = $interp->load($compPath))) {
	my $p = $compPath;
	my $pathInfo = $r->path_info;
	while (!(@info = $interp->load("$p/dhandler")) && $p) {
	    my ($basename,$dirname) = fileparse($p);
	    $pathInfo = "/$basename$pathInfo";
	    $p = substr($dirname,0,-1);
	}
	if (@info) {
	    $compPath = "$p/dhandler";
	    $r->path_info($pathInfo);
	} else {
	    $r->warn("Mason: no component corresponding to filename \"".$r->filename."\", comp path \"$compPath\"; returning 404.");
	    return NOT_FOUND;
	}
    }
    my $srcfile = $info[1];

    #
    # Decline if file does not pass top level predicate.
    #
    if (!$self->top_level_predicate->($srcfile)) {
	$r->warn("Mason: component file \"$srcfile\" does not pass top-level predicate; returning 404.");
	return NOT_FOUND;
    }
    
    #
    # Parse arguments into key/value pairs. Represent multiple valued
    # keys with array references.
    #
    my (%args);
    if ($argString) {
	my (@pairs) = split('&',$argString);
	foreach my $pair (@pairs) {
	    my ($key,$value) = split('=',$pair);
	    $key = url_unescape($key);
	    $value = url_unescape($value);
	    if (exists($args{$key})) {
		if (ref($args{$key})) {
		    $args{$key} = [@{$args{$key}},$value];
		} else {
		    $args{$key} = [$args{$key},$value];
		}
	    } else {
		$args{$key} = $value;
	    }
	}
    }
    $argString = '' if !defined($argString);

    #
    # Set up interpreter global variables.
    #
    $interp->vars(http_input=>$argString);
    $interp->vars(server=>$r);
    local $HTML::Mason::Commands::r = $r;
    
    return $interp->exec($compPath,%args);
}

sub simulate_debug_request
{
    my ($infoRef) = @_;
    my %info = %$infoRef;
    my $r = new HTML::Mason::FakeApache;

    while (my ($key,$value) = each(%{$info{server}})) {
	$r->{server}->{$key} = $value;
    }
    while (my ($key,$value) = each(%{$info{connection}})) {
	$r->{connection}->{$key} = $value;
    }
    delete($info{server});
    delete($info{connection});
    while (my ($key,$value) = each(%info)) {
	$r->{$key} = $value;
    }
    
    return $r;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent
{
    my ($r) = @_;
    my $sent = $r->header_out("Content-type");
    return $sent;
}

sub AUTOLOAD {
    my $self = shift;
    my $type = ref($self) or die "autoload error: bad function $AUTOLOAD";

    my $name = $AUTOLOAD;
    $name =~ s/.*://;   # strip fully-qualified portion
    return if $name eq 'DESTROY';

    die "No such function `$name' in class $type";
}
1;

#
# Apache-specific Mason commands
#

package HTML::Mason::Commands;
sub mc_dhandler_arg ()
{
    my $r = $INTERP->vars('server');
    die "mc_dhandler_arg: must be called in Apache environment" if !$r;
    return substr($r->path_info,1);
}

sub mc_suppress_http_header
{
    if ($_[0]) {
	$INTERP->suppress_hooks(name=>'http_header');
    } else {
	$INTERP->unsuppress_hooks(name=>'http_header');
    }
}

__END__
