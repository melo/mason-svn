# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;
require 5.004;

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC REQUEST OBJECT
#
package HTML::Mason::Request::ApacheHandler;
use vars qw(@ISA);
@ISA = qw(HTML::Mason::Request);

use HTML::Mason::MethodMaker
    ( read_write => [ qw( ah apache_req ) ] );

# Fields that can be set in new method, with defaults
my %reqfields =
    (ah => undef,
     apache_req => undef,
     cgi_object => undef,
     );

sub new
{
    my ($class,%options) = @_;
    my $interp = $options{interp} or die "HTML::Mason::Request::ApacheHandler::new: must specify interp\n";
    delete $options{interp};
    my $self = $class->SUPER::new(interp=>$interp);
    while (my ($key,$value) = each(%options)) {
	if (exists($reqfields{$key})) {
	    $self->{$key} = $value;
	} else {
	    die "HTML::Mason::Request::ApacheHandler::new: invalid option '$key'\n";
	}
    }
    return $self;
}

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self, $content) = @_;
    $self->SUPER::flush_buffer($content);
    $self->apache_req->rflush;
}

sub cgi_object
{
    my ($self) = @_;

    if ($HTML::Mason::ApacheHandler::ARGS_METHOD eq 'CGI')
    {
	$self->{cgi_object} ||= CGI->new('');
    }
    else
    {
	die "Can't call cgi_object method unless CGI.pm was used to handle incoming arguments.\n";
    }

    return $self->{cgi_object};
}

#----------------------------------------------------------------------
#
# APACHEHANDLER OBJECT
#
package HTML::Mason::ApacheHandler;

#JS - 6/30 - seems to infinite loop when using debug...help?!
#use Apache::Constants qw(OK DECLINED SERVER_ERROR NOT_FOUND);
sub OK { return 0 }
sub DECLINED { return -1 }
sub SERVER_ERROR { return 500 }
sub NOT_FOUND { return 404 }
use Data::Dumper;
use File::Path;
use HTML::Mason::Interp;
use HTML::Mason::Commands;
use HTML::Mason::FakeApache;
use HTML::Mason::Tools qw(dumper_method html_escape pkg_installed);
use HTML::Mason::Utils;
use Apache::Status;

use HTML::Mason::MethodMaker
    ( read_write => [ qw( apache_status_title
			  auto_send_headers

			  debug_dir_config_keys
			  debug_mode
			  debug_handler_proc
			  debug_handler_script
			  debug_perl_binary

			  decline_dirs
			  error_mode
			  interp
			  output_mode
			  top_level_predicate ) ]
      );

# use() params. Assign defaults, in case ApacheHandler is only require'd.
use vars qw($LOADED $ARGS_METHOD);
$LOADED = 0;
$ARGS_METHOD = undef;

my @used = ($HTML::Mason::IN_DEBUG_FILE);

# Fields that can be set in new method, with defaults
my %fields =
    (
     apache_status_title => 'mason',
     auto_send_headers => 1, 
     decline_dirs => 1,
     error_mode => 'html',
     interp => undef,
     output_mode => undef,    # deprecated - now interp->out_mode
     top_level_predicate => undef,
     debug_mode => 'none',
     debug_perl_binary => '/usr/bin/perl',
     debug_handler_script => undef,
     debug_handler_proc => undef,
     debug_dir_config_keys => [],
     );

sub import
{
    shift; # class not needed

    return if $LOADED;

    my %params = @_;

    # safe default.
    $params{args_method} ||= 'CGI';
    if ($params{args_method} eq 'CGI')
    {
	eval 'use CGI';
	die $@ if $@;
	$ARGS_METHOD = '_cgi_args';
    }
    elsif ($params{args_method} eq 'mod_perl')
    {
	eval 'use Apache::Request;';
	die $@ if $@;
	$ARGS_METHOD = '_mod_perl_args';
    }
    else
    {
	die "Invalid args_method parameter ('$params{args_method}') given to HTML::Mason::ApacheHandler in 'use'\n";
    }

    $LOADED = 1;
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

my %status_sub_defined = ();

sub _initialize {
    my ($self) = @_;

    my $interp = $self->interp;

    if ($Apache::Status::VERSION) {
	# Add an HTML::Mason menu item to the /perl-status page. Things we report:
	# -- Interp properties
	# -- loaded (cached) components
	my $name = $self->apache_status_title;
	unless ($status_sub_defined{$name}) {

	    my $title;
	    if ($name eq 'mason') {
		$title='HTML::Mason status';
	    } else {
		$title=$name;
		$name=~s/\W/_/g;
	    }

	    my $statsub = sub {
		my ($r,$q) = @_; # request and CGI objects
		return [] if !defined($r);
		my @strings = ();

		push (@strings,
		      qq(<FONT size="+2"><B>$self->apache_status_title</B></FONT><BR><BR>),
		      $self->interp_status);

		return \@strings;     # return an array ref
	    };
	    Apache::Status->menu_item ($name,$title,$statsub);
	    $status_sub_defined{$name}++;
	}
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(debug preview)) {
	my @newdirs = mkpath($interp->data_dir."/$subdir",0,0775);
	$interp->push_files_written(@newdirs);
    }

    #
    # Allow global $r in components
    #
    $interp->parser->allow_globals(qw($r));
}

#
# Generate an array that describes Interp's current status
#
sub interp_status
{
    my ($interp) = $_[0]->interp;

    my @strings;
    push @strings,
        qq(<DL><DT><FONT SIZE="+1"><B>Interp object properties</B></FONT>\n),
        qq(<DT><B>Startup options</B>\n);


    push @strings,
        map {"<DD><TT>$_ = ".(defined($interp->{$_}) ? 
                                $interp->{$_} : '<I>undef</I>'
                             )."</TT>\n" 
            } grep ref $interp->{$_} eq '', sort keys %{$interp->{_permitted}};

    push @strings, '</DL>',
            '<DL><DT><FONT SIZE="+1"><B>Cached components</B></FONT><DD>';

    if(my $cache = $interp->code_cache)
    {
	my $string;
	foreach my $key (sort keys %$cache) {
	    $string .= sprintf("<TT>%s (%s)</TT><BR>\n",$key,scalar(localtime($cache->{$key}->{lastmod})));
	}
	push (@strings, $string);
    } else {
        push @strings, '<I>None</I>';
    }     
    push @strings, '</DL>';
    return @strings;
}         

#
# Standard entry point for handling request
#
sub handle_request {

    #
    # Why do we use $apreq instead of $r here? A scoping bug in certain
    # versions of Perl 5.005 was getting confused about $r being used
    # in components, and the easiest workaround was to rename "$r" to
    # something else in this routine.  Go figure...
    # -jswartz 5/23
    #
    my ($self,$apreq) = @_;
    my ($outsub, $retval);
    my $outbuf = '';
    my $interp = $self->interp;
    $self->{request_number}++;

    #
    # Construct (and truncate if necessary) the request to log at start
    #
    if ($interp->system_log_event_check('REQ_START')) {
	my $rstring = $apreq->server->server_hostname . $apreq->uri;
	$rstring .= "?".scalar($apreq->args) if defined(scalar($apreq->args));
	$rstring = substr($rstring,0,150).'...' if length($rstring) > 150;
	$interp->write_system_log('REQ_START', $self->{request_number},
				  $rstring);
    }

    #
    # Determine debug file mode. Turn it off regardless if we are
    # already operating from a debug file.
    #
    my $debugMode = $self->debug_mode;
    $debugMode = 'none' if $HTML::Mason::IN_DEBUG_FILE;

    #
    # Capture debug state as early as possible, before we start messing with $apreq.
    #
    my $debugState;
    $debugState = $self->capture_debug_state($apreq)
	if ($debugMode eq 'all' or $debugMode eq 'error');

    #
    # Create an Apache-specific request with additional slots.
    #
    my $request = new HTML::Mason::Request::ApacheHandler
	(ah=>$self,
	 interp=>$interp,
	 apache_req=>$apreq,
	 cgi_object=>$self->{cgi_object},
	 );
    
    eval { $retval = $self->handle_request_1($apreq, $request, $debugState) };
    my $err = $@;
    my $err_code = $request->error_code;
    undef $request;  # ward off memory leak
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# If first component was not found, return 404. In case of
	# POST we must trick Apache into not reading POST content
	# again. Wish there were a more standardized way to do this...
	#
	if (defined($err_code) and $err_code eq 'top_not_found') {
	    if ($apreq->method eq 'POST') {
		$apreq->method('GET');
		$apreq->headers_in->unset('Content-length');
	    }
	    return NOT_FOUND;
	}

	#
	# Take out date stamp and (eval nnn) prefix
	# Add server name, uri, referer, and agent
	#
	$err =~ s@^\[[^\]]*\] \(eval [0-9]+\): @@mg;
	$err = html_escape($err);
	my $referer = $apreq->header_in('Referer') || '<none>';
	my $agent = $apreq->header_in('User-Agent') || '';
	$err = sprintf("while serving %s %s (referer=%s, agent=%s)\n%s",$apreq->server->server_hostname,$apreq->uri,$referer,$agent,$err);

	if ($self->error_mode eq 'fatal') {
	    die ("System error:\n$err\n");
	} elsif ($self->error_mode eq 'html') {
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print("<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n");
	    if ($debugMode eq 'error' or $debugMode eq 'all') {
		my $debugMsg = $self->write_debug_file($apreq,$debugState);
		print("<pre><font size=-1>\n$debugMsg\n</font></pre>\n");
	    }
	}
    } else {
	if ($debugMode eq 'all') {
	    my $debugMsg = $self->write_debug_file($apreq,$debugState);
	    print "\n<!--\n$debugMsg\n-->\n" if (http_header_sent($apreq) && !$apreq->header_only && $apreq->header_out("Content-type") =~ /text\/html/);
	}
    }

    $interp->write_system_log('REQ_END', $self->{request_number}, $err_status);
    return ($err) ? &OK : (defined($retval)) ? $retval : &OK;
}

#
# Shorthand for various data subdirectories and files.
#
sub debug_dir { return shift->interp->data_dir . "/debug" }
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub write_debug_file
{
    my ($self, $r, $dref) = @_;
    my $user = $r->connection->user || 'anon';
    my $outFile = sprintf("%d",int(rand(20))+1);
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
    $o .= dumper_method($d);
    $o .= 'my $r = HTML::Mason::ApacheHandler::simulate_debug_request($dref);'."\n";
    $o .= 'local %ENV = (%ENV,%{$dref->{ENV}});'."\n";
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
    my ($self, $r) = @_;
    my (%d,$expr);

    $expr = '';
    foreach my $field (qw(allow_options auth_name auth_type bytes_sent no_cache content_encoding content_languages content_type document_root filename header_only method method_number path_info protocol proxyreq requires status status_line the_request uri as_string get_remote_host get_remote_logname get_server_port is_initial_req is_main)) {
	$expr .= "\$d{$field} = \$r->$field;\n";
    }
    eval($expr);
    warn "error creating debug file: $@\n" if $@;

    if (pkg_installed('Apache::Table')) {
	$expr = "my \$href;\n";
	foreach my $field (qw(headers_in headers_out err_headers_out notes dir_config subprocess_env)) {
	    $expr .= "\$href = scalar(\$r->$field); \$d{$field} = {\%\$href};\n";
	}
	eval($expr);
	warn "error creating debug file: $@\n" if $@;
    } else {
	foreach my $field (qw(headers_in headers_out err_headers_out notes dir_config subprocess_env)) {
	    $d{$field} = {};
	}
    }
    
    $d{'args@'} = [$r->args];
    $d{'args$'} = scalar($r->args);
    
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

    $d{ENV} = {%ENV};

    return {%d};
}

sub handle_request_1
{
    my ($self,$r,$request,$debugState) = @_;
    my $interp = $self->interp;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    if (-d $r->finfo) {
	if ($self->decline_dirs) {
	    return DECLINED;
	} else {
	    $r->content_type(undef);
	}
    }

    #
    # Append path_info if filename does not represent an existing file
    # (mainly for dhandlers).
    #
    my $pathname = $r->filename;
    $pathname .= $r->path_info unless -f $r->finfo;

    #
    # Compute the component path via the resolver.
    #
    my $comp_path = $interp->resolver->file_to_path($pathname,$interp);
    return NOT_FOUND unless $comp_path;

    #
    # Decline if file does not pass top level predicate.
    #
    if (-f $r->finfo and defined($self->{top_level_predicate})) {
	return NOT_FOUND unless $self->{top_level_predicate}->($r->filename);
    }

    #
    # Parse arguments. $ARGS_METHOD is set by the import subroutine
    # (_cgi_args or _mod_perl_args).  We pass a reference to $r because
    # _mod_perl_args upgrades $r to the Apache::Request object.
    # 
    # When inside debug file, get arguments from special saved hash.
    # This circumvents POST content issues.
    #
    my %args;
    die "ARGS_METHOD not defined! Did you 'use HTML::Mason::ApacheHandler'?" unless defined($ARGS_METHOD);
    if ($HTML::Mason::IN_DEBUG_FILE) {
	%args = %{$r->{args_hash}};
    } else {
	%args = $self->$ARGS_METHOD(\$r);
    }
    $debugState->{args_hash} = \%args if $debugState;

    #
    # Deprecated output_mode parameter - just pass to request out_mode.
    #
    if (my $mode = $self->output_mode) {
	$request->out_mode($mode);
    }

    #
    # Set up interpreter global variables.
    #
    $interp->set_global(r=>$r);

    #
    # Craft the out method for this request to handle automatic http
    # headers.
    #
    my $retval;
    if ($self->auto_send_headers) {
	my $headers_sent = 0;
	my $delay_buf = '';
	my $out_method = sub {
	    # Check to see if the headers have been sent, first by fast
	    # variable check, then by slightly slower $r check.
	    unless ($headers_sent) {
		unless (http_header_sent($r)) {
		    # If header has not been sent, buffer initial whitespace
		    # so as to delay headers.
		    if ($_[0] !~ /\S/) {
			$delay_buf .= $_[0];
			return;
		    } else {
			$r->send_http_header();
			
			# If this is a HEAD request and our Mason request is
			# still active, abort it.
			if ($r->header_only) {
			    $request->abort if $request->depth > 0;
			    return;
			}
		    }
		}
		unless ($delay_buf eq '') {
		    $interp->out_method->($delay_buf);
		    $delay_buf = '';
		}
		$headers_sent = 1;
	    }
	    $interp->out_method->($_[0]);

	    # A hack, but good for efficiency in stream mode: change the
	    # current sink of the request so all this is bypassed for the
	    # remainder of this component and its children.
	    $request->top_stack->{sink} = $interp->out_method if $request->out_mode eq 'stream' and $request->top_stack->{sink} eq $request->out_method;
	};
	$request->out_method($out_method);

	$retval = $request->exec($comp_path, %args);

	# On a success code, send headers and any buffered whitespace
	# if it has not already been sent. On an error code, leave it
	# to Apache to send the headers.
	if (!$headers_sent and (!$retval or $retval==200)) {
	    $r->send_http_header() unless http_header_sent($r);
	    $interp->out_method->($delay_buf) unless $delay_buf eq '';
	}
    } else {
	$retval = $request->exec($comp_path, %args);
    }
    undef $request; # ward off leak
    return $retval;
}

#
# Get %args hash via CGI package
#
sub _cgi_args
{
    my ($self, $rref) = @_;

    my $r = $$rref;
    my $q;
    unless ($r->method eq 'GET' && !scalar($r->args)) {
        $self->{cgi_object} = CGI->new;
    }

    return unless exists $self->{cgi_object};

    my %args;
    foreach my $key ( $self->{cgi_object}->param ) {
	foreach my $value ( $self->{cgi_object}->param($key) ) {
	    if (exists($args{$key})) {
		if (ref($args{$key}) eq 'ARRAY') {
		    push @{ $args{$key} }, $value;
		} else {
		    $args{$key} = [$args{$key}, $value];
		}
	    } else {
		$args{$key} = $value;
	    }
	}
    }

    return %args;
}

#
# Get %args hash via Apache::Request package. As a side effect, assign the
# new Apache::Request package back to $r.
#
sub _mod_perl_args
{
    my ($self, $rref) = @_;

    my $apr = Apache::Request->new($$rref);
    $$rref = $apr;

    return unless $apr->param;

    my %args;
    foreach my $key ( $apr->param ) {
	foreach my $value ( $apr->param($key) ) {
	    if (exists($args{$key})) {
		if (ref($args{$key}) eq 'ARRAY') {
		    push @{ $args{$key} }, $value;
		} else {
		    $args{$key} = [$args{$key}, $value];
		}
	    } else {
		$args{$key} = $value;
	    }
	}
    }

    return %args;
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
sub http_header_sent { shift->header_out("Content-type") }


#----------------------------------------------------------------------
#
# APACHEHANDLER MASON COMMANDS
#
package HTML::Mason::Commands;
use vars qw($m);
# no longer needed
sub mc_suppress_http_header {}

1;
