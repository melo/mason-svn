# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
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

    if ($HTML::Mason::ApacheHandler::ARGS_METHOD ne '_cgi_args') {
	die "Can't call cgi_object method unless CGI.pm was used to handle incoming arguments.\n";
    } elsif (defined($_[1])) {
	$self->{cgi_object} = $_[1];
    } else {
	# We may not have created a CGI object if, say, request was a
	# GET with no query string. Create one on the fly if necessary.
	$self->{cgi_object} ||= CGI->new('');
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
use HTML::Mason::Error qw(error_process error_display_html);
use HTML::Mason;
use HTML::Mason::Commands;
use HTML::Mason::FakeApache;
use HTML::Mason::Tools qw(dumper_method html_escape make_fh pkg_installed);
use HTML::Mason::Utils;
use Params::Validate qw(:all);
use Apache;
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
use vars qw($ARGS_METHOD $AH $VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

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

# If loaded via a PerlModule directive in a conf file we are being
# loaded via 'require', not 'use'.  However, the import routine _must_
# be called at some point during startup or things go boom.

# If mod_perl <= 1.21 we will call import later (in the handler sub)
# to make sure we can see the user's MasonArgsMethod setting (if there
# is one)
__PACKAGE__->import if $mod_perl::VERSION > 1.21;

sub import
{
    shift; # class not needed

    return if defined $ARGS_METHOD;

    # We were called by the internal call a few lines back and it
    # seems that we're not being loaded in a conf file.  We will
    # expect to be called again via a 'use' line in the handler.pl
    # file.  This is a bit of a hack, I must say.
    return if caller eq __PACKAGE__ && ! _in_apache_conf_file();

    my %params = @_;

    if (_in_apache_conf_file())
    {
	$params{args_method} = _get_string_param('ArgsMethod');
    }

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

    _make_ah() if _in_apache_conf_file() && ! _get_boolean_param('MultipleConfig');
}

# This is my best guess as to whether we are being configured via the
# conf file or not.  Without a comp root it will blow up sooner or
# later anyway.  This may not be the case in the future though.
sub _in_apache_conf_file
{
    # We don't want to try to read the configuration til we're in a
    # request if mod_perl <= 1.21
    return 0 if $mod_perl::VERSION <= 1.21 && ( $Apache::Server::Starting || $Apache::ServerStarting ||
						$Apache::Server::ReStarting || $Apache::ServerReStarting );
    return $ENV{MOD_PERL} && ( _get_list_param('CompRoot') ||
			       _get_boolean_param('MultipleConfig') );
}

sub _make_ah
{
    return $AH if $AH && ! _get_boolean_param('MultipleConfig');

    my %p;

    $p{apache_status_title} = _get_string_param('ApacheStatusTitle');
    $p{auto_send_headers} = _get_boolean_param('AutoSendHeaders');
    $p{debug_handler_proc} = _get_string_param('DebugHandlerProc');
    $p{debug_handler_script} = _get_string_param('DebugHandlerScript');
    $p{debug_mode} = _get_string_param('DebugMode');
    $p{debug_perl_binary} = _get_string_param('DebugPerlBinary');
    $p{decline_dirs} = _get_boolean_param('DeclineDirs');
    $p{error_mode} = _get_string_param('ErrorMode');
    $p{top_level_predicate} = _get_code_param('TopLevelPredicate');

    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    $AH = HTML::Mason::ApacheHandler->new( interp => _make_interp(),
					   %p,
					 );

    return $AH;
}

sub _make_interp
{
    my %p;

    $p{allow_recursive_autohandlers} = _get_boolean_param('AllowRecursiveAutohandlers');
    $p{autohandler_name}    = _get_string_param('AutohandlerName');
    $p{code_cache_max_size} = _get_string_param('CodeCacheMaxSize');
    $p{current_time}        = _get_string_param('CurrentTime');
    $p{data_cache_dir}      = _get_string_param('DataCacheDir');
    $p{dhandler_name}       = _get_string_param('DhandlerName');
    $p{die_handler}         = _get_code_param('DieHandler');
    $p{max_recurse}         = _get_string_param('MaxRecurse');
    $p{out_method}          = _get_code_param('OutMethod');
    $p{out_mode}            = _get_string_param('OutMode');
    $p{preloads}              = [ _get_list_param('Preloads') ];
    delete $p{preloads} unless @{ $p{preloads} };
    $p{static_file_root}      = _get_string_param('StaticFileRoot');
    $p{system_log_events}     = _get_string_param('SystemLogEvents');
    $p{system_log_file}       = _get_string_param('SystemLogFile');
    $p{system_log_separator}  = _get_string_param('SystemLogSepartor');
    $p{use_autohandlers}      = _get_boolean_param('UseAutohandlers');
    $p{use_data_cache}        = _get_boolean_param('UseDataCache');
    $p{use_dhandlers}         = _get_boolean_param('UseDhandlers');
    $p{use_object_files}      = _get_boolean_param('UseObjectFiles');
    $p{use_reload_file}       = _get_boolean_param('UseReloadFile');

    my @comp_root = _get_list_param('CompRoot', 1);
    if (@comp_root == 1 && $comp_root[0] !~ /=>/)
    {
	$p{comp_root} = $comp_root[0];
    }
    else
    {
	my @root;
	foreach my $root (@comp_root)
	{
	    my ($k, $v) = split /\s*=>\s*/, $root;
	    die "Configuration parameter MasonCompRoot must be either a singular value or a multiple 'hash' values like 'foo => /home/mason/foo'"
		unless defined $k && defined $v;
	    push @{ $p{comp_root} }, [ $k => $v ];
	}
    }
    $p{data_dir} = _get_string_param('DataDir', 1);

    # If not defined we'll use the defaults
    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    my $interp = HTML::Mason::Interp->new( parser => _make_parser(),
					   %p,
					 );

    # if version <= 1.21 then these files shouldn't be created til
    # after a fork so they should have the right ids anyway
    if ($interp->files_written && $mod_perl::VERSION > 1.21 && ! ($> || $<))
    {
	chown Apache->server->uid, Apache->server->gid, $interp->files_written
	    or die "Can't change ownership of files written by interp object\n";
    }

    return $interp;
}

sub _make_parser
{
    my %p;
    $p{allow_globals} = [ _get_list_param('AllowGlobals') ];
    delete $p{allow_globals} unless @{ $p{allow_globals} };

    $p{default_escape_flags} = _get_string_param('DefaultEscapeFlags');
    $p{ignore_warnings_expr} = _get_string_param('IgnoreWarningsExpr');
    $p{in_package} = _get_string_param('InPackage');
    $p{postamble} = _get_string_param('Postamble');
    $p{preamble} = _get_string_param('Preamble');
    $p{taint_check} = _get_boolean_param('TaintCheck') || 0;
    $p{use_strict} = _get_boolean_param('UseStrict');

    $p{preprocess} = _get_code_param('Preprocess');
    $p{postprocess} = _get_code_param('Postprocess');

    # If not defined we'll use the defaults
    foreach (keys %p)
    {
	delete $p{$_} unless defined $p{$_};
    }

    return HTML::Mason::Parser->new(%p);
}

sub _get_string_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return $val;
}

sub _get_boolean_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return $val;
}

sub _get_code_param
{
    my ($p, $val) = _get_val(@_[0, 1]);

    return unless $val;

    my $sub_ref = eval $val;

    die "Configuration parameter '$p' is not valid perl:\n$@\n"
	if $@;

    return $sub_ref;
}

sub _get_list_param
{
    my ($p, @val) = _get_val(@_[0,1], 1);
    if (@val == 1 && ! defined $val[0])
    {
	@val = ();
    }

    return @val;
}

sub _get_val
{
    my ($p, $required, $wantarray) = @_;
    $p = "Mason$p";

    if ( $mod_perl::VERSION <= 1.21 && ( $Apache::Server::Starting || $Apache::ServerStarting ||
					 $Apache::Server::ReStarting || $Apache::ServerReStarting ) )
    {
	die "Can't get configuration info during server startup with mod_perl <= 1.21";
    }
    my $c = Apache->request ? Apache->request : Apache->server;

    my @val = $mod_perl::VERSION < 1.24 ? $c->dir_config($p) : $c->dir_config->get($p);

    die "Only a single value is allowed for configuration parameter '$p'\n"
	if @val > 1 && ! $wantarray;

    die "Configuration parameter '$p' is required\n"
	if $required && ! defined $val[0];

    return ($p, $wantarray ? @val : $val[0]);
}

sub new
{
    my $class = shift;
    my $self = {
	request_number => 0,
	%fields,
    };

    validate( @_,
	      { apache_status_title => { type => SCALAR, optional => 1 },
		auto_send_headers => { type => SCALAR | UNDEF, optional => 1 },
		decline_dirs => { type => SCALAR | UNDEF, optional => 1 },
		error_mode => { type => SCALAR, optional => 1 },
		output_mode => { type => SCALAR | UNDEF, optional => 1 },
		top_level_predicate => { type => CODEREF | UNDEF, optional => 1 },
		debug_mode => { type => SCALAR, optional => 1 },
		debug_perl_binary => { type => SCALAR, optional => 1 },
		debug_handler_script => { type => SCALAR, optional => 1 },
		debug_handler_proc => { type => SCALAR, optional => 1 },
		debug_dir_config_keys => { type => ARRAYREF, optional => 1 },

		# the only required param
		interp => { isa => 'HTML::Mason::Interp' },
	      }
	    );

    my (%options) = @_;

    die "error_mode parameter must be one of 'html', 'fatal', 'raw_html', or 'raw_fatal'\n"
	if exists $options{error_mode} && $options{error_mode} !~ /^(?:html|fatal|raw_html|raw_fatal)$/;

    while (my ($key,$value) = each(%options)) {
	$self->{$key} = $value;
    }

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
		      qq(<FONT size="+2"><B>) . $self->apache_status_title . qq(</B></FONT><BR><BR>),
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
            } grep ! ref $interp->{$_}, sort keys %$interp;

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

    if (lc($apreq->dir_config('Filter')) eq 'on') {
	$apreq = $apreq->filter_register;
    }

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
    my $debug_state;
    $debug_state = $self->capture_debug_state($apreq)
	if ($debugMode eq 'all' or $debugMode eq 'error');

    #
    # Create an Apache-specific request with additional slots.
    #
    my $request = new HTML::Mason::Request::ApacheHandler
	(ah=>$self,
	 interp=>$interp,
	 apache_req=>$apreq,
	 );
    
    eval { $retval = $self->handle_request_1($apreq, $request, $debug_state) };
    my $err = $@;
    my $err_code = $request->error_code;
    my $err_status = $err ? 1 : 0;

    if ($err) {
	#
	# If first component was not found, return NOT_FOUND. In case
	# of POST we must trick Apache into not reading POST content
	# again. Wish there were a more standardized way to do this...
	#
	# This $err_code stuff is really only used to communicate found
	# errors; it will be replaced with exceptions
	#
	if (defined($err_code) and $err_code eq 'top_not_found') {
	    if ($apreq->method eq 'POST') {
		$apreq->method('GET');
		$apreq->headers_in->unset('Content-length');
	    }
	    
	    # Log the error the same way that Apache does (taken from default_handler in http_core.c)
	    $apreq->log_error("[Mason] File does not exist: ",$apreq->filename . ($apreq->path_info ? $apreq->path_info : ""));
	    return NOT_FOUND;
	}

	#
	# Do not process error at all in raw mode or if die handler was overriden.
	#
	unless ($self->error_mode =~ /^raw_/ or $interp->die_handler_overridden) {
	    $err = error_process ($err, $request);
	}

	#
	# In fatal/raw_fatal mode, compress error to one line (for Apache logs) and die.
	# In html/raw_html mode, call error_display_html and print result.
	# The raw_ modes correspond to pre-1.02 error formats.
	#
	# [This is a load of spaghetti. It will be cleaned up in 1.2 when we lose
	# debug mode and standardize error handling.]
	#
	if ($self->error_mode eq 'fatal') {
	    unless ($interp->die_handler_overridden) {
		$err =~ s/\n/\t/g;
		$err =~ s/\t$//g;
		$err .= "\n" if $err !~ /\n$/;
	    }
	    die $err;
	} elsif ($self->error_mode eq 'raw_fatal') {
	    die ("System error:\n$err\n");	    
	} elsif ($self->error_mode =~ /html$/) {
	    unless ($interp->die_handler_overridden) {
		my $debug_msg;
		if ($debugMode eq 'error' or $debugMode eq 'all') {
		    $debug_msg = $self->write_debug_file($apreq,$debug_state);
		}
		if ($self->error_mode =~ /^raw_/) {
		    $err .= "$debug_msg\n" if $debug_msg;
		    $err = "<h3>System error</h3><p><pre><font size=-1>$err</font></pre>\n";
		} else {
		    $err .= "Debug info: $debug_msg\n" if $debug_msg;
		    $err = error_display_html($err);
		}
	    }
	    # Send HTTP headers if they have not been sent.
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print($err);
	}
    } else {
	if ($debugMode eq 'all') {
	    my $debug_msg = $self->write_debug_file($apreq,$debug_state);
	    print "\n<!--\n$debug_msg\n-->\n" if (http_header_sent($apreq) && !$apreq->header_only && $apreq->header_out("Content-type") =~ /text\/html/);
	}
    }
    undef $request;  # ward off memory leak

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
    my $out_path = "$outDir/$outFile";
    my $outfh = make_fh();
    unless ( open $outfh, ">$out_path" ) {
	$r->warn("cannot open debug file '$out_path' for writing");
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
    use File::Copy;
    use Getopt::Std;
    getopt('r');   # r=repeat count, p=user profile req, P=re-entrant profile call
    $opt_r ||= 1;
    
    if ($opt_p) {
	print STDERR "Profiling request ...";
	# re-enter with different option (no inf. loops, please)
	system ("perl", "-d:DProf", $0, "-P", "-r$opt_r")
            or die "Can't execute perl: $!";
    
# -----------------------------
# When done, merge named coderefs in tmon.mason with those in tmon.out,
# then run dprofpp
# -----------------------------
        my $fh = do { local *FH; *FH; };  # double *FH avoids warning
        open $fh, '< ./tmon.mason' or die "Missing file: tmon.mason: $!";
	foreach (<$fh>) { chomp;  my ($k,$v) = split(/\t/);  $::CODEREF_NAME{$k} = $v; }
	close $fh or die "can't close file: tmon.mason: $!";
    
	my $tmonout = do { local *FH; *FH; };
        open $tmonout, '< ./tmon.out' or die "Missing file: tmon.out: $!";
	my $tmontmp = do { local *FH; *FH; };
        open $tmontmp, '> ./tmon.tmp' or die "Couldn't write file: tmon.tmp: $!";
	my $regex = quotemeta(join('|', keys %::CODEREF_NAME));
	$regex =~ s/\\\|/|/g;   #un-quote the pipe chars
	while (<$tmonout>) {
	    s/HTML::Mason::Commands::($regex)/$::CODEREF_NAME{$1}/;
	    print $tmontmp $_;
	}
	close $tmonout or die "can't close file: tmon.out: $!";
        close $tmontmp or die "can't close file: tmon.tmp: $!";
	copy('tmon.tmp' => 'tmon.out') or die "$!";
	unlink('tmon.tmp') or warn "can't remove file: tmon.tmp: $!";
        print STDERR "\nRunning dprofpp ...\n";
	exec('dprofpp') or die "Couldn't execute dprofpp";
    }
}

PERL
    $o .= "BEGIN { \$HTML::Mason::IN_DEBUG_FILE = 1; require '".$self->debug_handler_script."' }\n\n";
    $o .= <<'PERL';
if ($opt_P) {
    open SAVEOUT, ">&STDOUT" or die "Can't open &STDOUT: $!";    # stifle component output while profiling
    open STDOUT, ">/dev/null" or die "Can't write to /dev/null: $!";
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
    my $fh = do { local *FH; *FH; };
    open $fh, '>./tmon.mason' or die "Couldn't write to file: tmon.mason: $!";
    print $fh map("$_\t$HTML::Mason::CODEREF_NAME{$_}\n", keys %HTML::Mason::CODEREF_NAME);
    close $fh or die "Can't close file: tmon.mason: $!";
}
PERL
    print $outfh $o;
    close $outfh or die "can't close file: $out_path: $!";
    chmod(0775,$out_path) or die "can't chmod file to 0775: $out_path: $!";

    my $debug_msg = "Debug file is '$out_path'.";
    return $debug_msg;
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
    my ($self,$r,$request,$debug_state) = @_;
    my $interp = $self->interp;

    #
    # If filename is a directory, then either decline or simply reset
    # the content type, depending on the value of decline_dirs.
    #
    # ** We should be able to use $r->finfo here, but finfo is broken
    # in some versions of mod_perl (e.g. see Shane Adams message on
    # mod_perl list on 9/10/00)
    #
    my $is_dir = -d $r->filename;
    my $is_file = -f _;

    if ($is_dir) {
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
    $pathname .= $r->path_info unless $is_file;

    #
    # Compute the component path via the resolver. Return NOT_FOUND on failure.
    #
    my $comp_path = $interp->resolver->file_to_path($pathname,$interp);
    unless ($comp_path) {
	$r->warn("[Mason] Cannot resolve file to component: $pathname (is file outside component root?)");
	return NOT_FOUND;
    }

    #
    # Return NOT_FOUND if file does not pass top level predicate.
    #
    if ($is_file and defined($self->top_level_predicate) and !$self->top_level_predicate->($r->filename)) {
	$r->warn("[Mason] File fails top level predicate: ".$r->filename);
	return NOT_FOUND;
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
	%args = $self->$ARGS_METHOD(\$r,$request);
    }
    $debug_state->{args_hash} = \%args if $debug_state;

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
    my ($self, $rref, $request) = @_;

    my $r = $$rref;

    # For optimization, don't bother creating a CGI object if request
    # is a GET with no query string
    return if $r->method eq 'GET' && !scalar($r->args);

    my $q = CGI->new;
    $request->cgi_object($q);

    my %args;
    my $methods = $r->method eq 'GET' ? [ 'param' ] : [ 'param', 'url_param' ];
    foreach my $method (@$methods) {
	foreach my $key ( $q->$method() ) {
	    foreach my $value ( $q->$method($key) ) {
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
    }

    return %args;
}

#
# Get %args hash via Apache::Request package. As a side effect, assign the
# new Apache::Request package back to $r, unless $r is already an Apache::Request.
#
sub _mod_perl_args
{
    my ($self, $rref, $request) = @_;

    my $apr = $$rref;
    unless (UNIVERSAL::isa($apr, 'Apache::Request')) {
	$apr = Apache::Request->new($apr);
	$$rref = $apr;
    }
    
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

#
# PerlHandler HTML::Mason::ApacheHandler
#
sub handler
{
    my $r = shift;

    my $ah = _make_ah;
    __PACKAGE__->import unless $ARGS_METHOD;

    return $ah->handle_request($r);
}


#----------------------------------------------------------------------
#
# APACHEHANDLER MASON COMMANDS
#
package HTML::Mason::Commands;
use vars qw($m);
# no longer needed
sub mc_suppress_http_header {}

1;
