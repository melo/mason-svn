# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;

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
    my $interp = $options{interp} or
	HTML::Mason::Exception::Params->throw( error => "HTML::Mason::Request::ApacheHandler->new: must specify interp\n" );
    delete $options{interp};
    my $self = $class->SUPER::new(interp=>$interp);
    while (my ($key,$value) = each(%options)) {
	if (exists($reqfields{$key})) {
	    $self->{$key} = $value;
	} else {
	    HTML::Mason::Exception::Params->throw( error => "HTML::Mason::Request::ApacheHandler->new: invalid option '$key'\n" );
	}
    }
    return $self;
}

# Override flush_buffer to also call $r->rflush
sub flush_buffer
{
    my ($self) = @_;
    $self->SUPER::flush_buffer;
    $self->apache_req->rflush;
}

sub cgi_object
{
    my ($self) = @_;

    HTML::Mason::Exception->throw( error => "Can't call cgi_object method unless CGI.pm was used to handle incoming arguments.\n" )
	unless defined $CGI::VERSION;

    if (defined($_[1])) {
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
use File::Path;
use File::Spec;
use HTML::Mason::Exceptions;
use HTML::Mason::Interp;
use HTML::Mason::Error qw(error_process error_display_html);
use HTML::Mason::Utils;
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

use Apache;
use Apache::Status;
die "mod_perl must be compiled with PERL_METHOD_HANDLERS=1 (or EVERYTHING=1) to use ", __PACKAGE__, "\n"
    unless Apache::perl_hook('MethodHandlers');

# Require a reasonably modern mod_perl - should probably be later
use mod_perl 1.22;

use HTML::Mason::MethodMaker
    ( read_write => [ qw( apache_status_title
                          args_method
			  auto_send_headers
			  decline_dirs
			  error_mode
			  interp
			  output_mode
			  top_level_predicate ) ]
    );

use vars qw($VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

__PACKAGE__->valid_params
    (
     apache_status_title   => { parse => 'string',  type => SCALAR,       default => 'HTML::Mason status' },
     args_method           => { parse => 'string',  type => SCALAR,       default => 'mod_perl',
			        callbacks =>
				{ "must be either 'CGI' or 'mod_perl'" =>
				  sub { $_[0] =~ /^(?:CGI|mod_perl)$/ } }
                              },
     auto_send_headers     => { parse => 'boolean', type => SCALAR|UNDEF, default => 1 },

     debug_dir_config_keys => { parse => 'list',    type => ARRAYREF,     optional => 1 },
     debug_handler_proc    => { parse => 'string',  type => SCALAR,       optional => 1 },
     debug_handler_script  => { parse => 'string',  type => SCALAR,       optional => 1 },
     debug_mode            => { parse => 'string',  type => SCALAR,       optional => 1 },
     debug_perl_binary     => { parse => 'string',  type => SCALAR,       optional => 1 },
     decline_dirs          => { parse => 'boolean', type => SCALAR|UNDEF, default => 1 },
     error_mode            => { parse => 'string',  type => SCALAR,       default => 'html',
				callbacks =>
				{ "must be one of 'html', 'fatal', 'raw_html', or 'raw_fatal'" =>
				  sub { $_[0] =~ /^(?:raw)?(?:html|fatal)$/ } }
			      },
     multiple_config       => { parse => 'boolean', type => SCALAR|UNDEF, optional => 1 },
     output_mode           => { parse => 'string',  type => SCALAR,       default => 'batch' },
     interp_class          => { parse => 'string',  type => SCALAR,       default => 'HTML::Mason::Interp' },
     top_level_predicate   => { parse => 'code',    type => CODEREF,      default => sub () {1} },

     # the only required param
     interp                => { isa => 'HTML::Mason::Interp' },
    );

sub import
{
    my $pack = shift;

    if ( my $args_method = $pack->get_param('ArgsMethod') )
    {
	if ($args_method eq 'CGI')
	{
	    unless (defined $CGI::VERSION)
	    {
		require CGI;
	    }
	}
	elsif ($args_method eq 'mod_perl')
	{
	    unless (defined $Apache::Request::VERSION)
	    {
		require Apache::Request;
	    }
	}

	$pack->make_ah() if $pack->_in_simple_conf_file;
    }
}

#
# This is my best guess as to whether we are being configured via the
# conf file without multiple configs.  Without a comp root it will
# blow up sooner or later anyway.  This may not be the case in the
# future though.
#
sub _in_simple_conf_file
{
    my $self = shift;

    return $ENV{MOD_PERL} && $self->_get_string_param('MasonCompRoot');
}

sub make_ah
{
    my $package = shift;

    my $comp_root = join '', $package->_get_list_param('MasonCompRoot');

    use vars qw($AH);
    return $AH if $AH && $AH->{last_comp_root} eq $comp_root;

    my %classes = $package->_get_class_params;

    my %p = $package->get_config( $package->allowed_params(%classes) );

    eval "use $p{interp_class}";
    die $@ if $@;

    $AH = $package->new( interp => $package->_make_interp($p{interp_class}, %classes),
			 %p,
		       );

    $AH->{last_comp_root} = $comp_root;

    return $AH;
}

sub _make_interp
{
    my ($self, $interp_class, %classes) = @_;
    my %p = $self->get_config( $interp_class->allowed_params(%classes) );

    # comp_root is sort of polymorphic, so fix it up
    if (exists $p{comp_root}) {
	if (@{$p{comp_root}} == 1 && $p{comp_root}->[0] !~ /=>/) {
	    $p{comp_root} = $p{comp_root}[0];  # Convert to a simple string
	} else {
	    foreach my $root (@{$p{comp_root}}) {
		$root = [ split /\s*=>\s*/, $root, 2 ];
		HTML::Mason::Exception::Params->throw
		    ( error => "Configuration parameter MasonCompRoot must be either a single string value ".
		      "or multiple key/value pairs like 'foo => /home/mason/foo'" )
			unless defined $root->[1];
	    }
	}
    }

    my $interp = $interp_class->new(%p);

    # If we're running as superuser, change file ownership to http user & group
    if ($interp->files_written && ! ($> || $<))
    {
	chown Apache->server->uid, Apache->server->gid, $interp->files_written
	    or HTML::Mason::Exception::System->throw( error => "Can't change ownership of files written by interp object\n" );
    }

    return $interp;
}

# The following routines handle getting information from $r->dir_config

sub calm_form {
    # Transform from StudlyCaps to name_like_this
    my ($self, $string) = @_;
    $string =~ s/^Mason//;
    $string =~ s/(^|.)([A-Z])/$1 ? "$1\L_$2" : "\L$2"/ge;
    return $string;
}

sub studly_form {
    # Transform from name_like_this to StudlyCaps
    my ($self, $string) = @_;
    $string =~ s/(?:^|_)(\w)/\U$1/g;
    return $string;
}

sub get_param {
    # Gets a single config item from dir_config.

    my ($self, $key, $spec) = @_;

    # If we weren't given a spec, try to locate one in our own class.
    $spec ||= $self->allowed_params->{$self->calm_form($key)};
    HTML::Mason::Exception->throw( error => "Unknown config item '$key'" )
        unless $spec;

    return unless $spec->{parse};
    my $method = "_get_$spec->{parse}_param";
    my $value = $self->$method('Mason'.$self->studly_form($key));

    if (!defined($value) and exists($spec->{default})) {
	$value = $spec->{default};
    }
    return $value;
}

sub get_config {
    # Gets a bunch of config items (specified in the $valid hash) from
    # current Apache dir_config variables.  Will assign defaults if
    # appropriate.  Will *not* check for required params - that
    # happens later, when the params are fed into methods.

    my ($self, $valid) = @_;

    my %config;
    while (my ($key, $spec) = each %$valid) {
	# For dir_config params, value is undef if unmentioned, so
	# don't store undefs.  The TableAPI might be a way around this.
	my $value = $self->get_param($key, $spec);
	next if !defined $value;
	next if $spec->{parse} eq 'list' and !@$value;

	$config{$key} = $value;
    }
    return %config;
}

sub _get_class_params
{
    my $self = shift;

    my $c = Apache->request ? Apache->request : Apache->server;

    my $config = $c->dir_config;

    # Get all params ending with 'Class'
    return map { $_ =~ /Class$/ ? ( $self->calm_form($_) => $config->{$_} ) : () } keys %$config;
}

sub _get_string_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return $val;
}

sub _get_boolean_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return $val;
}

sub _get_code_param
{
    my $self = shift;
    my ($p, $val) = $self->_get_val(@_);

    return unless $val;

    my $sub_ref = eval $val;

    HTML::Mason::Exception::Params->throw( error => "Configuration parameter '$p' is not valid perl:\n$@\n" )
	if $@;

    return $sub_ref;
}

sub _get_list_param
{
    my $self = shift;
    my ($p, @val) = $self->_get_val($_[0], 1);
    if (@val == 1 && ! defined $val[0])
    {
	@val = ();
    }

    return \@val;
}

sub _get_val
{
    my ($self, $p, $wantarray) = @_;

    my $c = Apache->request ? Apache->request : Apache->server;

    my @val = Apache::perl_hook('TableApi') ? $c->dir_config->get($p) : $c->dir_config($p);

    HTML::Mason::Exception::Params->throw( error => "Only a single value is allowed for configuration parameter '$p'\n" )
	if @val > 1 && ! $wantarray;

    return ($p, $wantarray ? @val : $val[0]);
}

sub new
{
    my $class = shift;

    my $self = bless {validate( @_, $class->validation_spec )}, $class;

    $self->_initialize;
    return $self;
}

# Register with Apache::Status at module startup.  Will get replaced
# with a more informative status once an interpreter has been created.

my $status_name = 'mason0001';

{
    local $^W; # to avoid subroutine redefined warnings - but this doesn't work!
    Apache::Status->menu_item
	    ($status_name => __PACKAGE__->allowed_params->{apache_status_title}{default},
	     sub { ["<b>(no interpreters created in this child yet)</b>"] });
}


sub _initialize {
    my ($self) = @_;

    $self->{request_number} = 0;

    if ($self->args_method eq 'mod_perl') {
	unless (defined $Apache::Request::VERSION) {
	    warn "Loading Apache::Request at runtime.  You could increase shared memory between Apache processes by preloading it in your httpd.conf or handler.pl file\n";
	    require Apache::Request;
	}
    } else {
	unless (defined $CGI::VERSION) {
	    warn "Loading CGI at runtime.  You could increase shared memory between Apache processes by preloading it in your httpd.conf or handler.pl file\n";

	    require CGI;
	}
    }

    # Add an HTML::Mason menu item to the /perl-status page.
    if (defined $Apache::Status::VERSION) {
	# A closure, carries a reference to $self
	my $statsub = sub {
	    my ($r,$q) = @_; # request and CGI objects
	    return [] if !defined($r);

	    if ($r->path_info and $r->path_info =~ /expire_code_cache=(.*)/) {
		$self->interp->delete_from_code_cache($1);
	    }

	    return ["<center><h2>" . $self->apache_status_title . "</h2></center>" ,
		    $self->status_as_html,
		    $self->interp->status_as_html];
	};
	Apache::Status->menu_item($status_name++, $self->apache_status_title, $statsub);
    }

    my $interp = $self->interp;

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(preview)) {
	my @newdirs = mkpath($interp->data_dir."/$subdir",0,0775);
	$interp->push_files_written(@newdirs);
    }

    #
    # Allow global $r in components
    #
    $interp->compiler->add_allowed_globals('$r');
}

# Generate HTML that describes ApacheHandler's current status.
# This is used in things like Apache::Status reports.

sub status_as_html {
    my ($self) = @_;

    # Should I be scared about this?  =)

    my $comp_text = <<'EOF';
<h3>ApacheHandler properties:</h3>
<blockquote>
 <tt>
<%perl>
foreach my $property (sort keys %$ah) {
    my $val = $ah->{$property};
    # only object can ->can, others die
    eval { $val->can('anything') };
    if (ref $val ) {
        $val = '<font color="darkred">' . (ref $val);
        $val .= $@ ? ' reference' : ' object';
        $val .= '</font>';
    }

    $val =~ s,([\x00-\x1F]),'<font color="purple">control-' . chr( ord('A') + ord($1) - 1 ) . '</font>',eg; # does this work for non-ASCII?
</%perl>
    <% $property |h %> => <% defined $val ? $val : '<i>undef</i>' %>
                          <% $val eq $valid{$property}{default} ? '<font color=green>(default)</font>' : '' %>
		          <br>
% }
  </tt>
</blockquote>

<%args>
 $ah       # The ApacheHandler we'll elucidate
 %valid    # Contains default values for member data
</%args>
EOF

    my $interp = $self->interp;
    my $comp = $interp->make_anonymous_component(comp => $comp_text);
    my $out;
    local $interp->{out_method} = \$out;
    $interp->exec($comp, ah => $self, valid => $interp->allowed_params);
    return $out;
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
    # If someone is using a custom request class that doesn't accept
    # 'ah' and 'apache_req' that's their problem.
    #
    my $request = $self->interp->make_request( ah => $self,
					       interp => $interp,
					       apache_req => $apreq,
					     );
    eval { $retval = $self->handle_request_1($apreq, $request) };
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
	my $raw_err = $err;
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
	    die ("System error:\n$raw_err\n");
	} elsif ($self->error_mode =~ /html$/) {
	    unless ($interp->die_handler_overridden) {
		my $debug_msg;
		if ($self->error_mode =~ /^raw_/) {
		    $err .= "$debug_msg\n" if $debug_msg;
		    $err = "<h3>System error</h3><p><pre><font size=-1>$raw_err</font></pre>\n";
		} else {
		    $err .= "Debug info: $debug_msg\n" if $debug_msg;
		    $err = error_display_html($err,$raw_err);
		}
	    }
	    # Send HTTP headers if they have not been sent.
	    if (!http_header_sent($apreq)) {
		$apreq->content_type('text/html');
		$apreq->send_http_header();
	    }
	    print($err);
	}
    }
    undef $request;  # ward off memory leak

    $interp->write_system_log('REQ_END', $self->{request_number}, $err_status);
    return ($err) ? &OK : (defined($retval)) ? $retval : &OK;
}

#
# Shorthand for various data subdirectories and files.
#
sub preview_dir { return shift->interp->data_dir . "/preview" }

sub handle_request_1
{
    my ($self,$r,$request) = @_;

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
    my $comp_path = $interp->resolver->file_to_path($pathname);
    unless ($comp_path) {
	$r->warn("[Mason] Cannot resolve file to component: $pathname (is file outside component root?)");
	return NOT_FOUND;
    }

    #
    # Return NOT_FOUND if file does not pass top level predicate.
    #
    if ($is_file and !$self->top_level_predicate->($r->filename)) {
	$r->warn("[Mason] File fails top level predicate: ".$r->filename);
	return NOT_FOUND;
    }

    my %args;
    if ($self->args_method eq 'mod_perl') {
	$r = Apache::Request->new($r);
	%args = $self->_mod_perl_args($r, $request);
    } else {
	%args = $self->_cgi_args($r, $request);
    }

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

    $interp->out_method( sub { $r->print( grep {defined} @_ ) } );

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
    my ($self, $r, $request) = @_;

    # For optimization, don't bother creating a CGI object if request
    # is a GET with no query string
    return if $r->method eq 'GET' && !scalar($r->args);

    my $q = CGI->new;
    $request->cgi_object($q);

    return HTML::Mason::Utils::cgi_request_args($q, $r->method);
}

#
# Get %args hash via Apache::Request package.
#
sub _mod_perl_args
{
    my ($self, $apr, $request) = @_;

    my %args;
    foreach my $key ( $apr->param ) {
	my @values = $apr->param($key);
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return %args;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent { shift->header_out("Content-type") }

#
# PerlHandler HTML::Mason::ApacheHandler
#
sub handler ($$)
{
    my ($package, $r) = @_;

    my $ah = $package->make_ah();

    return $ah->handle_request($r);
}

1;

__END__

=head1 NAME

HTML::Mason::ApacheHandler - Mason/mod_perl interface

=head1 SYNOPSIS

    use HTML::Mason::ApacheHandler;

    my $ah = new HTML::Mason::ApacheHandler (..name/value params..);
    ...
    sub handler {
        my $r = shift;
        $ah->handle_request($r);
    }

=head1 DESCRIPTION

The ApacheHandler object links Mason to mod_perl, running components in
response to HTTP requests. It is controlled primarily through
parameters to the new() constructor.

handle_request() is not a user method, but rather is called from the
HTML::Mason::handler() routine in handler.pl.


=head1 PARAMETERS TO THE new() CONTRUCTOR

=over

=item apache_status_title

Title that you want this ApacheHandler to appear as under
Apache::Status.  Default is "HTML::Mason status".  This is useful if
you create more than one ApacheHandler object and want them all
visible via Apache::Status.

=item args_method

Method to use for unpacking GET and POST arguments. The valid options
are 'CGI' and 'mod_perl'; these indicate that a CGI.pm or
Apache::Request object (respectively) will be created for the purposes
of argument handling.

Apache::Request is the default and requires that you have installed
this package.

When specifying args_method='CGI', the Mason request object ($m)
will have a method called C<cgi_object> available.  This method
returns the CGI object used in the ApacheHandler code.

When specifying args_method='mod_perl', the $r global is upgraded
to an Apache::Request object. This object inherits all Apache
methods and adds a few of its own, dealing with parameters and
file uploads.  See L<Apache::Request> for more information.

While Mason will load Apache::Request or CGI as needed at runtime, it
is recommended that you preload the relevant module either in your
httpd.conf or handler.pl file as this will save some memory.

=item auto_send_headers

True or undef; default true.  Indicates whether Mason should
automatically send HTTP headers before sending content back to the
client. If you set to false, you should call $r->send_http_header
manually.

See the L<Devel/sending_http_headers> of the Component Developer's
Guide for details about the automatic header feature.

=item decline_dirs

Indicates whether Mason should decline directory requests, leaving
Apache to serve up a directory index or a FORBIDDEN error as
appropriate. Default is 1. See L<Admin/Allowing directory requests>
for more information about handling directories with Mason.

=item error_mode

Specifies how to handle Perl errors. Options are 'html', 'fatal',
'raw_html', and 'raw_fatal'; the default is 'html'.

In html mode the handler sends a readable HTML version of the error
message to the client. This mode is most useful on a development
server.

In fatal mode the handler simply dies with a compact version of the
error message. This may be caught with an eval around
C<$ah-E<gt>handle_request> or left for Apache to handle. In the latter
case the error will end up in the error logs. This mode is most
useful on a production server.

The raw_html and raw_fatal modes emulate pre-1.02 error behavior. They
are analagous to the modes above except that the errors are not
processed for readability or compactness. The resulting messages are
much longer, but may include information accidentally omitted by
Mason's processing.

Regardless of this setting, no readability processing occurs if you
have overriden the L<Interp/die_handler> Interp parameter.

=item interp

The only required parameter.  Specifies a Mason interpreter to use for
handling requests.  The interpreter should be an instance of the
C<HTML::Mason::Interp> class, or a subclass thereof.

=item output_mode

This parameter has been replaced by the equivalent Interp parameter
L<Interp/out_mode>. For backward compatibility, setting
C<$ah-E<gt>output_mode> will cause C<$interp-E<gt>out_mode> to be set
appropriately.

=item top_level_predicate

Reference to a subroutine that decides whether a component can answer
top level requests. This allows for private-use components that live
within the DocumentRoot but are inaccesible from URLs. By default,
always returns 1.

The subroutine receives one parameter, the absolute path to the
component.  It then returns either a true (serve component) or false
(reject component). In this example, the predicate rejects requests
for components whose name starts with an "_" character:

    top_level_predicate => sub { $_[0] !~ m{/_[^/]+$}

=back

=head1 ACCESSOR METHODS

All of the above properties have standard accessor methods of the
same name: no arguments retrieves the value, and one argument sets it.
For example:

    my $ah = new HTML::Mason::ApacheHandler;
    my $errmode = $ah->error_mode;
    $ah->error_mode('html');

=head1 AUTHOR

Jonathan Swartz, swartz@pobox.com

=head1 SEE ALSO

L<HTML::Mason>,
L<HTML::Mason::Parser>,
L<HTML::Mason::Interp>,
L<HTML::Mason::Admin>

=cut
