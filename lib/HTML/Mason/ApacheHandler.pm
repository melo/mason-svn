# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use strict;

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC REQUEST OBJECT
#
package HTML::Mason::Request::ApacheHandler;

use Apache::Constants qw( REDIRECT );

use HTML::Mason::Request;
use Class::Container;
use Params::Validate qw(BOOLEAN);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

use base qw(HTML::Mason::Request);

use HTML::Mason::Exceptions( abbr => [qw(param_error error)] );

use constant OK         => 0;
use constant DECLINED   => -1;
use constant NOT_FOUND  => 404;

BEGIN
{
    my $ap_req_class = $mod_perl::VERSION < 1.99 ? 'Apache' : 'Apache::RequestRec';

    __PACKAGE__->valid_params
	( ah         => { isa => 'HTML::Mason::ApacheHandler',
			  descr => 'An ApacheHandler to handle web requests',
			  public => 0 },

	  apache_req => { isa => $ap_req_class, default => undef,
			  descr => "An Apache request object",
			  public => 0 },

	  cgi_object => { isa => 'CGI',    default => undef,
			  descr => "A CGI.pm request object",
			  public => 0 },

	  auto_send_headers => { parse => 'boolean', type => BOOLEAN, default => 1,
				 descr => "Whether HTTP headers should be auto-generated" },
	);
}

use HTML::Mason::MethodMaker
    ( read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		      qw( ah apache_req auto_send_headers ) ] );

# A hack for subrequests
sub _properties { qw(ah apache_req), shift->SUPER::_properties }

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);  # Magic!

    unless ($self->apache_req or $self->cgi_object)
    {
	param_error __PACKAGE__ . "->new: must specify 'apache_req' or 'cgi_object' parameter";
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

    error "Can't call cgi_object() unless 'args_method' is set to CGI.\n"
	unless $self->ah->args_method eq 'CGI';

    if (defined($_[1])) {
	$self->{cgi_object} = $_[1];
    } else {
	# We may not have created a CGI object if, say, request was a
	# GET with no query string. Create one on the fly if necessary.
	$self->{cgi_object} ||= CGI->new('');
    }

    return $self->{cgi_object};
}

#
# Override this method to return NOT_FOUND when we get a
# TopLevelNotFound exception. In case of POST we must trick
# Apache into not reading POST content again. Wish there were
# a more standardized way to do this...
#
sub exec
{
    my $self = shift;
    my $r = $self->apache_req;
    my $retval;

    if ( $self->is_subrequest )
    {
        # no need to go through all the rigamorale below for
        # subrequests, and it may even break things to do so, since
        # $r's print should only be redefined once.
	eval { $retval = $self->SUPER::exec(@_) };
    }
    else
    {
        local $^W = 0;
        # ack, this has to be done at runtime to account for the fact
        # that Apache::Filter change's $r's class and implements its
        # own print() method.
        my $real_apache_print = $r->can('print');

	# Remap $r->print to Mason's $m->print while executing
	# request, but just for this $r, in case user does an internal
	# redirect or apache subrequest.
	no strict 'refs';

        my $req_class = ref $r;
	local *{"$req_class\::print"} = sub {
	    my $local_r = shift;
	    if ($local_r eq $r) {
		$self->print(@_);
	    } else {
		$local_r->$real_apache_print(@_);
	    }
	};
	eval { $retval = $self->SUPER::exec(@_) };
    }

    if ($@) {
	if (isa_mason_exception($@, 'TopLevelNotFound')) {
	    # Log the error the same way that Apache does (taken from default_handler in http_core.c)
	    $r->log_error("[Mason] File does not exist: ", $r->filename . ($r->path_info ? $r->path_info : ""));
	    return $self->ah->return_not_found($r);
	} else {
	    die $@;
	}
    }

    # On a success code, send headers if they have not been sent and
    # if we are the top-level request. Since the out_method sends
    # headers, this will typically only apply after $m->abort.
    # On an error code, leave it to Apache to send the headers.
    if (!$self->is_subrequest
	and $self->auto_send_headers
	and !HTML::Mason::ApacheHandler::http_header_sent($r)
	and (!$retval or $retval==200)) {
	$r->send_http_header();
    }

    return defined($retval) ? $retval : OK;
}

#
# Override this method to always die when top level component is not found,
# so we can return NOT_FOUND.
#
sub _handle_error
{
    my ($self, $err) = @_;

    if (isa_mason_exception($err, 'TopLevelNotFound')) {
	die $err;
    } else {
        if ( $self->error_format eq 'html' ) {
            $self->apache_req->content_type('text/html');
        }
	$self->SUPER::_handle_error($err);
    }
}

sub redirect
{
    my ($self, $url, $status) = @_;
    my $r = $self->apache_req;

    $self->clear_buffer;
    $r->method('GET');
    $r->headers_in->unset('Content-length');
    $r->err_header_out( Location => $url );
    $self->abort($status || REDIRECT);
}

#----------------------------------------------------------------------
#
# APACHE-SPECIFIC FILE RESOLVER OBJECT
#
package HTML::Mason::Resolver::File::ApacheHandler;

use strict;

use HTML::Mason::Tools qw(paths_eq);

use HTML::Mason::Resolver::File;
use base qw(HTML::Mason::Resolver::File);

#
# Given an apache request object, return the associated component
# path or undef if none exists. This is called for top-level web
# requests that resolve to a particular file.
#
sub apache_request_to_comp_path {
    my ($self, $r) = @_;

    my $file = $r->filename;
    $file .= $r->path_info unless -f $file;

    # Clear up any weirdness here so that paths_eq compares two
    # 'canonical' paths (canonpath is called on comp roots when
    # resolver object is created.  Seems to be needed on Win32 (see
    # bug #356).
    $file = File::Spec->canonpath($file);

    foreach my $root (map $_->[1], $self->comp_root_array) {
	if (paths_eq($root, substr($file, 0, length($root)))) {
	    my $path = substr($file, length $root);
            $path = length $path ? join '/', File::Spec->splitdir($path) : '/';
            chop $path if $path ne '/' && substr($path, -1) eq '/';

            return $path;
	}
    }
    return undef;
}


#----------------------------------------------------------------------
#
# APACHEHANDLER OBJECT
#
package HTML::Mason::ApacheHandler;

use File::Path;
use File::Spec;
use HTML::Mason::Exceptions( abbr => [qw(param_error system_error error)] );
use HTML::Mason::Interp;
use HTML::Mason::Tools qw( load_pkg );
use HTML::Mason::Utils;
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

use Apache;
use Apache::Constants qw( OK DECLINED NOT_FOUND );

# Require a reasonably modern mod_perl - should probably be later
use mod_perl 1.22;

if ( $mod_perl::VERSION < 1.99 )
{
    die "mod_perl must be compiled with PERL_METHOD_HANDLERS=1 (or EVERYTHING=1) to use ", __PACKAGE__, "\n"
	unless Apache::perl_hook('MethodHandlers');
}

use vars qw($VERSION);

$VERSION = 1.69;

use Class::Container;
use base qw(Class::Container);

BEGIN
{
    __PACKAGE__->valid_params
	(
	 apache_status_title =>
         { parse => 'string', type => SCALAR, default => 'HTML::Mason status',
           descr => "The title of the Apache::Status page" },

	 args_method =>
         { parse => 'string',  type => SCALAR,       default => 'mod_perl',
           callbacks =>
           { "must be either 'CGI' or 'mod_perl'" =>
             sub { $_[0] =~ /^(?:CGI|mod_perl)$/ } },
           descr => "Whether to use CGI.pm or Apache::Request for parsing the incoming HTTP request",
         },

	 decline_dirs =>
         { parse => 'boolean', type => BOOLEAN, default => 1,
           descr => "Whether Mason should decline to handle requests for directories" },

	 # the only required param
	 interp =>
         { isa => 'HTML::Mason::Interp',
           descr => "A Mason interpreter for processing components" },
	);

    __PACKAGE__->contained_objects
	(
	 interp =>
         { class => 'HTML::Mason::Interp',
           descr => 'The interp class coordinates multiple objects to handle request execution'
         },
	);
}

use HTML::Mason::MethodMaker
    ( read_only  => [ 'args_method' ],
      read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		      qw( apache_status_title
			  decline_dirs
			  interp ) ]
    );

use vars qw($AH $STARTED);

# hack to let the make_params_pod.pl script work
__PACKAGE__->_startup() if Apache->server;
sub _startup
{
    my $pack = shift;
    return if $STARTED++; # Allows a subclass to call us, without running twice

    if ( my $args_method = $pack->get_param('ArgsMethod') )
    {
	if ($args_method eq 'CGI')
	{
	    require CGI unless defined $CGI::VERSION;
	}
	elsif ($args_method eq 'mod_perl')
	{
	    require Apache::Request unless defined $Apache::Request::VERSION;
	}

	# if we are in a simple conf file (meaning one without
	# multiple different Mason configs) we make the apachehandler
	# object now and simply reuse it later in the handler sub
	$AH = $pack->make_ah() if $pack->_in_simple_conf_file;
    }
}

#
# This is our best guess as to whether we are being configured via the
# conf file without multiple configs.  It's flawed, because simple
# configurations don't require an explicit component root to be set.
#
sub _in_simple_conf_file
{
    my $self = shift;

    my @roots = $self->_get_string_param('MasonCompRoot');
    return $ENV{MOD_PERL} && @roots;
}

my %AH;
sub make_ah
{
    my ($package, $r) = @_;

    my ($vals, %p) = $package->_get_mason_params($r);

    #
    # Now that we have all the original config strings stored, we put
    # them all together in a string that we use to determine whether
    # or not we've seen this particular set of config values before.
    #
    my $key = join $;, map "$_$;$vals->{$_}", sort keys %$vals;

    #
    # If the user has virtual hosts, each with a different document
    # root, then we will have to be called from the handler method.
    # This means we have an active request.  In order to distinguish
    # between virtual hosts with identical config directives that have
    # no comp root defined (meaning they expect to use the default
    # comp root), we append the document root for the current request
    # to the key.
    #
    $key .= $; . $r->document_root if $r;

    return $AH{$key} if exists $AH{$key};

    # can't use hash_list for this one because it's _either_ a string
    # or a hash_list
    if (exists $p{comp_root}) {
	if (@{$p{comp_root}} == 1 && $p{comp_root}->[0] !~ /=>/) {
	    $p{comp_root} = $p{comp_root}[0];  # Convert to a simple string
	} else {
            my @roots;
	    foreach my $root (@{$p{comp_root}}) {
		$root = [ split /\s*=>\s*/, $root, 2 ];
		param_error "Configuration parameter MasonCompRoot must be either ".
                            "a single string value or multiple key/value pairs ".
                            "like 'foo => /home/mason/foo'.  Invalid parameter:\n$root"
		    unless defined $root->[1];

                push @roots, $root;
	    }

            $p{comp_root} = \@roots;
	}
    }

    my $ah = $package->new(%p, $r);
    $AH{$key} = $ah if $key;

    return $ah;
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

sub _get_mason_params
{
    my $self = shift;
    my $r = shift;

    my $config = $r ? $r->dir_config : Apache->server->dir_config;

    #
    # We will accumulate all the string versions of the keys and
    # values here for later use.
    #
    my %vals;

    # Get all params starting with 'Mason'
    my %candidates;

    foreach my $studly ( keys %$config )
    {
	(my $calm = $studly) =~ s/^Mason// or next;
	$calm = $self->calm_form($calm);

	$candidates{$calm} = $config->{$studly};
    }

    return ( \%vals,
             map { $_ =>
                       scalar $self->get_param( $_, \%vals, \%candidates, $r )
                   }
             keys %candidates );
}

sub get_param {
    # Gets a single config item from dir_config.

    my ($self, $key, $vals, $params, $r) = @_;

    $key = $self->calm_form($key);

    my $spec = $self->allowed_params( $params || {} )->{$key}
        or error "Unknown config item '$key'";

    # Guess the default parse type from the Params::Validate validation spec
    my $type = ($spec->{parse} or
		$spec->{type} & ARRAYREF ? 'list' :
		$spec->{type} & SCALAR   ? 'string' :
		$spec->{type} & CODEREF  ? 'code' :
		undef)
        or error "Unknown parse type for config item '$key'";

    my $method = "_get_${type}_param";
    return $self->$method('Mason'.$self->studly_form($key), $vals, $r);
}

sub _get_string_param
{
    my $self = shift;
    return $self->_get_val(@_);
}

sub _get_boolean_param
{
    my $self = shift;
    return $self->_get_val(@_);
}

sub _get_code_param
{
    my $self = shift;
    my $p = $_[0];
    my $val = $self->_get_val(@_);

    return unless $val;

    my $sub_ref = eval $val;

    param_error "Configuration parameter '$p' is not valid perl:\n$@\n"
	if $@;

    return $sub_ref;
}

sub _get_list_param
{
    my $self = shift;
    my @val = $self->_get_val(@_);
    if (@val == 1 && ! defined $val[0])
    {
	@val = ();
    }

    return \@val;
}

sub _get_hash_list_param
{
    my $self = shift;
    my @val = $self->_get_val(@_);
    if (@val == 1 && ! defined $val[0])
    {
        return {};
    }

    my %hash;
    foreach my $pair (@val)
    {
        my ($key, $val) = split /\s*=>\s*/, $pair, 2;
        param_error "Configuration parameter $_[0] must be a key/value pair ".
                    qq|like "foo => 'bar'".  Invalid parameter:\n$pair|
                unless defined $key && defined $val;

        $hash{$key} = $val;
    }

    return \%hash;
}

use constant
    HAS_TABLE_API => $mod_perl::VERSION >= 1.99 || Apache::perl_hook('TableApi');

sub _get_val
{
    my ($self, $p, $vals, $r) = @_;

    my $c = $r ? $r : Apache->server;
    my @val = HAS_TABLE_API ? $c->dir_config->get($p) : $c->dir_config($p);

    param_error "Only a single value is allowed for configuration parameter '$p'\n"
	if @val > 1 && ! wantarray;

    $vals->{$p} = join '', @val if $vals;

    return wantarray ? @val : $val[0];
}

sub new
{
    my $class = shift;
    # get $r off end of params if its there
    my $r = pop if @_ % 2 == 1;

    my %defaults = ( request_class  => 'HTML::Mason::Request::ApacheHandler',
                     resolver_class => 'HTML::Mason::Resolver::File::ApacheHandler',
                   );
    my $allowed_params = $class->allowed_params(%defaults, @_);

    if ( exists $allowed_params->{comp_root} and
	 my $req = $r || Apache->request )  # DocumentRoot is only available inside requests
    {
	$defaults{comp_root} = $req->document_root;
    }

    my %params = @_;

    if (exists $allowed_params->{data_dir} and not exists $params{data_dir})
    {
	# constructs path to <server root>/mason
	my $def = $defaults{data_dir} = Apache->server_root_relative('mason');
	die "Default data_dir (MasonDataDir) '$def' must be an absolute path"
	    unless File::Spec->file_name_is_absolute($def);
	  
	my @levels = File::Spec->splitdir($def);
	die "Default data_dir (MasonDataDir) '$def' must be more than two levels deep (or must be set explicitly)"
	    if @levels <= 3;
    }

    # Set default error_format based on error_mode
    if (exists($params{error_mode}) and $params{error_mode} eq 'fatal') {
	$defaults{error_format} = 'line';
    } else {
	$defaults{error_mode} = 'output';
	$defaults{error_format} = 'html';
    }

    # Push $r onto default allow_globals
    if (exists $allowed_params->{allow_globals}) {
	if ( $params{allow_globals} ) {
	    push @{ $params{allow_globals} }, '$r';
	} else {
	    $defaults{allow_globals} = ['$r'];
	}
    }

    # Don't allow resolver to get created without comp_root, if it needs one
    if ( exists $allowed_params->{comp_root} &&
         ! $defaults{comp_root} &&
         ! $params{comp_root} )
    {
	die "No comp_root specified and cannot determine DocumentRoot." .
            " Please provide comp_root explicitly.";
    }

    my $self = $class->SUPER::new(%defaults, %params);

    unless ( $self->interp->resolver->can('apache_request_to_comp_path') )
    {
	error "The resolver class your Interp object uses does not implement " .
              "the 'resolve_backwards' method.  This means that ApacheHandler " .
              "cannot resolve requests.  Are you using a handler.pl file created ".
	      "before version 1.10?  Please see the handler.pl sample " .
              "that comes with the latest version of Mason.";
    }

    # If we're running as superuser, change file ownership to http user & group
    if (!($> || $<) && $self->interp->files_written)
    {
	chown Apache->server->uid, Apache->server->gid, $self->interp->files_written
	    or system_error( "Can't change ownership of files written by interp object: $!\n" );
    }

    $self->_initialize;
    return $self;
}

# Register with Apache::Status at module startup.  Will get replaced
# with a more informative status once an interpreter has been created.
my $status_name = 'mason0001';
if ( load_pkg('Apache::Status') )
{
    Apache::Status->menu_item
	($status_name => __PACKAGE__->allowed_params->{apache_status_title}{default},
         sub { ["<b>(no interpreters created in this child yet)</b>"] });
}


sub _initialize {
    my ($self) = @_;

    if ($self->args_method eq 'mod_perl') {
	unless (defined $Apache::Request::VERSION) {
	    warn "Loading Apache::Request at runtime.  You could " .
                 "increase shared memory between Apache processes by ".
                 "preloading it in your httpd.conf or handler.pl file\n";
	    require Apache::Request;
	}
    } else {
	unless (defined $CGI::VERSION) {
	    warn "Loading CGI at runtime.  You could increase shared ".
                 "memory between Apache processes by preloading it in ".
                 "your httpd.conf or handler.pl file\n";

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
		    $self->status_as_html(apache_req => $r),
		    $self->interp->status_as_html(ah => $self, apache_req => $r)];
	};
	local $^W = 0; # to avoid subroutine redefined warnings
	Apache::Status->menu_item($status_name, $self->apache_status_title, $statsub);
    }

    my $interp = $self->interp;

    #
    # Allow global $r in components
    #
    $interp->compiler->add_allowed_globals('$r')
	if $interp->compiler->can('add_allowed_globals');
}

# Generate HTML that describes ApacheHandler's current status.
# This is used in things like Apache::Status reports.

sub status_as_html {
    my ($self, %p) = @_;

    # Should I be scared about this?  =)

    my $comp_source = <<'EOF';
<h3>ApacheHandler properties:</h3>
<blockquote>
 <tt>
<table width="75%">
<%perl>
foreach my $property (sort keys %$ah) {
    my $val = $ah->{$property};
    my $default = ( defined $val && defined $valid{$property}{default} && $val eq $valid{$property}{default} ) || ( ! defined $val && exists $valid{$property}{default} && ! defined $valid{$property}{default} );

    my $display = $val;
    if (ref $val) {
        $display = '<font color="darkred">';
        # only object can ->can, others die
        my $is_object = eval { $val->can('anything'); 1 };
        if ($is_object) {
            $display .= ref $val . ' object';
        } else {
            if (UNIVERSAL::isa($val, 'ARRAY')) {
                $display .= 'ARRAY reference - [ ';
                $display .= join ', ', @$val;
                $display .= '] ';
            } elsif (UNIVERSAL::isa($val, 'HASH')) {
                $display .= 'HASH reference - { ';
                my @pairs;
                while (my ($k, $v) = each %$val) {
                   push @pairs, "$k => $v";
                }
                $display .= join ', ', @pairs;
                $display .= ' }';
            } else {
                $display = ref $val . ' reference';
            }
        }
        $display .= '</font>';
    }

    defined $display && $display =~ s,([\x00-\x1F]),'<font color="purple">control-' . chr( ord('A') + ord($1) - 1 ) . '</font>',eg; # does this work for non-ASCII?
</%perl>
 <tr valign="top" cellspacing="10">
  <td>
    <% $property | h %>
  </td>
  <td>
   <% defined $display ? $display : '<i>undef</i>' %>
   <% $default ? '<font color=green>(default)</font>' : '' %>
  </td>
 </tr>
% }
</table>
  </tt>
</blockquote>

<%args>
 $ah       # The ApacheHandler we'll elucidate
 %valid    # Contains default values for member data
</%args>
EOF

    my $interp = $self->interp;
    my $comp = $interp->make_component(comp_source => $comp_source);
    my $out;

    $self->interp->make_request
	( comp => $comp,
	  args => [ah => $self, valid => $interp->allowed_params],
	  ah => $self,
	  apache_req => $p{apache_req},
	  out_method => \$out,
	)->exec;

    return $out;
}

sub handle_request
{
    my ($self, $r) = @_;

    my $req = $self->prepare_request($r);
    return $req unless ref($req);
    $req->exec;
}

sub prepare_request
{
    my ($self, $r) = @_;

    if (lc($r->dir_config('Filter')) eq 'on') {
	$r = $r->filter_register;
    }

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
    # Compute the component path via the resolver. Return NOT_FOUND on failure.
    #
    my $comp_path = $interp->resolver->apache_request_to_comp_path($r);
    unless ($comp_path) {
	#
	# Append path_info if filename does not represent an existing file
	# (mainly for dhandlers).
	#
	my $pathname = $r->filename;
	$pathname .= $r->path_info unless $is_file;

	$r->warn("[Mason] Cannot resolve file to component: " .
                 "$pathname (is file outside component root?)");
	return $self->return_not_found($r);
    }

    # Assigning $r to a new variable here is _crucial_ to avoid a
    # memory leak if we create an Apache::Request object in
    # request_args().
    my ($args, $new_r, $cgi_object) = $self->request_args($r);

    #
    # Set up interpreter global variables.
    #
    $interp->set_global( r => $new_r );

    # If someone is using a custom request class that doesn't accept
    # 'ah' and 'apache_req' that's their problem.
    #
    my $request = $interp->make_request( comp => $comp_path,
					 args => [%$args],
					 ah => $self,
					 apache_req => $new_r,
				       );

    # get this from current object.
    my $real_apache_print = $r->can('print');

    # Craft the request's out method to handle http headers, content
    # length, and HEAD requests.
    my $sent_headers = 0;
    my $out_method = sub {

	# Send headers if they have not been sent by us or by user.
        # We use instance here because if we store $request we get a
        # circular reference and a big memory leak.
	if (!$sent_headers and HTML::Mason::Request->instance->auto_send_headers) {
	    unless (http_header_sent($new_r)) {
		$new_r->send_http_header();
	    }
	    $sent_headers = 1;
	}

	# We could perhaps install a new, faster out_method here that
	# wouldn't have to keep checking whether headers have been
	# sent and what the $r->method is.  That would require
	# additions to the Request interface, though.

	
	# Call $new_r->print (using the real Apache method, not our
	# overriden method). If request was HEAD, suppress output.
	unless ($new_r->method eq 'HEAD') {
	    $new_r->$real_apache_print(grep {defined} @_);
	}
    };

    $request->out_method($out_method);

    $request->cgi_object($cgi_object) if $cgi_object;

    return $request;
}

sub request_args
{
    my ($self, $r) = @_;

    #
    # Get arguments from Apache::Request or CGI.
    #

    # put Apache::Request object here.  Overwriting $r causes a memory
    # leak!
    my $apr;

    my ($args, $cgi_object);
    if ($self->args_method eq 'mod_perl') {
        $apr = ( UNIVERSAL::isa($r, 'Apache::Request') ?
                 $r :
                 Apache::Request->instance($r) );

	$args = $self->_mod_perl_args($apr);
    } else {
        $apr = $r;
	$cgi_object = CGI->new;
	$args = $self->_cgi_args($r, $cgi_object);
    }
    return ($args, $apr, $cgi_object);
}

#
# Get $args hashref via CGI package
#
sub _cgi_args
{
    my ($self, $r, $q) = @_;

    # For optimization, don't bother creating a CGI object if request
    # is a GET with no query string
    return {} if $r->method eq 'GET' && !scalar($r->args);

    return HTML::Mason::Utils::cgi_request_args($q, $r->method);
}

#
# Get $args hashref via Apache::Request package.
#
sub _mod_perl_args
{
    my ($self, $apr, $request) = @_;

    my %args;
    foreach my $key ( $apr->param ) {
	my @values = $apr->param($key);
	$args{$key} = @values == 1 ? $values[0] : \@values;
    }

    return \%args;
}

#
# Determines whether the http header has been sent.
#
sub http_header_sent { shift->header_out("Content-type") }

# Utility function to prepare $r before returning NOT_FOUND.
sub return_not_found
{
    my ($self, $r) = @_;

    if ($r->method eq 'POST') {
	$r->method('GET');
	$r->headers_in->unset('Content-length');
    }
    return NOT_FOUND;
}

#
# PerlHandler HTML::Mason::ApacheHandler
#
BEGIN
{
    # A method handler is prototyped differently in mod_perl 1.x than in 2.x
    my $handler_code = sprintf <<'EOF', $mod_perl::VERSION >= 1.99 ? ': method' : '($$)';
sub handler %s
{
    my ($package, $r) = @_;
    my $ah = $AH || $package->make_ah($r);
    return $ah->handle_request($r);
}
EOF
    eval $handler_code;
    die $@ if $@;
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

=head1 PARAMETERS TO THE new() CONSTRUCTOR

=over

=item apache_status_title

Title that you want this ApacheHandler to appear as under
Apache::Status.  Default is "HTML::Mason status".  This is useful if
you create more than one ApacheHandler object and want them all
visible via Apache::Status.

=item args_method

Method to use for unpacking GET and POST arguments. The valid options
are 'CGI' and 'mod_perl'; these indicate that a C<CGI.pm> or
C<Apache::Request> object (respectively) will be created for the purposes
of argument handling.

'mod_perl' is the default and requires that you have installed the
C<Apache::Request> package.

If the args_method is 'CGI', the Mason request object (C<$m>) will have a
method called C<cgi_object> available.  This method returns the CGI
object used for argument processing.

If args_method is 'mod_perl', the C<$r> global is upgraded to an
Apache::Request object. This object inherits all Apache methods and
adds a few of its own, dealing with parameters and file uploads.  See
C<Apache::Request> for more information.

While Mason will load C<Apache::Request> or C<CGI> as needed at runtime, it
is recommended that you preload the relevant module either in your
F<httpd.conf> or F<handler.pl> file, as this will save some memory.

=item decline_dirs

True or false, default is true. Indicates whether Mason should decline
directory requests, leaving Apache to serve up a directory index or a
C<FORBIDDEN> error as appropriate. See ADMIN<allowing directory requests>
for more information about handling directories with Mason.

=item interp

The interpreter object to associate with this compiler. By default a
new object of class P<interp_class> will be created.

=item interp_class

The class to use when creating a interpreter. Defaults to
L<HTML::Mason::Interp|HTML::Mason::Interp>.

=back

=head1 ACCESSOR METHODS

All of the above properties, except interp_class, have standard accessor
methods of the same name: no arguments retrieves the value, and one
argument sets it, except for args_method, which is not settable.  For
example:

    my $ah = new HTML::Mason::ApacheHandler;
    my $decline_dirs = $ah->decline_dirs;
    $ah->decline_dirs(1);

=head1 OTHER METHODS

The ApacheHandler object has a few other publically accessible methods
that may be of interest to end users.

=over 4

=item handle_request ($r)

This method takes an Apache object representing a request and
translates that request into a form Mason can understand.  It's return
value is an Apache status code.

=item prepare_request ($r)

This method takes an Apache object representing a request and returns
a new Mason request object or an Apache status code.  If it is a
request object you can manipulate that object as you like, and then
call the request object's C<exec> method to have it generate output.

If this method returns an Apache status code, that means that it could
not create a Mason request object.

This method is useful if you would like to have a chance to decline a
request based on properties of the Mason request object or a component
object.  For example:

    my $req = $ah->prepare_request($r);
    # $req must be an Apache status code if it's not an object
    return $req unless ref($req);

    return DECLINED
        unless $req->request_comp->source_file =~ /\.html$/;

    $req->exec;

=item request_args ($r)

Given an Apache request object, this method returns a three item list.
The first item is a hash reference containing the arguments passed by
the client's request.

The second is an Apache request object, possibly the one originally
passed to the method.

The third item may be a CGI.pm object or C<undef>, depending on the
value of the P<args_method> parameter.

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Admin|HTML::Mason::Admin>,
L<HTML::Mason::Interp|HTML::Mason::Interp>

=cut
