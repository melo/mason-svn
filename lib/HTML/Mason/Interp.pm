# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Interp;

use strict;

use Carp;
use File::Basename;
use File::Path;
use File::Spec;
use HTML::Mason::Request;
use HTML::Mason::Resolver::File;
use HTML::Mason::Tools qw(make_fh read_file taint_is_on load_pkg);

use HTML::Mason::Exceptions( abbr => [qw(param_error system_error wrong_compiler_error compilation_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_  } );

use HTML::Mason::Container;
use base qw(HTML::Mason::Container);

BEGIN
{
    # Fields that can be set in new method, with defaults
    __PACKAGE__->valid_params
	(
	 autohandler_name             => { parse => 'string',  default => 'autohandler', type => SCALAR,
					   descr => "The filename to use for Mason's 'autohandler' capability" },
	 code_cache_max_size          => { parse => 'string',  default => 10*1024*1024, type => SCALAR,  # 10M
					   descr => "The maximum size of the component code cache" },
	 compiler                     => { isa => 'HTML::Mason::Compiler',
					   descr => "A Compiler object for compiling components" },
	 data_dir                     => { parse => 'string', optional => 1, type => SCALAR,
					   descr => "A directory for storing cache files and other state information" },
	 dhandler_name                => { parse => 'string',  default => 'dhandler', type => SCALAR,
					   descr => "The filename to use for Mason's 'dhandler' capability" },
	 fixed_source                 => { parse => 'boolean', default => 0, type => BOOLEAN,
					   descr => "When true, we only compile source files once" },
	 # OBJECT cause qr// returns an object
	 ignore_warnings_expr         => { parse => 'string',  type => SCALAR|OBJECT,
					   default => qr/Subroutine .* redefined/i,
					   descr => "A regular expression describing Perl warning messages to ignore" },
	 out_method                   => { parse => 'code',    type => CODEREF|SCALARREF,
					   default => sub { print STDOUT grep {defined} @_ },
					   descr => "A subroutine or scalar reference through which all output will pass" },
	 max_recurse                  => { parse => 'string',  default => 32, type => SCALAR,
					   descr => "The maximum recursion depth for component, inheritance, and request stack" },
	 preloads                     => { parse => 'list',    optional => 1, type => ARRAYREF,
					   descr => "A list of components to load immediately when creating the Interpreter" },
	 resolver                     => { isa => 'HTML::Mason::Resolver',
					   descr => "A Resolver object for fetching components from storage" },
	 system_log_events            => { parse => 'string',  optional => 1, type => BOOLEAN|HASHREF,
					   descr => "An array or |-separated string of Mason events to log" },
	 system_log_file              => { parse => 'string',  optional => 1, type => SCALAR,
					   descr => "A filename in which Mason events will be logged" },
	 system_log_separator         => { parse => 'string',  default => "\cA", type => SCALAR,
					   descr => "A string to separate entries in the Mason events log" },
	 use_object_files             => { parse => 'boolean', default => 1, type => BOOLEAN,
					   descr => "Whether to cache component objects on disk" },
	);

    __PACKAGE__->contained_objects
	(
	 resolver => 'HTML::Mason::Resolver::File',
	 compiler => 'HTML::Mason::Compiler::ToObject',
	 request  => { class => ( 'HTML::Mason::Request' ),
		       delayed => 1 },
	);
}

use HTML::Mason::MethodMaker
    ( read_only => [ qw( code_cache
			 hooks
			 system_log_file
			 system_log_separator
			 preloads
                         source_cache ) ],

      read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		      qw( autohandler_name
		          code_cache_max_size
			  compiler
			  data_dir
			  dhandler_name
			  fixed_source
			  ignore_warnings_expr
			  max_recurse
			  resolver
			  use_object_files )
		    ],

      read_write_contained => { request => [ [ autoflush => { type => BOOLEAN } ],
					     [ data_cache_defaults => { type => HASHREF } ],
					   ]
			      },
      );

sub new
{
    my $class = shift;

    my @args = $class->create_contained_objects(@_);

    my $self = bless { validate( @args, $class->validation_spec ),
		       code_cache => {},
		       code_cache_current_size => 0,
		       files_written => [],
		       hooks => {},
		       system_log_fh => undef,
		       system_log_events_hash => undef,
		     }, $class;

    $self->system_log_events($self->{system_log_events}) if exists $self->{system_log_events};
    $self->_initialize;
    return $self;
}

sub _initialize
{
    my ($self) = shift;
    $self->{code_cache} = {};
    $self->{code_cache_current_size} = 0;

    #
    # Check that directories are absolute.
    #
    foreach my $field (qw(data_dir)) {
	if ($self->{$field}) {
	    $self->{$field} = File::Spec->canonpath( $self->{$field} );
	    param_error "$field '".$self->{$field}."' must be an absolute directory"
		unless File::Spec->file_name_is_absolute( $self->{$field} );
	}
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    if ($self->data_dir) {
	foreach my $subdir ( qw(obj cache etc) ) {
	    my @newdirs = mkpath( File::Spec->catdir( $self->data_dir, $subdir ) , 0, 0775 );
	    $self->push_files_written(@newdirs);
	}
    } else {
	$self->use_object_files(0);
    }

    #
    # Open system log file
    #
    if ($self->{system_log_events_hash}) {
	if (! $self->system_log_file && $self->data_dir) {
	    # make a default if we have a data_dir
	    $self->system_log_file( File::Spec->catfile( $self->data_dir, 'etc', 'system.log' ) );
	}
	if ($self->system_log_file) {
	    my $fh = make_fh();
	    open $fh, ">>".$self->system_log_file
		or system_error "Couldn't open system log file $self->{system_log_file} for append";
	    my $oldfh = select $fh;
	    $| = 1;
	    select $oldfh;
	    $self->{system_log_fh} = $fh;
	}
    }

    #
    # Preloads
    #
    if ($self->preloads) {
	foreach my $pattern (@{$self->preloads}) {
	    error "preload pattern '$pattern' must be an absolute path"
		unless File::Spec->file_name_is_absolute($pattern);
	    my @paths = $self->resolver->glob_path($pattern)
		or warn "Didn't find any components for preload pattern '$pattern'";
	    foreach (@paths) { $self->load($_) }
	}
    }
}

#
# Shorthand for various data subdirectories and files.
#
sub object_dir { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'obj' ) : ''; }
sub cache_dir  { my $self = shift; return $self->data_dir ? File::Spec->catdir( $self->data_dir, 'cache' ) : ''; }

#
# exec is the initial entry point for executing a component
# in a new request.
#
sub exec {
    my ($self, $comp, @args) = @_;
    $self->make_request(comp=>$comp, args=>\@args)->exec;
}

sub make_request {
    my $self = shift;

    return $self->create_delayed_object( 'request', interp => $self, out_method => $self->out_method, @_ );
}

sub comp_exists {
    my ($self, $path) = @_;
    return $self->resolver->get_info($path);
}

#
# Load <$path> into a component, possibly parsing the source and/or
# caching the code. Returns a component object or undef if the
# component was not found.
#
sub load {
    my ($self,$path) = @_;
    my ($maxfilemod, $objfile, $objfilemod);
    my $code_cache = $self->code_cache;
    my $resolver = $self->{resolver};

    #
    # Get source info from resolver. Cache the results in fixed_source mode.
    #
    my $source;
    if ($self->{fixed_source}) {
	unless (exists($self->{source_cache}{$path})) {
	    $self->{source_cache}{$path} = $resolver->get_info($path);
	}
	$source = $self->{source_cache}{$path};
    } else {
	$source = $resolver->get_info($path);
    }

    # No component matches this path.
    return unless defined $source;

    # comp_id is the unique name for the component, used for cache key
    # and object file name.
    my $comp_id = $source->comp_id;

    #
    # Get last modified time of source.
    #
    my $srcmod = $source->last_modified;

    #
    # If code cache contains an up to date entry for this path, use
    # the cached sub.  Always use the cached sub in fixed_source mode.
    #
    return $code_cache->{$comp_id}->{comp}
	if exists($code_cache->{$comp_id}) and ( $self->{fixed_source} || $code_cache->{$comp_id}->{lastmod} >= $srcmod );

    if ($self->{use_object_files}) {
	$objfile = $self->comp_id_to_objfile($comp_id);

	my @stat = stat $objfile;
	if ( @stat && ! -f _ ) {
	    error "The object file '$objfile' exists but it is not a file!";
	}

	if ($self->{fixed_source}) {
	    # No entry in the code cache so if the object file exists,
	    # we will use it, otherwise we must create it.  These
	    # values make that happen.
	    $objfilemod = @stat ? $srcmod : 0;
	} else {
	    # If the object file exists, get its modification time.
	    # Otherwise (it doesn't exist or it is a directory) we
	    # must create it.
	    $objfilemod = @stat ? $stat[9] : 0;
	}
    }

    #
    # Load the component from source or object file.
    #
    $self->write_system_log('COMP_LOAD', $comp_id);	# log the load event

    my $comp;
    if ($objfile) {
	#
	# We are using object files.  Update object file if necessary
	# and load component from there.
	#
	my $object_code;
	do
	{
	    if ($objfilemod < $srcmod) {
		$object_code = $source->object_code( compiler => $self->compiler );
		$self->write_object_file( object_code => $object_code, object_file => $objfile );
	    }
	    # read the existing object file
	    $object_code ||= read_file($objfile);
	    $comp = eval { $self->eval_object_code( object_code => $object_code ) };

	    if ($@) {
		if (isa_mason_exception($@, 'Compilation::IncompatibleCompiler')) {
		    $objfilemod = 0;
		    undef $object_code;
		} else {
		    $self->_compilation_error( $source->friendly_name, $@ );
		}
	    }
	} until ($object_code);
    } else {
	#
	# No object files. Load component directly into memory.
	#
	my $object_code = $source->object_code( compiler => $self->compiler );
	$comp = eval { $self->eval_object_code( object_code => $object_code ) };
	$self->_compilation_error( $source->friendly_name, $@ ) if $@;
    }
    $comp->assign_runtime_properties($self, $source);

    #
    # Delete any stale cached version of this component, then
    # cache it if it's small enough.
    #
    $self->delete_from_code_cache($comp_id);

    if ($comp->object_size <= $self->code_cache_max_elem) {
	$code_cache->{$comp_id} = {lastmod=>$srcmod, comp=>$comp, type=>'physical'};
	$self->{code_cache_current_size} += $comp->object_size;
    }
    return $comp;
}

sub delete_from_code_cache {
    my ($self, $comp) = @_;
    return unless exists $self->{code_cache}{$comp};

    $self->{code_cache_current_size} -= $self->{code_cache}{$comp}{comp}->object_size;
    delete $self->{code_cache}{$comp};
    return;
}


sub comp_id_to_objfile {
    my ($self, $comp_id) = @_;

    return File::Spec->catfile( $self->object_dir, split /\//, $comp_id );
}

#
# If code cache has exceeded maximum, remove least frequently used
# elements from cache until size falls below minimum.
#
sub purge_code_cache {
    my ($self) = @_;

    if ($self->{code_cache_current_size} > $self->code_cache_max_size) {
	my $code_cache = $self->{code_cache};
	my $min_size = $self->code_cache_min_size;
	my $decay_factor = $self->code_cache_decay_factor;

	my @elems;
	while (my ($path,$href) = each(%{$code_cache})) {
	    push(@elems,[$path,$href->{comp}->mfu_count,$href->{comp}]);
	}
	@elems = sort { $a->[1] <=> $b->[1] } @elems;
	while (($self->{code_cache_current_size} > $min_size) and @elems) {
	    $self->delete_from_code_cache(shift(@elems)->[0]);
	}

	#
	# Multiply each remaining cache item's count by a decay factor,
	# to gradually reduce impact of old information.
	#
	foreach my $elem (@elems) {
	    $elem->[2]->mfu_count( $elem->[2]->mfu_count * $decay_factor );
	}
    }
}

#
# Construct a component on the fly.  Virtual if 'path' parameter is
# given, otherwise anonymous.
#
sub make_component {
    my $self = shift;

    my %p = validate(@_, { comp_source => { type => SCALAR, optional => 1 },
			   comp_file   => { type => SCALAR, optional => 1 },
			   name        => { type => SCALAR, optional => 1 } });

    $p{comp_source} = read_file(delete $p{comp_file}) if exists $p{comp_file};
    param_error "Must specify either 'comp_source' or 'comp_file' parameter to 'make_component()'"
	unless $p{comp_source};

    $p{name} ||= '<anonymous component>';

    my $source = HTML::Mason::ComponentSource->new( friendly_name => $p{name},
						    comp_path => $p{name},
						    comp_id => undef,
						    last_modified => time,
						    comp_class => 'HTML::Mason::Component',
						    source_callback => sub { $p{comp_source} },
						  );

    my $object_code = $source->object_code( compiler => $self->compiler);

    my $comp = eval { $self->eval_object_code( object_code => $object_code ) };
    $self->_compilation_error( $p{name}, $@ ) if $@;

    $comp->assign_runtime_properties($self, $source);

    return $comp;
}

sub set_global
{
    my ($self, $decl, @values) = @_;
    param_error "Interp->set_global: expects a variable name and one or more values"
	unless @values;
    my ($prefix, $name) = ($decl =~ s/^([\$@%])//) ? ($1, $decl) : ('$', $decl);

    my $varname = sprintf("%s::%s",$self->compiler->in_package,$name);
    no strict 'refs';
    if ($prefix eq '$') {
	$$varname = $values[0];
    } elsif ($prefix eq '@') {
	@$varname = @values;
    } else {
	%$varname = @values;
    }
}

#
# Allow scalar or hash reference as argument to system_log_events.
#
sub system_log_events
{
    my ($self, $value) = @_;
    if (defined($value)) {
	if (!ref($value)) {
	    $value =~ s/\s//g;
	    my %opts = map( ($_, 1), split /\|/, $value);
	    @opts{qw(REQUEST CACHE COMP_LOAD)} = (1,1,1) if $opts{ALL};
	    @opts{qw(CACHE_READ CACHE_WRITE)} = (1,1) if $opts{CACHE};
	    @opts{qw(REQ_START REQ_END)} = (1,1) if $opts{REQUEST};
	    $self->{system_log_events_hash} = \%opts;
	} elsif (ref($value) eq 'HASH') {
	    $self->{system_log_events_hash} = $value;
	} else {
	    param_error "Interp->system_log_events: argument must be a scalar or hash reference, not '$value'";
	}
    }
    return $self->{system_log_events};
}

#
# Determine if the specified event should be logged.
#
sub system_log_event_check
{
    my ($self,$flag) = @_;
    return ($self->{system_log_fh} && $self->{system_log_events_hash}->{$flag});
}

sub comp_root { shift->resolver->comp_root(@_) }

#
# Allow scalar or code reference as argument to out_method.
#
sub out_method
{
    my ($self) = shift;

    if (@_)
    {
	validate_pos( @_, { type => SCALARREF | CODEREF } );
	$self->{out_method} = shift;
    }

    return $self->{out_method};
}

sub files_written
{
    my $self = shift;
    return @{$self->{files_written}};
}

#
# Push onto list of written files.
#
sub push_files_written
{
    my $self = shift;
    my $fref = $self->{'files_written'};
    push(@$fref,@_);
}

#
# Look for component <$name> starting in <$startpath> and moving upwards
# to the root. Return component object or undef.
#
sub find_comp_upwards
{
    my ($self, $startpath, $name) = @_;
    $startpath =~ s{/+$}{};

    # Don't use File::Spec here, this is a URL path.
    do {
      my $comp = $self->load("$startpath/$name");
      return $comp if $comp;
    } while $startpath =~ s{/+[^/]*$}{};

    return;  # Nothing found
}

#
# Hook functions.
#
my @hook_types = qw(start_comp end_comp start_file end_file);
my %hook_type_map = map(($_,1),@hook_types);

sub add_hook {
    my $self = shift;
    my %args = validate( @_, { name => {type => SCALAR},
			       code => {type => CODEREF},
			       type => {type => SCALAR,
					callbacks =>
					{ 'Must be one of ' . join(",",@hook_types) => sub {exists $hook_type_map{$_[0]}} },
				       }
			     } );
    $self->{hooks}->{$args{type}}->{$args{name}} = $args{code};
}

sub remove_hook {
    my $self = shift;
    my %args = validate( @_, { name => SCALAR,
			       type => SCALAR });

    delete($self->{hooks}->{$args{type}}->{$args{name}});
}

#
# Write a line to the Mason system log.
# Each line begins with the time, pid, and event name.
#
# We only print the line if the log file handle is defined AND the
# event name is in system_log_events_hash.
#
sub write_system_log {
    my $self = shift;

    if ($self->{system_log_fh} && $self->{system_log_events_hash}->{$_[0]}) {
	my $time = (load_pkg('Time::HiRes') ? scalar(Time::HiRes::gettimeofday()) : time);
	my $fh = $self->{system_log_fh};
	print $fh (join ($self->system_log_separator,
			 $time,                  # current time
			 $_[0],                  # event name
			 $$,                     # pid
			 @_[1..$#_]              # event-specific fields
			),"\n")
	    or param_error "Cannot write to system log: $!";
    }
}

# Code cache parameter methods

sub code_cache_min_size { shift->code_cache_max_size * 0.75 }
sub code_cache_max_elem { shift->code_cache_max_size * 0.20 }
sub code_cache_decay_factor { 0.75 }


###################################################################
# The eval_object_code & write_object_file methods used to be in
# Parser.pm.  This is a temporary home only.  They need to be moved
# again at some point in the future (during some sort of interp
# re-architecting.
###################################################################

#
# eval_object_code
#   (object_code, object_file, error)
# Evaluate an object file or object text.  Return a component object
# or undef if error.
#
# I think this belongs in the resolver (or comp loader) - Dave
#
sub eval_object_code
{
    my ($self, %p) = @_;
    my $object_code = $p{object_code};

    # If in taint mode, untaint the object text
    ($object_code) = ($object_code =~ /^(.*)/s) if taint_is_on;

    #
    # Evaluate object file or text with warnings on
    #
    my $ignore_expr = $self->ignore_warnings_expr;
    my ($comp, $err);
    my $warnstr = '';

    {
	local $^W = 1;
	local $SIG{__WARN__} = $ignore_expr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignore_expr/ } : sub { $warnstr .= $_[0] };

	$comp = eval $object_code;
    }

    $err = $warnstr . $@;

    #
    # If no error generated and no component object returned, we
    # have a prematurely-exited <%once> section or other syntax
    # accident.
    #
    unless (1 or $err or (defined($comp) and (UNIVERSAL::isa($comp, 'HTML::Mason::Component') or ref($comp) eq 'CODE'))) {
	$err = "could not generate component object (return() in a <%once> section or extra close brace?)";
    }

    unless ($err) {
	# Yes, I know I always freak out when people start poking
	# around in object internals but since there is no longer a
	# parser_version method in Component.pm there is no other way.
	# Only pre-1.10 components have parser_version set.
	wrong_compiler_error 'This object file was created by a pre-1.10 parser.  Please remove the component files in your object directory.'
	    if ref $comp && exists $comp->{parser_version};

	wrong_compiler_error 'This object file was created by an incompatible Compiler or Lexer.  Please remove the component files in your object directory.'
	    if UNIVERSAL::can( $comp, 'compiler_id' ) && $comp->compiler_id ne $self->compiler->object_id;
    }

    #
    # Return component or error
    #
    if ($err) {
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}

	compilation_error $err;
    } else {
	return $comp;
    }
}

#
# write_object_file
#   (object_code=>..., object_file=>..., files_written=>...)
# Save object text in an object file.
#
# We attempt to handle several cases in which a file already exists
# and we wish to create a directory, or vice versa.  However, not
# every case is handled; to be complete, mkpath would have to unlink
# any existing file in its way.
#
#
# I think this belongs in the comp storage mechanism - Dave
#
sub write_object_file
{
    my ($self, %options) = @_;
    my ($object_code,$object_file,$files_written) =
	@options{qw(object_code object_file files_written)};
    my @newfiles = ($object_file);

    if (defined $object_file && !-f $object_file) {
	my ($dirname) = dirname($object_file);
	if (!-d $dirname) {
	    unlink($dirname) if (-e $dirname);
	    push(@newfiles,mkpath($dirname,0,0775));
	    system_error "Couldn't create directory $dirname: $!"
		unless -d $dirname;
	}
	rmtree($object_file) if (-d $object_file);
    }

    ($object_file) = $object_file =~ /^(.*)/s if taint_is_on;

    my $fh = make_fh();
    open $fh, ">$object_file"
	or system_error "Couldn't write object file $object_file: $!";
    print $fh $object_code
	or system_error "Couldn't write object file $object_file: $!";
    close $fh 
	or system_error "Couldn't close object file $object_file: $!";
    @$files_written = @newfiles if (defined($files_written))
}

sub _compilation_error {
    my ($self, $filename, $err) = @_;

    HTML::Mason::Exception::Compilation->throw(error=>$err, filename=>$filename);
}


sub object_file {
    my ($self, $comp) = @_;
    return $comp->persistent ?
	$self->comp_id_to_objfile($comp->comp_id) :
	undef;
}

# Generate HTML that describes Interp's current status.
# This is used in things like Apache::Status reports.  Currently shows:
# -- Interp properties
# -- loaded (cached) components
#
# Note that Apache::Status has an extremely narrow URL API, and I
# think the only way to pass info to another request is through
# PATH_INFO.  That's why the expiration stuff is awkward.
sub status_as_html {
    my ($self, %p) = @_;

    # Should I be scared about this?  =)

    my $comp_source = <<'EOF';
<h3>Interpreter properties:</h3>
<blockquote>
 <h4>Startup options:</h4>
 <tt>
<table width="100%">
<%perl>
foreach my $property (sort keys %$interp) {
    my $val = $interp->{$property};

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

 <h4>Components in memory cache:</h4>
 <tt>
% my $cache;
% if ($cache = $interp->code_cache and %$cache) {
%   foreach my $key (sort keys %$cache) {
      <% $key |h%> (modified <% scalar localtime $cache->{$key}->{lastmod} %>)
%     if (my $cu = $current_url) {
%       $cu =~ s,\?,/expire_code_cache=$key?,;
        <a href="<% $cu %>"><i>expire</i></a>
%     }
      <br>
%   }
% } else {
    <I>None</I>
% }
  </tt>
</blockquote>

<%args>
 $interp   # The interpreter we'll elucidate
 %valid    # Default values for interp member data
 $current_url => ''
</%args>
EOF

    my $current_url = '';
    if (my $r = eval {Apache->request}) {
	my $path_info = quotemeta $r->path_info;
	($current_url = $r->uri) =~ s/$path_info$//;
	$current_url .= '?' . $r->args;
    }

    my $comp = $self->make_component(comp_source => $comp_source);
    my $out;
    local $self->{out_method} = \$out;

    my $args = {interp => $self, valid => $self->validation_spec, current_url => $current_url};
    $self->make_request(comp=>$comp, args=>$args, %p)->exec;

    return $out;
}


1;

__END__

=head1 NAME

HTML::Mason::Interp - Mason Component Interpreter

=head1 SYNOPSIS

    my $i = new HTML::Mason::Interp (data_dir=>'/usr/local/mason',
                                     comp_root=>'/usr/local/www/htdocs/',
                                     ...other params...);

=head1 DESCRIPTION

Interp is the Mason workhorse, executing components and routing their
output and errors to all the right places. In a mod_perl environment,
Interp objects are handed off immediately to an ApacheHandler object
which internally calls the Interp implementation methods. In that case
the only user method is the new() constructor.

If you want to call components outside of mod_perl (e.g. from CGI or a
stand-alone Perl script), see the L<STANDALONE MODE> section below.

=head1 PARAMETERS FOR new() CONSTRUCTOR

=over

=item autohandler_name

File name used for autohandlers. Default is "autohandler".

=item autoflush

This parameter indicates whether or not requests created by this
interpreter should have autoflush turned on or off by default.

=item code_cache_max_size

Specifies the maximum size, in bytes, of the in-memory code cache
where components are stored. e.g.

    code_cache_max_size => 20*1024*1024
    code_cache_max_size => 20_000_000

Default is 10 MB. See the L<Admin/Code Cache> section of the I<Admin Guide>
for further details.

=item comp_root

The required Mason component root. All components live under the comp_root.

You may also specify multiple component roots to be searched in the
spirit of Perl's @INC. To do so you must specify a list of lists:

    comp_root => [[key1, root1], [key2, root2], ...]

Each pair consists of a key and root.  The key is a string that
identifies the root mnemonically to a component developer.  Data cache
and object directories are split up by these keys to make sure
different components sharing the same path have different cache and
object files. The key is also included whenever Mason prints the
component title, as in an error message.

For example:

    comp_root => [['private', '/usr/home/joe/comps'],
                  ['main', '/usr/local/www/htdocs']]

This specifies two component roots, a main component tree and a
private tree which overrides certain components.  The order is
respected ala @INC, so 'private' is searched first and 'main' second.

=item compiler

Compiler object for compiling components on the fly.  If none is
provided a default compiler using the
C<HTML::Mason::Compiler::ToObject> and C<HTML::Mason::Lexer> classes
will be created.

=item data_dir

The Mason data directory. Mason's various data directories (obj,
cache, etc), live within the data_dir.

If this parameter is not given then there are several results.  First,
Mason will not use object files, since it has no place to put them.
Second, Mason will not be able to create a system log unless an
explicit system log file name is given.  Third, the default caching
class for the request object will be Cache::MemoryCache instead of
Cache::FileCache.

=item data_cache_defaults

A hash reference of default options to use for the C<$m-E<gt>cache>
command. For example, to use the Cache::MemoryCache implementation
by default,

    data_cache_defaults => {cache_class => 'MemoryCache'}

These settings are overriden by options given to particular
C<$m-E<gt>cache> calls.

=item dhandler_name

File name used for dhandlers. Default is "dhandler".

=item ignore_warnings_expr

Regular expression indicating which warnings to ignore when compiling
components. Any warning that is not ignored will prevent the
component from being compiled and executed. For example:

    ignore_warnings_expr =>
        'Global symbol.*requires explicit package'

If undef, all warnings are heeded; if '.', all warnings are ignored.

By default, this is set to 'Subroutine .* redefined'.  This allows you
to declare global subroutines inside <%once> sections and not receive
an error when the component is reloaded.

=item max_recurse

The maximum component stack depth the interpreter is allowed to
descend before signalling an error.  Default is 32.

=item out_method

Indicates where to send output. If out_method is a reference to a
scalar, output is appended to the scalar.  If out_method is a
reference to a subroutine, the subroutine is called with each output
string. For example, to send output to a file called "mason.out":

    my $fh = new IO::File ">mason.out";
    ...
    out_method => sub { $fh->print($_[0]) }

By default, out_method prints to standard output.  When the
HTML::Mason::ApacheHandler module is used, the out method uses the C<<
$r->print >> method to send output.

=item preloads

A list of component paths, optionally with glob wildcards, to load
when the interpreter initializes. e.g.

    preloads => ['/foo/index.html','/bar/*.pl']

Default is the empty list.  For maximum performance, this should only
be used for components that are frequently viewed and rarely updated.
See the L<Admin/preloading> section of the I<Admin Guide> for further
details.

=item system_log_events

A string value indicating one or more events to record in the system
log, separated by "|".  The following events are available for logging:

=over 4

=item * COMP_LOAD

The loading of a component object file from disk

=item * CACHE_READ

The reading of a cache item

=item * CACHE_WRITE

The writing of a cache item

=item * REQ_START

The start of a Mason request

=item * REQ_END

The end of a Mason request

=item * REQUEST

Shorthand for C<REQ_START|REQ_END>

=item * CACHE

Shorthand for C<CACHE_READ|CACHE_WRITE>

=item * ALL

Shorthand for C<REQUEST|CACHE|COMP_LOAD>

=back

Default is to log nothing.

=item system_log_file

Absolute path of system log.  Default is $data_dir/etc/system.log .

=item system_log_separator

Separator to use between fields on a line in the system log. Default is ctrl-A (C<"\cA">).

=item use_object_files

True or false, default is true.  Specifies whether Mason creates
object files to save the results of component parsing. You may want to
turn off object files for disk space reasons, but otherwise this
should be left alone.

=back

=head1 ACCESSOR METHODS

All of the above properties have standard accessor methods of the same
name. In general, no arguments retrieves the value, and one argument
sets and returns the value.  For example:

    my $interp = new HTML::Mason::Interp (...);
    my $c = $interp->compiler;
    my $comproot = $interp->comp_root;
    $interp->out_method(\$buf);

The following properties can be queried but not modified:
comp_root, data_dir, system_log_file, system_log_separator, preloads.

=head1 OTHER METHODS

=over

=for html <a name="item_set_global">

=item set_global ($varname, [values...])

This method sets a global to be used in components. C<varname> is a
variable name, optionally preceded with a prefix (C<$>, C<@>, or
C<%>); if the prefix is omitted then C<$> is assumed. C<varname> is
followed by a value, in the case of a scalar, or by one or more values
in the case of a list or hash.  For example:

    # Set a global variable $dbh containing the database handle
    $interp->set_global(dbh => DBI->connect(...));

    # Set a global hash %session from a local hash
    $interp->set_global('%session', %s);

The global is set in the package that components run in: usually
C<HTML::Mason::Commands>, although this can be overridden via the
Compiler parameter L<Compiler/in_package>.  The lines above, for
example, are equivalent to:

    $HTML::Mason::Commands::dbh = DBI->connect(...);
    %HTML::Mason::Commands::session = %s;

assuming that C<in_package> has not been changed.

Any global that you set should also be registered with the Compiler
parameter L<Compiler/allow_globals>; otherwise you'll get warnings
from C<strict>.

=for html <a name="item_comp_exists">

=item comp_exists (path)

Given an I<absolute> component path, this method returns a boolean
value indicating whether or not a component exists for that path.

=for html <a name="item_make_component">

=item make_component (comp_source=>... [, path=>...])

=item make_component (comp_file=>... [, path=>...])

This method compiles Mason component source code and returns a
Component object.  The source may be passed in as a string in C<comp_source>,
or as a filename in C<comp_file>.  When using C<comp_file>, the
filename is specified as a path on the file system, not as a path
relative to Mason's component root (see 
L<HTML::Mason::Request/fetch_comp> for that).

If you pass a C<path> parameter, the new component will be 'public',
and callable from other components via the specified path.  Otherwise
the component will be anonymous, and the only way to access the
component will be through the returned component object.

If Mason encounters an error during processing, an exception will be thrown.

Example of usage:

    # Make an anonymous component
    my $anon_comp =
      eval { $interp->make_component
               ( comp_source => '<%perl>my $name = "World";</%perl>Hello <% $name %>!' ) };
    die $@ if $@;

    $m->comp($anon_comp);

=back

=head1 STANDALONE MODE

Although Mason is most commonly used in conjunction with mod_perl,
there is also a functional API that allows you to use Mason from CGI
programs or from stand-alone Perl scripts.

When using Mason outside of mod_perl, just create an Interp object;
you do not need the ApacheHandler object.  Once you've created an
interpreter, the main thing you'll want to do with it is call a
component and do something with the output. To call a component, use
Interp's exec() method:

    $interp->exec(<comp> [,<..list of component params..>]);

where I<comp> is a component path or component object.

Component parameters are given as a series of name/value pairs, just
as they are with C<$m-E<gt>comp>. exec returns the return value of
the component. Component output is sent to standard output by default,
but you can change this by specifying C<out_method>.

=head2 Using Mason from a CGI script

Here is a skeleton CGI script that calls a component and sends the
output to the browser.

    #!/usr/bin/perl -w
    use CGI;
    use HTML::Mason;
    use HTML::Mason::Utils qw(cgi_request_args);

    my $interp = new HTML::Mason::Interp (comp_root=>'...',
                                          data_dir=>'...');

    my $q = new CGI;
    my $comp = $ENV{'PATH_TRANSLATED'};
    my $root = $interp->comp_root;
    $comp =~ s/^$root//
        or die "Requested file '$comp' is outside component root '$root'";
    my %args = cgi_request_args($q, $q->request_method);
    print $q->header;

    $interp->exec($comp, %args);

The relevant portions of the httpd.conf file look like:

    DocumentRoot /path/to/comp/root
    ScriptAlias /cgi-bin/ /path/to/cgi-bin/
    Action html-mason /cgi-bin/handler.cgi

    <Directory /path/to/comp/root>
    SetHandler html-mason
    </Directory>

This simply causes Apache to call my handler.cgi script every time a
file under the component root is requested.

It should be noted that because this script simply prints standard
HTTP headers and then executes a component, there would be no way of
sending non-OK responses to the browser from your components.

A more complex handler.cgi script might look like this:

    #!/usr/bin/perl -w
    use CGI;
    use HTML::Mason;
    use HTML::Mason::Compiler::ToObject;
    use HTML::Mason::Utils qw(cgi_request_args);
    use HTTP::Headers;

    my $compiler = new HTML::Mason::Compiler::ToObject (allow_globals => '$H');
    my $interp = new HTML::Mason::Interp (comp_root=>'...',
                                          data_dir=>'...',
                                          compiler=>$compiler);

    my $q = new CGI;
    my $comp = $ENV{'PATH_TRANSLATED'};
    my $root = $interp->comp_root;
    $comp =~ s/^$root//
        or die "Requested file '$comp' is outside component root '$root'";
    my %args = cgi_request_args($q, $q->request_method);

    # Gather all component output into a buffer.
    my $buffer;
    $interp->out_method(\$buffer);

    my $H = new HTTP::Headers;
    $interp->set_global('$H'=>$H);

    eval { $interp->exec($comp, %args) };

    if ($@) {
        handle_error($@);
    } else {
        unless ($H->header('content-type')) {
            $H->header('content-type' => 'text/html');
        }
        print $H->as_string;
        print $buffer;
    }

This script offers components the use of a global named C<$h> which
can be used to set headers (for example, for a redirect).  It also
catches errors in the execution of the script and has a routine to
handle them.

=head2 Using Mason from a standalone script

Here is a skeleton script that calls a component and places the output
in a file:

    my $outbuf;
    my $interp = new HTML::Mason::Interp (comp_root=>'<component root>',
					  data_dir=>'<data directory>',
					  out_method=>\$outbuf);
    my $retval = $interp->exec('<component path>', <args>...);
    open(F,">mason.out");
    print F $outbuf;
    close(F);
    print "return value of component was: $retval\n";

This allows you to use Mason as a pure text templating solution --
like Text::Template and its brethren, but with more power (and of
course more complexity).

=head1 SEE ALSO

L<HTML::Mason>,
L<HTML::Mason::Admin>,
L<HTML::Mason::ApacheHandler>

=cut
