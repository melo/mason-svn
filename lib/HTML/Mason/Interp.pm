# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Interp;

use strict;

use Carp;
use File::Basename;
use File::Path;
use File::Spec;
use HTML::Mason::Config;
use HTML::Mason::Request;
use HTML::Mason::Resolver::File;
use HTML::Mason::Tools qw(make_fh read_file taint_is_on);
use Params::Validate qw(:all);
Params::Validate::set_options( on_fail => sub { HTML::Mason::Exception::Params->throw( error => join '', @_ ) } );

require Time::HiRes if $HTML::Mason::Config{use_time_hires};

use HTML::Mason::MethodMaker
    ( read_only => [ qw( code_cache
			 comp_root
			 data_dir
			 data_cache_dir
			 die_handler
			 die_handler_overridden
			 hooks
			 system_log_file
			 system_log_separator
			 preloads ) ],

      read_write => [ qw( allow_recursive_autohandlers
			  autohandler_name
			  code_cache_max_size
			  compiler
			  data_cache_defaults
			  dhandler_name
		          ignore_warnings_expr
			  max_recurse
			  out_mode
			  resolver
			  static_file_root
			  use_object_files
			  use_reload_file ) ],
      );

# Fields that can be set in new method, with defaults
my %fields =
    (
     allow_recursive_autohandlers => 1,
     autohandler_name => 'autohandler',
     code_cache_max_size => 10*1024*1024,
     compiler => undef,
     comp_root => undef,
     current_time => 'real',
     data_dir => undef,
     data_cache_defaults => undef,
     dhandler_name => 'dhandler',
     die_handler => sub { Carp::confess($_[0]) },
     die_handler_overridden => 0,
     ignore_warnings_expr => 'Subroutine .* redefined',
     system_log_file => undef,
     system_log_separator => "\cA",
     max_recurse => 32,
     out_mode => 'batch',
     preloads => [],
     resolver => undef,
     static_file_root => undef,
     use_autohandlers => 1,
     use_data_cache => 1,
     use_dhandlers => 1,
     use_object_files => 1,
     use_reload_file => 0
     );

sub new
{
    my $class = shift;
    my $self = {
	%fields,
        code_cache => {},
        code_cache_current_size => 0,
	files_written => [],
	hooks => {},
	last_reload_time => 0,
	last_reload_file_pos => 0,
	out_method => sub { for (@_) { print $_ if defined } },
	system_log_fh => undef,
	system_log_events_hash => undef
    };

    validate( @_,
	      { allow_recursive_autohandlers => { type => SCALAR | UNDEF, optional => 1 },
		autohandler_name => { type => SCALAR | UNDEF, optional => 1 },
		code_cache_max_size => { type => SCALAR, optional => 1 },
		current_time => { type => SCALAR, optional => 1 },
		data_cache_dir => { type => SCALAR, optional => 1 },
		dhandler_name => { type => SCALAR | UNDEF, optional => 1 },
		die_handler => { type => CODEREF | SCALAR | UNDEF, optional => 1 },
		out_method => { type => CODEREF | SCALARREF, optional => 1 },
		out_mode => { type => SCALAR, optional => 1 },
		max_recurse => { type => SCALAR, optional => 1 },
		compiler => { isa => 'HTML::Mason::Compiler', optional => 1 },
		preloads => { type => ARRAYREF, optional => 1 },
		static_file_root => { type => SCALAR, optional => 1 },
		system_log_events => { type => SCALAR | UNDEF, optional => 1 },
		system_log_file => { type => SCALAR, optional => 1 },
		system_log_separator => { type => SCALAR, optional => 1 },
		use_autohandlers => { type => SCALAR | UNDEF, optional => 1 },
		use_data_cache => { type => SCALAR | UNDEF, optional => 1 },
		use_dhandlers => { type => SCALAR | UNDEF, optional => 1 },
		use_object_files => { type => SCALAR | UNDEF, optional => 1 },
		use_reload_file => { type => SCALAR | UNDEF, optional => 1 },

		comp_root => { type => SCALAR | ARRAYREF },
		data_dir => { type => SCALAR },
	      }
	    );

    my (%options) = @_;

    HTML::Mason::Exception::Params->throw( error => "out_mode parameter must be either 'batch' or 'stream'\n" )
	if defined $options{out_mode} && $options{out_mode} ne 'batch' && $options{out_mode} ne 'stream';

    while (my ($key,$value) = each(%options)) {
	next if $key =~ /out_method|system_log_events/;
	$self->{$key} = $value;
    }
    $self->{autohandler_name} = undef unless $self->{use_autohandlers};
    $self->{dhandler_name} = undef unless $self->{use_dhandlers};
    $self->{die_handler_overridden} = 1 if exists $options{die_handler};

    $self->{data_cache_dir} ||= ($self->{data_dir} . "/cache");
    bless $self, $class;

    $self->out_method($options{out_method}) if (exists($options{out_method}));
    $self->system_log_events($options{system_log_events}) if (exists($options{system_log_events}));
    $self->_initialize;
    return $self;
}

sub _initialize
{
    my ($self) = shift;
    $self->{code_cache} = {};
    $self->{code_cache_current_size} = 0;
    $self->{data_cache_defaults} = {};

    unless ($self->{compiler}) {
	require HTML::Mason::Compiler::ToObject;
	require HTML::Mason::Lexer;
	$self->compiler( HTML::Mason::Compiler::ToObject->new( lexer_class => 'HTML::Mason::Lexer' ) );
    }

    #
    # Create resolver if not provided
    #
    unless ($self->{resolver}) {
	HTML::Mason::Exception::Params->throw( error => "must specify value for comp_root\n" )
	    if !$self->{comp_root};
	$self->{resolver} = HTML::Mason::Resolver::File->new($self);
    }

    #
    # Check that directories are absolute.
    #
    foreach my $field (qw(comp_root data_dir)) {
	next if $field eq 'comp_root' and ref($self->{$field}) eq 'ARRAY';
	$self->{$field} = File::Spec->canonpath( $self->{$field} );
 	HTML::Mason::Exception::Params->throw( error => "$field ('".$self->{$field}."') must be an absolute directory" )
	    unless File::Spec->file_name_is_absolute( $self->{$field} );
    }

    #
    # If comp_root has multiple dirs, confirm format.
    #
    if (ref($self->comp_root) eq 'ARRAY') {
	foreach my $pair (@{$self->comp_root}) {
	    HTML::Mason::Exception::Params->throw( error => "Multiple-path component root must consist of a list of two-element lists; see documentation" )
		if ref($pair) ne 'ARRAY';
	    $pair->[1] = File::Spec->canonpath( $pair->[1] );
	    HTML::Mason::Exception::Params->throw( error => "comp_root ('$pair->[0]') must contain only absolute directories" )
		unless File::Spec->file_name_is_absolute( $pair->[1] );
	}
    }

    #
    # Create data subdirectories if necessary. mkpath will die on error.
    #
    foreach my $subdir (qw(obj cache cache/locks etc)) {
	my @newdirs = mkpath($self->data_dir."/$subdir",0,0775);
	$self->push_files_written(@newdirs);
    }

    #
    # Open system log file
    #
    if ($self->{system_log_events_hash}) {
	$self->{system_log_file} = File::Spec->catfile( $self->data_dir, 'etc', 'system.log' ) if !$self->system_log_file;
	my $fh = make_fh();
	open $fh, ">>".$self->system_log_file
	    or HTML::Mason::Exception::System->throw( error => "Couldn't open system log file $self->{system_log_file} for append" );
	my $oldfh = select $fh;
	$| = 1;
	select $oldfh;
	$self->{system_log_fh} = $fh;
    }
    
    #
    # Preloads
    #
    if ($self->preloads) {
	HTML::Mason::Exception::Exception->throw( error => "array reference expected for preloads parameter" )
	    unless UNIVERSAL::isa($self->preloads, 'ARRAY');
	foreach my $pattern (@{$self->preloads}) {
	    HTML::Mason::Exception->throw( error => "preloads pattern must be an absolute path" )
		unless substr($pattern,0,1) eq '/';
	    my @paths = $self->resolver->glob_path($pattern);
	    foreach (@paths) { $self->load($_) }
	}
    }

    #
    # Adjust to current size of reload file
    #
    if ($self->use_reload_file && -f $self->reload_file) {
	$self->{last_reload_file_pos} = (stat(_))[7];
	$self->{last_reload_time} = (stat(_))[9];
    }
}

#
# Shorthand for various data subdirectories and files.
#
sub object_dir { return File::Spec->catdir( shift->data_dir, 'obj' ); }
sub reload_file { return File::Spec->catfile( shift->data_dir, 'etc', 'reload.lst' ); }

#
# exec is the initial entry point for executing a component
# in a new request.
#
sub exec {
    my $self = shift;
    my $req = HTML::Mason::Request->new(interp=>$self);
    $req->exec(@_);
}

#
# Check if reload file has changed. If so, read paths from last read
# position to end of file and delete those paths from the cache.
#
sub check_reload_file {
    my ($self) = @_;
    my $reload_file = $self->reload_file;
    return if (!-f $reload_file);
    my $lastmod = (stat(_))[9];
    if ($lastmod > $self->{last_reload_time}) {
	my $length = (stat(_))[7];
	$self->{last_reload_file_pos} = 0 if ($length < $self->{last_reload_file_pos});
	my $fh = make_fh();
	open $fh, $reload_file or return;

	my $block;
	my $pos = $self->{last_reload_file_pos};
	seek ($fh,$pos,0);
	read($fh,$block,$length-$pos);
	$self->{last_reload_time} = $lastmod;
	$self->{last_reload_file_pos} = tell $fh;
	my @lines = split("\n",$block);
	foreach my $comp_path (@lines) {
	    if (exists($self->{code_cache}->{$comp_path})) {
		$self->{code_cache_current_size} -= $self->{code_cache}->{$comp_path}->{comp}->object_size;
		delete($self->{code_cache}->{$comp_path});
	    }
	}
	close $fh;
    }
}

#
# Return the absolute version of a component path. Handles . and ..
# Second argument is directory path to resolve relative paths against.
#
sub process_comp_path
{
    my ($self,$comp_path,$dir_path) = @_;

    $comp_path = "$dir_path/$comp_path" if $comp_path !~ m@^/@;
    return 'HTML::Mason::Tools'->mason_canonpath($comp_path);
}

#
# Look up <$path> as a component path. Return fully qualified path or
# or undef if it does not exist.
# 
sub lookup {
    my ($self,$path) = @_;
    my %info = $self->resolver->resolve($path);
    return $info{path};
}

#
# Load <$path> into a component, possibly parsing the source and/or
# caching the code. Returns a component object or undef if the
# component was not found.
#
sub load {
    my ($self,$path) = @_;
    my ($err,$maxfilemod,$objfile,$objfilemod);
    my (@objstat, $objisfile);
    my $code_cache = $self->{code_cache};
    my $resolver = $self->{resolver};

    #
    # If using reload file, assume that we are using object files and
    # have a cached subroutine or object file.
    #
    if ($self->{use_reload_file}) {
	my $fq_path = $path;   # note - this will foil multiple component roots
	return $code_cache->{$fq_path}->{comp} if exists($code_cache->{$fq_path});

	$objfile = File::Spec->catfile( $self->object_dir, $fq_path );
	return undef unless (-f $objfile);   # component not found
	
	$self->write_system_log('COMP_LOAD', $fq_path);	# log the load event
	my $comp = $self->eval_object_text(object=>$objfile, error=>\$err)
	    or $self->_compilation_error($objfile, $err);
	$comp->assign_runtime_properties($self,$fq_path);
	
	$code_cache->{$fq_path}->{comp} = $comp;
	return $comp;
    }

    #
    # Use resolver to look up component and get fully-qualified path.
    # Return undef if component not found.
    #
    my %lookup_info = $resolver->resolve($path);
    my $fq_path = $lookup_info{path} or return undef;

    #
    # Get last modified time of source.
    #
    my $srcmod = $lookup_info{last_modified};
    
    if ($self->{use_object_files}) {
	$objfile = File::Spec->catfile( $self->object_dir, $fq_path );
	@objstat = stat $objfile;
	$objisfile = -f _;
    }
    
    #
    # If code cache contains an up to date entry for this path,
    # use the cached sub.
    #
    if (exists($code_cache->{$fq_path}) and $code_cache->{$fq_path}->{lastmod} >= $srcmod) {
	return $code_cache->{$fq_path}->{comp};
    } else {
	$objfilemod = (defined($objfile) and $objisfile) ? $objstat[9] : 0;
	#
	# Load the component from source or object file.
	#
	$self->write_system_log('COMP_LOAD', $fq_path);	# log the load event

	my $comp;
	if ($objfile) {
	    #
	    # We are using object files.  Update object file if necessary
	    # and load component from there.
	    #
	    update_object:
	    my $object;
	    #
	    # the encapsulation is breaking here.  I think the
	    # resolver needs to return this text. - dave
	    #
	    my $file = $resolver->get_component(%lookup_info);
	    if ($objfilemod < $srcmod) {
		$object = $self->compiler->compile( comp => $file, name => $lookup_info{description}, comp_class => $resolver->comp_class );
		$self->write_object_file(object_text=>$object, object_file=>$objfile);
	    }
	    # read the existing object file
	    $object ||= read_file($objfile);
	    $comp = $self->eval_object_text(object=>$object, error=>\$err);
	    if (!$comp) {
		# If this is an earlier version object file, replace it.
		if ($err =~ /object file was created by a pre-0\.7 parser/
		    or (($err =~ /object file was created by.*version ([\d\.]+) .* you are running parser version ([\d\.]+)\./) and $1 < $2)) {
		    $objfilemod = 0;
		    goto update_object;
		} else {
		    $self->_compilation_error( $lookup_info{description}, $err );
		}
	    }
	} else {
	    #
	    # No object files. Load component directly into memory.
	    #
	    my $file = $resolver->get_component(%lookup_info);
	    my $object = $self->compiler->compile( comp => $file, name => $lookup_info{description}, comp_class => $resolver->comp_class );
	    $comp = $self->eval_object_text(object=>$object, error=>\$err)
		or $self->_compilation_error( $lookup_info{description}, $err );
	}
	$comp->assign_runtime_properties($self,$fq_path);

	#
	# Cache code in memory, adjusting current cache size appropriately. Don't cache
	# elements that are too large.
	#
	$self->{code_cache_current_size} -= $code_cache->{$fq_path}->{comp}->object_size if (exists($code_cache->{$fq_path}));
	if ($comp->object_size <= $self->code_cache_max_elem) {
	    $code_cache->{$fq_path} = {lastmod=>$srcmod, comp=>$comp};
	    $self->{code_cache_current_size} += $comp->object_size;
	} else {
	    delete($code_cache->{$fq_path}) if (exists($code_cache->{$fq_path}));
	}
	return $comp;
    }
}

#
# If code cache has exceeded maximum, remove least frequently used
# elements from cache until size falls below minimum.
#
sub purge_code_cache {
    my ($self) = @_;

    if ($self->{code_cache_current_size} > $self->code_cache_max_size) {
	my $code_cache = $self->{code_cache};
	my $cur_size = $self->{code_cache_current_size};
	my $min_size = $self->code_cache_min_size;
	my $decay_factor = $self->code_cache_decay_factor;

	my @elems;
	while (my ($path,$href) = each(%{$code_cache})) {
	    push(@elems,[$path,$href->{comp}->mfu_count,$href->{comp}]);
	}
	@elems = sort { $a->[1] <=> $b->[1] } @elems;
	while (($cur_size > $min_size) and @elems) {
	    my $elem = shift(@elems);
	    $cur_size -= $elem->[2]->object_size;
	    delete($code_cache->{$elem->[0]});
	}
	$self->{code_cache_current_size} = $cur_size;

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
# Make an anonymous component and assign it to this interpreter.
#
sub make_component {
    my $self = shift;
    my %p = @_;

    my $err;

    my $object = $self->compiler->compile( comp => ${ $p{object_text} }, name => $p{script_file}, comp_class => $p{comp_class} );
    $self->write_object_file(object_text=>$object, object_file=>$p{script_file});

    my $comp = $self->eval_object_text(object=>$object, error=>\$err);
    $comp->assign_runtime_properties($self) if $comp;
    return $comp;
}

#
# Set or fetch the current time value.
#
sub current_time {
    my $self = shift;
    if (@_) {
	my $newtime = shift;
	HTML::Mason::Exception::Params->throw( error => "Interp->current_time: invalid value '$newtime' - must be 'real' or a numeric time value" )
	    if $newtime ne 'real' && $newtime !~ /^[0-9]+$/;
	$newtime = time if $newtime eq 'real';
	return $self->{current_time} = $newtime;
    } else {
	return $self->{current_time};
    }
}

sub set_global
{
    my ($self, $decl, @values) = @_;
    HTML::Mason::Exception::Params->throw( error => "Interp->set_global: expects a variable name and one or more values")
	unless @values;
    my ($prefix, $name) = ($decl =~ /^[\$@%]/) ? (substr($decl,0,1),substr($decl,1)) : ("\$",$decl);

    my $varname = sprintf("%s::%s",$self->compiler->in_package,$name);
    if ($prefix eq "\$") {
	no strict 'refs'; $$varname = $values[0];
    } elsif ($prefix eq "\@") {
	no strict 'refs'; @$varname = @values;
    } else {
	no strict 'refs'; %$varname = @values;
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
	    HTML::Mason::Exception::Params->throw( error => "Interp->system_log_events: argument must be a scalar or hash reference" );
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
    my ($self,$startpath,$name) = @_;

    my $comp;
    my $p = File::Spec->canonpath($startpath);

    my $last_p;
    while (!($comp = $self->load( File::Spec->catfile( $p, $name ) )) && $p) {
	my $last_p = $p;
	my ($basename,$dirname) = fileparse($p);
	$p = File::Spec->canonpath($dirname);    # certain versions leave ./ in $dirname
	last if $p eq $last_p;  # last dir was something like '/' and so is this one
    }
    return $comp;
}

#
# Hook functions.
#
my @hook_types = qw(start_comp end_comp start_file end_file);
my %hook_type_map = map(($_,1),@hook_types);

sub add_hook {
    my ($self, %args) = @_;
    foreach (qw(name type code)) {
	HTML::Mason::Exception::Params->throw( error => "Interp->add_hook: must specify $_\n" )
	    unless exists($args{$_});
    }
    HTML::Mason::Exception::Params->throw( error => "Interp->add_hook: type must be one of " . join(",",@hook_types)."\n" )
	unless $hook_type_map{$args{type}};
    HTML::Mason::Exception::Params->throw( error => "Interp->add_hook: code must be a code reference\n" )
	unless UNIVERSAL::isa( $args{code}, 'CODE' );
    $self->{hooks}->{$args{type}}->{$args{name}} = $args{code};
}

sub remove_hook {
    my ($self, %args) = @_;
    foreach (qw(name type)) {
	HTML::Mason::Exception::Params->throw( error => "Interp->remove_hook: must specify $_\n" )
	    unless exists($args{$_});
    }
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
	my $time = ($HTML::Mason::Config{use_time_hires} ? scalar(Time::HiRes::gettimeofday()) : time);
	my $fh = $self->{system_log_fh};
	print $fh (join ($self->system_log_separator,
			 $time,                  # current time
			 $_[0],                  # event name
			 $$,                     # pid
			 @_[1..$#_]              # event-specific fields
			),"\n")
	    or HTML::Mason::Exception::Params->throw( error => "Cannot write to system log: $!" );
    }
}

# Code cache parameter methods

sub code_cache_min_size { shift->code_cache_max_size * 0.75 }
sub code_cache_max_elem { shift->code_cache_max_size * 0.20 }
sub code_cache_decay_factor { 0.75 }


########################################
# Methods that used to be in Parser.pm.
# This is a temporary home only.
# They need to be moved again.
########################################

#
# eval_object_text
#   (object_text, object_file, error)
# Evaluate an object file or object text.  Return a component object
# or undef if error.
#
# I think this belongs in the resolver (or comp loader) - Dave
#
sub eval_object_text
{
    my ($self, %options) = @_;
    my ($object, $errref) = @options{qw(object error)};

    #
    # Evaluate object file or text with warnings on
    #
    my $ignore_expr = $self->ignore_warnings_expr;
    my ($comp,$err);
    {
	my $warnstr = '';
	local $^W = 1;
	local $SIG{__WARN__} = $ignore_expr ? sub { $warnstr .= $_[0] if $_[0] !~ /$ignore_expr/ } : sub { $warnstr .= $_[0] };

	# If in taint mode, untaint the object text
	($object) = ($object =~ /^(.*)/s) if taint_is_on;
	$comp = eval $object;

	$err = $warnstr . $@;

	#
	# If no error generated and no component object returned, we
	# have a prematurely-exited <%once> section or other syntax
	# accident.
	#
	unless (1 or $err or (defined($comp) and (UNIVERSAL::isa($comp, 'HTML::Mason::Component') or ref($comp) eq 'CODE'))) {
	    $err = "could not generate component object (return() in a <%once> section or extra close brace?)";
	}
    }

    #
    # I will overhaul this sometime soon - dave (I hope soon)
    #
    if ($self->{use_object_files}) {
	# check compatibility between lexer/compiler that wrote this
	# and lexer/compiler we're using now.
    }

    #
    # Return component or error
    #
    if ($err) {
	# attempt to stem very long eval errors
	if ($err =~ /has too many errors\./) {
	    $err =~ s/has too many errors\..*/has too many errors./s;
	}
	$$errref = $err if defined($errref);
	return undef;
    } else {
	return $comp;
    }
}

#
# write_object_file
#   (object_text=>..., object_file=>..., files_written=>...)
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
    my ($object_text,$object_file,$files_written) =
	@options{qw(object_text object_file files_written)};
    my @newfiles = ($object_file);

    if (defined $object_file && !-f $object_file) {
	my ($dirname) = dirname($object_file);
	if (!-d $dirname) {
	    unlink($dirname) if (-e $dirname);
	    push(@newfiles,mkpath($dirname,0,0775));
	    die "Couldn't create directory $dirname: $!" if (!-d $dirname);
	}
	rmtree($object_file) if (-d $object_file);
    }

    ($object_file) = $object_file =~ /^(.*)/s if taint_is_on;

    my $fh = make_fh();
    open $fh, ">$object_file" or die "Couldn't write object file $object_file: $!";
    print $fh $object_text;
    close $fh or die "Couldn't close object file $object_file: $!";
    @$files_written = @newfiles if (defined($files_written))
}

sub make_anonymous_component
{
    my $self = shift;
    my %p = @_;

    my $object = $self->compiler->compile(%p);

    my $error;
    my $comp = $self->eval_object_text( object => $object, error => \$error );
    $self->_compilation_error( '<<anonymous component>>', $error ) if $error;

    return $comp;
}

sub _compilation_error {
    my ($self, $filename, $err) = @_;

    my $msg = sprintf("Error during compilation of %s:\n%s\n",$filename, $err);
    HTML::Mason::Exception::Compilation->throw( error => $msg );
}


1;
