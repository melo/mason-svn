# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Request;

use strict;

use File::Spec;
use HTML::Mason::Tools qw(read_file compress_path load_pkg absolute_comp_path);
use HTML::Mason::Utils;
use HTML::Mason::Buffer;

use Class::Container;
use base qw(Class::Container);

use HTML::Mason::Exceptions( abbr => [qw(param_error syntax_error abort_error
					 top_level_not_found_error error)] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );

BEGIN
{
    __PACKAGE__->valid_params
	(
	 args  => { type => ARRAYREF, default => [],
		    descr => "Array of arguments to initial component",
		    public => 0 },
	 autoflush  => { parse => 'boolean', default => 0, type => SCALAR,
			 descr => "Whether output should be buffered or sent immediately" },
	 comp  => { type => SCALAR | HASHREF, optional => 0,
		    descr => "Initial component, either an absolute path or a component object",
		    public => 0 },
	 data_cache_defaults => { type => HASHREF|UNDEF, optional => 1,
				  descr => "A hash of default parameters for Cache::Cache" },
	 declined_comps => { type => HASHREF, optional=>1,
			     descr => "Hash of components that have been declined in previous parent requests",
			     public => 0 },
	 dhandler_name => { parse => 'string',  default => 'dhandler', type => SCALAR,
			    descr => "The filename to use for Mason's 'dhandler' capability" },
	 interp     => { isa => 'HTML::Mason::Interp',
			 descr => "An interpreter for Mason control functions",
			 public => 0 },
	 error_format => { parse => 'string', type => SCALAR, default => 'text',
			   callbacks => { "must be one of 'brief', 'text', 'line', or 'html'" =>
					  sub { HTML::Mason::Exception->can("as_$_[0]"); } },
			   descr => "How error conditions are returned to the caller (brief, text, line or html)" },
	 error_mode => { parse => 'string', type => SCALAR, default => 'fatal',
			 callbacks => { "must be one of 'output' or 'fatal'" =>
					sub { $_[0] =~ /^(?:output|fatal)$/ } },
			 descr => "How error conditions are manifest (output or fatal)" },
	 max_recurse => { parse => 'string',  default => 32, type => SCALAR,
			  descr => "The maximum recursion depth for component, inheritance, and request stack" },
	 out_method => { parse => 'code',    type => CODEREF|SCALARREF,
			 default => sub { print STDOUT grep {defined} @_ },
			 descr => "A subroutine or scalar reference through which all output will pass" },
    );

    __PACKAGE__->contained_objects
	(
	 buffer     => { class => 'HTML::Mason::Buffer',
			 delayed => 1,
			 descr => "This class receives component output and dispatches it appropriately" },
	);
}

my @read_write_params;
BEGIN { @read_write_params = qw( autoflush
				 data_cache_defaults
				 dhandler_name
				 error_format
				 error_mode
                                 max_recurse
                                 out_method ); }
use HTML::Mason::MethodMaker
    ( read_only => [ qw( count
			 dhandler_arg
			 interp
			 parent_request
			 request_depth
			 request_comp ) ],

      read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
                      @read_write_params ]
    );

sub _properties { @read_write_params }

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);

    %$self = (%$self, buffer_stack => undef,
		      count => 0,
		      dhandler_arg => undef,
	              execd => 0,
		      parent_request => undef,
		      request_depth => 0,
		      stack => undef,
		      wrapper_chain => undef,
		      wrapper_index => undef,
	     );

    $self->{request_comp} = delete($self->{comp});
    $self->{request_args} = delete($self->{args});
    if (UNIVERSAL::isa($self->{request_args}, 'HASH')) {
	$self->{request_args} = [%{$self->{request_args}}];
    }
    $self->{count} = ++$self->{interp}{request_count};
    $self->_initialize;
    return $self;
}

# in the future this method may do something completely different but
# for now this works just fine.
sub instance {
    return $HTML::Mason::Commands::m;
}

sub _initialize {
    my ($self) = @_;
    my $interp = $self->interp;

    # All errors returned from this routine will be converted to a
    # Mason exception and placed in the {prepare_error} slot.  exec()
    # will then trigger the error. This makes for an easier new + exec
    # API.
    local $SIG{'__DIE__'} = sub {
	my $err = $_[0];
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    };

    eval {
	# create base buffer
	$self->{buffer_stack} = [];
	$self->{stack} = [];

	# request_comp can be an absolute path or component object.  If a path,
	# load into object.
	my $request_comp = $self->{request_comp};
	my ($path);
	if (!ref($request_comp)) {
	    $request_comp =~ s{/+}{/}g;
	    $self->{top_path} = $path = $request_comp;

	    search: {
		$request_comp = $self->interp->load($path);

		# If path was not found, check for dhandler.
		unless ($request_comp) {
		    if ( $request_comp = $interp->find_comp_upwards($path, $self->dhandler_name) ) {
			my $parent_path = $request_comp->dir_path;
			($self->{dhandler_arg} = $self->{top_path}) =~ s{^$parent_path/?}{};
		    }
		}

		# If the component was declined previously in this request,
		# look for the next dhandler up the tree.
		if ($request_comp and $self->{declined_comps}->{$request_comp->comp_id}) {
		    $path = $request_comp->dir_path;
		    unless ($path eq '/' and $request_comp->name eq $self->dhandler_name) {
			if ($request_comp->name eq $self->dhandler_name) {
			    $path =~ s/\/[^\/]+$//;
			}
		    }
		    redo search;
		}
	    }

	    unless ($self->{request_comp} = $request_comp) {
		top_level_not_found_error "could not find component for initial path '$self->{top_path}'\n";
	    }

	} elsif ( ! UNIVERSAL::isa( $request_comp, 'HTML::Mason::Component' ) ) {
	    param_error "comp ($request_comp) must be a component path or a component object";
	}
    };
    $self->{prepare_error} = $@ if $@;
}

sub exec {
    my ($self) = @_;
    my $interp = $self->interp;

    # Cheap way to prevent users from executing the same request twice.
    if ($self->{execd}++) {
	die "Can only call exec() once for a given request object. Did you want to use a subrequest?";
    }

    # All errors returned from this routine will be in exception form.
    local $SIG{'__DIE__'} = sub {
	my $err = $_[0];
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    };

    #
    # $m is a dynamically scoped global containing this
    # request. This needs to be defined in the HTML::Mason::Commands
    # package, as well as the component package if that is different.
    #
    local $HTML::Mason::Commands::m = $self;
    $interp->set_global('m'=>$self)
        if ($interp->compiler->in_package ne 'HTML::Mason::Commands');

    my @result;
    eval {
	# Create initial buffer.
	my $buffer = $self->create_delayed_object( 'buffer', sink => $self->out_method );
	push @{ $self->{buffer_stack} }, $buffer;

	# If there was an error during request preparation, throw it now.
	if (my $err = $self->{prepare_error}) {
	    $err->throw;
	}

	# Build wrapper chain and index.
	my $request_comp = $self->request_comp;
        my @request_args = $self->request_args;
	my $first_comp;
	{
	    my @wrapper_chain = ($request_comp);

	    for (my $parent = $request_comp->parent; $parent; $parent = $parent->parent) {
		unshift(@wrapper_chain,$parent);
		error "inheritance chain length > " . $self->max_recurse . " (infinite inheritance loop?)"
		    if (@wrapper_chain > $self->max_recurse);
	    }

	    $first_comp = $wrapper_chain[0];
	    $self->{wrapper_chain} = [@wrapper_chain];
	    $self->{wrapper_index} = {map((($wrapper_chain[$_]->path || '') => $_),(0..$#wrapper_chain))};
	}

	{
	    local *SELECTED;
	    tie *SELECTED, 'Tie::Handle::Mason', $self;

	    my $old = select SELECTED;
	    if (wantarray) {
		@result = eval {$self->comp({base_comp=>$request_comp}, $first_comp, @request_args)};
	    } else {
		$result[0] = eval {$self->comp({base_comp=>$request_comp}, $first_comp, @request_args)};
	    }
	    select STDOUT;
	    die $@ if $@;
	}
    };

    # Handle errors.
    my $err = $@;
    if ($err and !$self->aborted($err)) {
	pop @{ $self->{buffer_stack} };
	$interp->purge_code_cache;
	$self->_handle_error($err);
	return;
    }

    # Flush output buffer.
    $self->flush_buffer;
    pop @{ $self->{buffer_stack} };

    # Purge code cache if necessary. We do this at the end so as not
    # to affect the response of the request as much.
    $interp->purge_code_cache;

    # Return aborted value or result.
    return ($self->aborted($err) ? $err->aborted_value : (wantarray ? @result : $result[0]));
}

#
# Display or die with error as dictated by error_mode and error_format.
#
sub _handle_error
{
    my ($self, $err) = @_;

    # Set error format for when error is stringified.
    if (UNIVERSAL::can($err, 'format')) {
	$err->format($self->error_format);
    }

    # In fatal mode, die with error. In display mode, output stringified error.
    if ($self->error_mode eq 'fatal') {
	die $err;
    } else {
	UNIVERSAL::isa( $self->out_method, 'CODE' ) ? $self->out_method->("$err") : ( ${ $self->out_method } = "$err" );
    }
}

sub subexec
{
    my ($self, $comp, @args) = @_;

    $self->make_subrequest(comp=>$comp, args=>\@args)->exec;
}

sub make_subrequest
{
    my ($self, %params) = @_;
    my $interp = $self->interp;

    # Give subrequest the same values as parent request for read/write params
    my %defaults = map { ($_, $self->$_()) } $self->_properties;

    unless ( $params{out_method} )
    {
	$defaults{out_method} = sub { $self->top_buffer->receive(@_) };
    }

    # Make subrequest, and set parent_request and request_depth appropriately.
    my $subreq = $interp->make_request(%defaults, %params);
    $subreq->{parent_request} = $self;
    $subreq->{request_depth}  = $self->request_depth+1;
    error "subrequest depth > " . $self->max_recurse . " (infinite subrequest loop?)"
	if $subreq->request_depth > $self->max_recurse;

    return $subreq;
}

sub is_subrequest
{
    my ($self) = @_;

    return $self->parent_request ? 1 : 0;
}

#
# Abort out of current execution.
#
sub abort
{
    my ($self, $aborted_value) = @_;
    HTML::Mason::Exception::Abort->throw( error => 'Request->abort was called', aborted_value => $aborted_value );
}

#
# Determine whether $err (or $@ by default) is an Abort exception.
#
sub aborted {
    my ($self, $err) = @_;
    $err = $@ if !defined($err);
    return isa_mason_exception( $err, 'Abort' );
}

#
# Return a new cache object specific to this component.
#
sub cache
{
    my ($self, %options) = @_;

    if ($self->data_cache_defaults) {
	%options = (%{$self->data_cache_defaults}, %options);
    }
    $options{namespace}   ||= compress_path($self->current_comp->comp_id);
    $options{cache_root}  ||= $self->interp->cache_dir;
    $options{username}      = "mason";

    my $cache_class = $self->interp->cache_dir ? 'Cache::FileCache' : 'Cache::MemoryCache';
    if ($options{cache_class}) {
	$cache_class = $options{cache_class};
	$cache_class = "Cache::$cache_class" unless $cache_class =~ /::/;
	delete($options{cache_class});
    }
    load_pkg('Cache::Cache', '$m->cache requires the Cache::Cache module, available from CPAN.');
    load_pkg($cache_class, 'Fix your Cache::Cache installation or choose another cache class.');

    my $cache = $cache_class->new (\%options)
	or error "could not create cache object";

    return $cache;
}

sub cache_self {
    my ($self, %options) = @_;

    return if $self->top_stack->{in_cache_self};

    my $expires_in = delete $options{expires_in} || 'never';
    my $key = delete $options{key} || '__mason_cache_self__';
    my $cache = $self->cache(%options);

    my ($output, $retval);
    if (my $cached = $cache->get($key)) {
	($output, $retval) = @$cached;
    } else {
	local $self->top_stack->{in_cache_self} = 1;

	push @{ $self->{buffer_stack} },
            $self->top_buffer->new_child(sink => \$output, ignore_flush => 1);

	my $comp = $self->top_stack->{comp};
	my @args = @{ $self->top_stack->{args} };

	#
	# Go back and find the context that the component was first
	# called in (back up there in the comp method).
	#
	my $wantarray = (caller(1))[5];
	my @result;
	eval {
	    if ($wantarray) {
		@result = $comp->run(@args);
	    } elsif (defined $wantarray) {
		$result[0] = $comp->run(@args);
	    } else {
		$comp->run(@args);
	    }
	};
	$retval = \@result;

	#
	# Whether there was an error or not we need to pop the buffer
	# stack.
	#
	pop @{ $self->{buffer_stack} };
	if ($@) {
	    UNIVERSAL::can($@, 'rethrow') ? $@->rethrow : error $@;
	}

	$cache->set($key, [$output, $retval], $expires_in);
    }

    #
    # Print the component output.
    #
    $self->print($output);

    #
    # Return the component return value in case the caller is interested,
    # followed by 1 indicating the cache retrieval success.
    #
    return (@$retval, 1);

}

sub call_dynamic {
    my ($m, $key, @args) = @_;
    my $comp = ($m->current_comp->is_subcomp) ? $m->current_comp->owner : $m->current_comp;
    if (!defined($comp->dynamic_subs_request) or $comp->dynamic_subs_request ne $m) {
	$comp->dynamic_subs_init;
	$comp->dynamic_subs_request($m);
    }

    return $comp->run_dynamic_sub($key, @args);
}

sub call_next {
    my ($self,@extra_args) = @_;
    my $comp = $self->fetch_next
	or error "call_next: no next component to invoke";
    return $self->comp($comp, @{$self->current_args}, @extra_args);
}

sub caller
{
    my ($self) = @_;
    return $self->callers(1);
}

#
# Return a specified component from the stack, or the whole stack as a list.
#
sub callers
{
    my $self = shift;
    if (defined $_[0]) {
	return $self->stack_entry($_[0])->{comp};
    } else {
	return map($_->{comp}, reverse $self->stack);
    }
}

#
# Return a specified argument list from the stack.
#
sub caller_args
{
    my ($self,$index) = @_;
    param_error "caller_args expects stack level as argument" unless defined $index;

    my $args = $self->stack_entry($index)->{args};
    return wantarray ? @$args : { @$args };
}

sub comp_exists
{
    my ($self, $path) = @_;
    return $self->interp->comp_exists(absolute_comp_path($path, $self->current_comp->dir_path)) ? 1 : 0;
}

sub decline
{
    my ($self) = @_;

    $self->clear_buffer;
    my $subreq = $self->make_subrequest
	(comp => $self->{top_path},
	 args => [$self->request_args],
	 declined_comps => {$self->request_comp->comp_id, 1, %{$self->{declined_comps}}});
    my $retval = $subreq->exec;
    $self->abort($retval);
}

#
# Return the current number of stack levels. 1 means top level, 0
# means that no component has been called yet.
#
sub depth
{
    my ($self) = @_;

    # direct access for speed because this method is called on every
    # call to $m->comp
    return scalar @{ $self->{stack} };
}

#
# Given a component path (absolute or relative), returns a component.
# Handles SELF and PARENT, comp:method, relative->absolute
# conversion, and local subcomponents.
#
sub fetch_comp
{
    my ($self,$path) = @_;
    param_error "fetch_comp: requires path as first argument" unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	my $c = $self->current_comp->parent
	    or error "PARENT designator used from component with no parent";
	return $c;
    }

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my $method_comp;
	my ($owner_path,$method_name) = split(':',$path,2);
	my $owner_comp = $self->fetch_comp($owner_path)
	    or error "could not find component for path '$owner_path'\n";
	$owner_comp->_locate_inherited('methods',$method_name,\$method_comp)
	    or error "no method '$method_name' for component " . $owner_comp->title;
	return $method_comp;
    }

    #
    # If path does not contain a slash, check for a subcomponent in the
    # current component first.
    #
    if ($path !~ /\//) {
	my $cur_comp = $self->current_comp;
	# Check my subcomponents.
	if (my $subcomp = $cur_comp->subcomps($path)) {
	    return $subcomp;
	}
	# If I am a subcomponent, also check my owner's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($cur_comp->is_subcomp and my $subcomp = $cur_comp->owner->subcomps($path)) {
	    return $subcomp;
	}
    }

    #
    # Otherwise pass the absolute path to interp->load.
    #
    # For speed, don't call ->current_comp, instead access it directly
    $path = absolute_comp_path($path, $self->{stack}[-1]{comp}->dir_path)
	unless substr($path, 0, 1) eq '/';

    my $comp = $self->interp->load($path);

    return $comp;
}

#
# Fetch the index of the next component in wrapper chain. If current
# component is not in chain, search the component stack for the most
# recent one that was.
#
sub _fetch_next_helper {
    my ($self) = @_;
    my $index = $self->{wrapper_index}->{$self->current_comp->path};
    unless (defined($index)) {
	my @callers = $self->callers;
	shift(@callers);
	while (my $comp = shift(@callers) and !defined($index)) {
	    $index = $self->{wrapper_index}->{$comp->path};
	}
    }
    return $index;
}

#
# Fetch next component in wrapper chain.
#
sub fetch_next {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next: cannot find next component in chain"
	unless defined($index);
    return $self->{wrapper_chain}->[$index+1];
}

#
# Fetch remaining components in wrapper chain.
#
sub fetch_next_all {
    my ($self) = @_;
    my $index = $self->_fetch_next_helper;
    error "fetch_next_all: cannot find next component in chain"
	unless defined($index);
    my @wc = @{$self->{wrapper_chain}};
    return @wc[($index+1)..$#wc];
}

sub file
{
    my ($self,$file) = @_;
    my $interp = $self->interp;
    unless ( File::Spec->file_name_is_absolute($file) ) {
	if ($self->current_comp->is_file_based) {
	    my $source_dir = $self->current_comp->source_dir;
	    $file = File::Spec->catfile( $source_dir, $file );
	} else {
	    $file = File::Spec->catfile( File::Spec->rootdir, $file );
	}
    }
    my $content = read_file($file,1);
    return $content;
}

sub print
{
    my $self = shift;

    # direct access for optimization cause $m->print is called a lot
    $self->{buffer_stack}[-1]->receive(@_);

    # ditto
    $self->flush_buffer if $self->{autoflush};
}

*out = \&print;

#
# Execute the given component
#
sub comp {
    my $self = shift;

    # Get modifiers: optional hash reference passed in as first argument.
    # merge multiple hash references to simplify user and internal usage.
    my %mods = ();
    %mods = (%{shift()},%mods) while ref($_[0]) eq 'HASH';

    my ($comp,@args) = @_;

    param_error "comp: requires path or component as first argument"
	unless defined($comp);

    #
    # $comp can be an absolute path or component object.  If a path,
    # load into object.
    #
    my $path;
    if (!ref($comp)) {
	$path = $comp;
	$comp = $self->fetch_comp($path)
	    or error "could not find component for path '$path'\n";
    }

    #
    # Check for maximum recursion.
    #
    my $depth = $self->depth;
    error "$depth levels deep in component stack (infinite recursive call?)\n"
        if ($depth >= $self->max_recurse);

    #
    # Determine base_comp (base component for method and attribute inheritance)
    # User may override with { base_comp => $compref }
    # Don't change on SELF:x and PARENT:x calls
    # Assume they know what they are doing if a component ref is passed in
    #
    my $base_comp = exists($mods{base_comp}) ? $mods{base_comp} : $self->base_comp;
    unless ( $mods{base_comp} ||	# base_comp override
	     !$path || 		# path is undef if $comp is a reference
	     $path =~ m/^(?:SELF|PARENT)(?:\:..*)?$/ ) {
	$base_comp = ( $path =~ m/(.*):/ ?
		       $self->fetch_comp($1) :
		       $comp );
	$base_comp = $base_comp->owner if $base_comp->is_subcomp;
    }

    # Push new frame onto stack.
    push @{ $self->{stack} }, { comp => $comp,
                                args => [@args],
                                base_comp => $base_comp,
                                content => $mods{content},
                              };

    if ($mods{store}) {
	# This extra buffer is to catch flushes (in the given scalar ref).
	# The component's main buffer can then be cleared without
	# affecting previously flushed output.
        push @{ $self->{buffer_stack} },
            $self->top_buffer->new_child( sink => $mods{store}, ignore_flush => 1 );
    }
    push @{ $self->{buffer_stack} }, $self->top_buffer->new_child;

    my @result;

    # The eval block creates a new context so we need to get this
    # here.
    my $wantarray = wantarray;

    #
    # Finally, call component subroutine.
    #
    eval {
        if ($wantarray) {
            @result = $comp->run(@args);
        } elsif (defined $wantarray) {
            $result[0] = $comp->run(@args);
        } else {
            $comp->run(@args);
        }
    };

    #
    # If an error occurred, pop stack and pass error down to next level.
    #
    if (my $err = $@) {
	# any unflushed output is at $self->top_buffer->output
	$self->flush_buffer if $self->aborted;

	pop @{ $self->{stack} };
	pop @{ $self->{buffer_stack} };
	pop @{ $self->{buffer_stack} } if ($mods{store});

	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    }

    $self->top_buffer->flush;
    pop @{ $self->{stack} };
    pop @{ $self->{buffer_stack} };
    pop @{ $self->{buffer_stack} } if ($mods{store});

    return wantarray ? @result : $result[0];  # Will return undef in void context (correct)
}

#
# Like comp, but return component output.
#
sub scomp {
    my $self = shift;
    my $buf;
    $self->comp({store=>\$buf},@_);
    return $buf;
}

sub content {
    my $self = shift;
    my $content = $self->top_stack->{content};
    return undef unless defined($content);

    # make the stack frame look like we are still the previous component
    my $old_frame = pop @{ $self->{stack} };

    push @{ $self->{buffer_stack} }, $self->top_buffer->new_child( ignore_flush => 1 );
    eval { $content->(); };
    my $err = $@;

    my $buffer = pop @{ $self->{buffer_stack} };

    push @{ $self->{stack} }, $old_frame;

    if ($err) {
	UNIVERSAL::can($err, 'rethrow') ? $err->rethrow : error $err;
    }

    return $buffer->output;
}

sub clear_buffer
{
    my $self = shift;
    for ($self->buffer_stack) {
	last if $_->ignore_flush;
	$_->clear;
    }
}

sub flush_buffer
{
    my $self = shift;
    for ($self->buffer_stack) {
	last if $_->ignore_flush;
	$_->flush;
    }
}

sub request_args
{
    my ($self) = @_;
    if (wantarray) {
	return @{$self->{request_args}};
    } else {
	return { @{$self->{request_args}} };
    }
}
*top_args = \&request_args;
*top_comp = \&request_comp;

#
# Subroutine called by every component while in debug mode, convenient
# for breakpointing.
#
sub debug_hook
{
    1;
}


#
# stack handling
#

# Return the current stack as an array.
sub stack {
    my ($self) = @_;
    return @{ $self->{stack} };
}

# Return the stack entry 'i' slots from the /back/ of the array
sub stack_entry {
    my ($self, $i) = @_;
    return $self->{stack}->[-1 - $i];
}

# Set or retrieve the hashref at the top of the stack.
sub top_stack {
    my ($self,$href) = @_;
    error "top_stack: nothing on component stack"
	unless $self->depth > 0;
    $self->{stack}->[-1] = $href if defined($href);
    return $self->{stack}->[-1];
}

# These push/pop interfaces are not used internally (for speed) but
# may be useful externally.  For example, Compiler::ToObject generates
# code for <%filter> blocks that pushes and pops a buffer object.
sub push_buffer_stack {
    my $self = shift;

    push @{ $self->{buffer_stack} }, shift;
}

sub pop_buffer_stack {
    my ($self) = @_;
    return pop @{ $self->{buffer_stack} };
}

sub push_stack {
    my ($self,$href) = @_;
    push @{ $self->{stack} }, $href;
}

sub pop_stack {
    my ($self) = @_;
    return pop @{ $self->{stack} };
}

sub buffer_stack {
    my ($self) = @_;
    return wantarray ? reverse @{ $self->{buffer_stack} } : @{ $self->{buffer_stack} };
}


#
# Accessor methods for top of stack elements.
#
sub current_comp { return $_[0]->top_stack->{comp} }
sub current_args { return $_[0]->top_stack->{args} }
sub top_buffer { return $_[0]->{buffer_stack}->[-1] }
sub base_comp { return $_[0]->top_stack->{base_comp} }

package Tie::Handle::Mason;

sub TIEHANDLE
{
    my $class = shift;

    my $req = shift;

    return bless { request => $req }, $class;
}

sub PRINT
{
    my $self = shift;

    my $old = select STDOUT;

    $self->{request}->print(@_);

    select $old;
}

sub PRINTF
{
    my $self = shift;

    # apparently sprintf(@_) won't work, it needs to be a scalar
    # followed by a list
    $self->PRINT(sprintf(shift, @_));
}

1;

__END__

=head1 NAME

HTML::Mason::Request - Mason Request Class

=head1 SYNOPSIS

    $m->abort (...)
    $m->comp (...)
    etc.

=head1 DESCRIPTION

The Request API is your gateway to all Mason features not provided by
syntactic tags. Mason creates a new Request object for every web
request. Inside a component you access the current request object via
the global C<$m>.  Outside of a component, you can use the class
method C<instance>.

=head1 COMPONENT PATHS

The methods L<Request-E<gt>comp|HTML::Mason::Request/comp>,
L<Request-E<gt>comp_exists|HTML::Mason::Request/comp_exists>, and
L<Request-E<gt>fetch_comp|HTML::Mason::Request/fetch_comp> take a
component path argument.  Component paths are like URL paths, and
always use a forward slash (/) as the separator, regardless of what
your operating system uses.

=over

=item *

If the path is absolute (starting with a '/'), then the component is
found relative to the component root.

=item *

If the path is relative (no leading '/'), then the component is found
relative to the current component directory.

=item *

If the path matches both a subcomponent and file-based component, the
subcomponent takes precedence.

=back

=head1 CONSTRUCTOR PARAMETERS

=over 4

=item autoflush

Indicates whether or not to delay sending output until all output has
been generated.

=item data_cache_defaults

The default parameters used when $m->cache is called.

=item dhandler_name

File name used for dhandlers. Default is "dhandler".

=item error_format

The format used to display errors.  The options are 'brief', 'text',
'line', and 'html'.  The default is 'text' except when running under
ApacheHandler, in which case the default is 'html'.

=item error_mode

This can be either 'fatal' or 'output'.  If the mode is 'fatal',
errors generate an exception.  With 'output' mode, the error is sent
to the same output as normal component output.  The default is
'fatal', except when running under ApacheHandler or CGIHandler, in
which case the output is 'default'.

=item max_recurse

The maximum recursion depth for the component stack, for the request
stack, and for the inheritance stack. An error is signalled if the
maximum is exceeded.  Default is 32.

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

=back

=head1 METHODS

=over

=for html <a name="item_abort"></a>

=item abort ([return value])

Ends the current request, finishing the page without returning
through components. The optional argument specifies the return
value from C<Interp::exec>; in a web environment, this ultimately
becomes the HTTP status code.

C<abort> is implemented by throwing an HTML::Mason::Exception::Abort
object and can thus be caught by eval(). The C<aborted> method is a
shortcut for determining whether a caught error was generated by
C<abort>.

=for html <a name="item_aborted"></a>

=item aborted ([$err])

Returns true or undef indicating whether the specified C<$err>
was generated by C<abort>. If no C<$err> was passed, use C<$@>.

In this code, we catch and process fatal errors while letting C<abort>
exceptions pass through:

    eval { code_that_may_fail_or_abort() };
    if ($@) {
        die $@ if $m->aborted;

        # handle fatal errors...

C<$@> can lose its value quickly, so if you are planning to call
$m->aborted more than a few lines after the eval, you should save $@
to a temporary variable.

=for html <a name="item_base_comp"></a>

=item base_comp

Returns the current base component for method and attributes.
Initially, the base component is the same as the requested component
(returned by C<< $m->request_comp >>.  However, whenever a component
call is made, the base component changes to the called component,
unless the component call was made uses a component object for its
first argument, or the call starts with SELF: or PARENT:.

=for html <a name="item_cache"></a>

=item cache (cache_class=>'...', [cache_options])

C<$m-E<gt>cache> returns a new cache object with a namespace specific
to this component.

I<cache_class> specifies the class of cache object to create. It
defaults to Cache::FileCache in most cases, or Cache::MemoryCache if
the interpreter has no data directory, and must be a subclass of
Cache::Cache.  If I<cache_class> does not contain a "::", the prefix
"Cache::" is automatically prepended.

I<cache_options> may include any valid options to the new() method of
the cache class. e.g. for Cache::FileCache, valid options include
default_expires_in and cache_depth.

See the L<data caching in the I<Component Developer's
Guide>|HTML::Mason::Devel/"data caching"> for examples and caching
strategies. See the Cache::Cache documentation for a complete list of
options and methods.

=for html <a name="item_cache_self"></a>

=item cache_self (expires_in => '...', key => '...', [cache_options])

C<$m-E<gt>cache_self> caches the entire output and return result of a
component.

It takes all of the options which can be passed to the cache method,
plus two additional options:

=over

=item *

I<expires_in>: Indicates when the cache expires - it is passed as the
third argument to $cache-E<gt>set.  See the Cache::Cache documentation
for details on what formats it accepts.

=item *

I<key>: An identifier used to uniquely identify the cache results - it
is passed as the first argument to $cache-E<gt>get and
$cache-E<gt>set.  A default key will be provided if none is passed.

=back

C<cache_self> either returns undef, or a list containing the
return value of the component followed by '1'. You should return
immediately upon getting the latter result, as this indicates
that you are inside the second invocation of the component.

To cache the component's output:

    <%init>
    return if $m->cache_self(expires_in => '3 hours'[, key => 'fookey']);
    ... <rest of init> ...
    </%init>

To cache the component's scalar return value:

    <%init>
    my ($result, $cached) = $m->cache_self(expires_in => '3 hours'[, key => 'fookey']);

    return $result if $cached;
    ... <rest of init> ...
    </%init>

To cache the component's list return value:

    <%init>
    my (@retval) = $m->cache_self(expires_in => '3 hours'[, key => 'fookey']);

    return @retval if pop @retval;
    ... <rest of init> ...
    </%init>

We call C<pop> on C<@retval> to remove the mandatory '1' at the end of
the list.

=for html <a name="item_caller_args"></a>

=item caller_args

Returns the arguments passed by the component at the specified stack
level. Use a positive argument to count from the current component and
a negative argument to count from the component at the bottom of the
stack. e.g.

    $m->caller_args(0)   # arguments passed to current component
    $m->caller_args(1)   # arguments passed to component that called us
    $m->caller_args(-1)  # arguments passed to first component executed

When called in scalar context, a hash reference is returned.  When
called in list context, a list of arguments (which may be assigned to
a hash) is returned.

=for html <a name="item_callers"></a>

=item callers

With no arguments, returns the current component stack as a list of
component objects, starting with the current component and ending with
the top-level component. With one numeric argument, returns the
component object at that index in the list. Use a positive argument to
count from the current component and a negative argument to count from
the component at the bottom of the stack. e.g.

    my @comps = $m->callers   # all components
    $m->callers(0)            # current component
    $m->callers(1)            # component that called us
    $m->callers(-1)           # first component executed

=for html <a name="item_call_next"></a>

=item call_next ([args...])

Calls the next component in the content wrapping chain; usually called
from an autohandler. With no arguments, the original arguments are
passed to the component.  Any arguments specified here serve to
augment and override (in case of conflict) the original
arguments. Works like C<$m-E<gt>comp> in terms of return value and
scalar/list context.  See the L<autohandlers section in the
I<Component Developer's Guide>|HTML::Mason::Devel/"autohandlers"> for
examples.

=for html <a name="item_clear_buffer"></a>

=item clear_buffer

Clears the Mason output buffer. Any output sent before this line is
discarded. Useful for handling error conditions that can only be
detected in the middle of a request.

clear_buffer is, of course, thwarted by C<flush_buffer>.

=for html <a name="item_comp"></a>

=item comp (comp, args...)

Calls the component designated by I<comp> with the specified
option/value pairs. I<comp> may be a component path or a component
object.

Components work exactly like Perl subroutines in terms of return
values and context. A component can return any type of value, which is
then returned from the C<$m-E<gt>comp> call.

The <& &> tag provides a convenient shortcut for C<$m-E<gt>comp>.

As of 1.10, component calls can accept an initial hash reference of
I<modifiers>.  The only currently supported modifier is C<store>, which
stores the component's output in a scalar reference. For example:

  my $buf;
  my $return = $m->comp( { store => \$buf }, '/some/comp', type => 'big' );

This mostly duplicates the behavior of I<scomp>, but can be useful in
rare cases where you need to capture both a component's output and
return value.

This modifier can be used with the <& &> tag as well, for example:

  <& { store => \$buf }, '/some/comp', size => 'medium' &>

=for html <a name="item_comp_exists"></a>

=item comp_exists (comp_path)

Returns 1 if I<comp_path> is the path of an existing component, 0
otherwise.  That path given may be relative, in which case the current
component's directory path will be prepended.

=for html <a name="content"></a>

=item content

Evaluates the content (passed between <&| comp &> and </&> tags) of the 
current component, and returns the resulting text.

Returns undef if there is no content.

=for html <a name="item_count"></a>

=item count

Returns the number of this request, which is unique for a given
request and interpreter.

=for html <a name="item_current_comp"></a>

=item current_comp

Returns the current component object.

=for html <a name="item_decline"></a>

=item decline

Used from a top-level component or dhandler, this method clears the
output buffer, aborts the current request and restarts with the next
applicable dhandler up the tree. If no dhandler is available, an error
occurs.  This method bears no relation to the Apache DECLINED status
except in name.

=for html <a name="item_depth"></a>

=item depth

Returns the current size of the component stack.  The lowest possible
value is 1, which indicates we are in the top-level component.

=for html <a name="item_dhandler_arg"></a>

=item dhandler_arg

If the request has been handled by a dhandler, this method returns the
remainder of the URI or C<Interp::exec> path when the dhandler directory is
removed. Otherwise returns undef.

C<dhandler_arg> may be called from any component in the request, not just
the dhandler.

=for html <a name="item_error_format"></a>

=item error_format

Indicates how errors are formatted. The built-in choices are

=over

=item *

I<brief> - just the error message with no trace information

=item *

I<text> - a multi-line text format

=item *

I<line> - a single-line text format, with different pieces of information separated by tabs (useful for log files)

=item *

I<html> - a fancy html format

=back

The default format within mod_perl and CGI environments is either I<line> or
I<html> depending on whether the error mode is I<fatal> or I<output>,
respectively. The default for standalone mode is I<text>.

The formats correspond to HTML::Mason::Exception methods named
as_I<format>. You can define your own format by creating an
appropriately named method; for example, to define an "xml" format,
create a method HTML::Mason::Exception::as_xml patterned after one of
the built-in methods.

=for html <a name="item_error_mode"></a>

=item error_mode

Indicates how errors are returned to the caller.  The choices are
I<fatal>, meaning die with the error, and I<output>, meaning output
the error just like regular output.

The default mode within mod_perl and CGI environments is I<output>,
causing the error will be displayed in HTML form in the browser.
The default for standalone mode is I<fatal>.

=for html <a name="item_exec"></a>

=item exec (comp, args...)

Starts the request by executing the top-level component and
arguments. This is normally called for you on the main request, but
you can use it to execute subrequests.

A request can only be executed once; e.g. it is an error to call this
recursively on the same request.

=for html <a name="item_fetch_comp"></a>

=item fetch_comp (comp_path)

Given a I<comp_path>, returns the corresponding component object or
undef if no such component exists.

=for html <a name="item_fetch_next"></a>

=item fetch_next

Returns the next component in the content wrapping chain, or undef if
there is no next component. Usually called from an autohandler.  See
the L<autohandlers section in the I<Component Developer's
Guide>|HTML::Mason::Devel/"autohandlers"> for usage and examples.

=for html <a name="item_fetch_next_all"></a>

=item fetch_next_all

Returns a list of the remaining components in the content wrapping
chain. Usually called from an autohandler.  See the L<autohandlers
section in the I<Component Developer's
Guide>|HTML::Mason::Devel/"autohandlers"> for usage and examples.

=for html <a name="item_file"></a>

=item file (filename)

Returns the contents of I<filename> as a string. If I<filename> is a
relative path, Mason prepends the current component directory.

=for html <a name="item_flush_buffer"></a>

=item flush_buffer

Flushes the Mason output buffer. Under mod_perl, also sends HTTP
headers if they haven't been sent and calls C<< $r->rflush >> to flush
the Apache buffer. Flushing the initial bytes of output can make your
servers appear more responsive.

Attempts to flush the buffers are ignored within the context of a call
to C<< $m->scomp >> or when output is being stored in a scalar
reference, as with the C< { store => \$out } > component call
modifier.

Additionally, if a component has a C<< <%filter> >> block, that
component is buffered until its entire output is generated.  This
means that inside that component and any components that it calls,
the buffer cannot be flushed.

=for html <a name="item_instance"></a>

=item instance

This class method returns the C<HTML::Mason:::Request> currently in
use.  If called when no Mason request is active it will return
C<undef>.

If called inside a subrequest, it returns the subrequest object.

=for html <a name="item_interp"></a>

=item interp

Returns the Interp object associated with this request.

=for html <a name="item_make_subrequest"></a>

=item make_subrequest (comp => path, args => arrayref, other parameters)

This method creates a new Request object which inherits its parent's
settable properties, such as C<autoflush> and C<out_method>.  These
values may be overridden by passing parameters to this method.

The "comp" parameter is required, while all other parameters are
optional.

See the L<Subrequests section in the I<Component Developer's
Guide>|HTML::Mason::Devel/"Subrequests"> for more details about the
subrequest feature.

=for html <a name="item_out"></a>

=item out (string)

A synonym for C<$m-E<gt>print>.

=for html <a name="item_print"></a>

=item print (string)

Print the given I<string>. Rarely needed, since normally all text is just
placed in the component body and output implicitly. C<$m-E<gt>print> is useful
if you need to output something in the middle of a Perl block.

In 1.1 and on, C<print> and C<$r-E<gt>print> are remapped to C<$m-E<gt>print>,
so they may be used interchangeably. Before 1.1, one should only use
C<$m-E<gt>print>.

=for html <a name="item_scomp"></a>

=item scomp (comp, args...)

Like C<$m-E<gt>comp>, but returns the component output as a string
instead of printing it. (Think sprintf versus printf.) The
component's return value is discarded.

=for html <a name="item_subexec"></a>

=item subexec (comp, args...)

This method creates a new subrequest with the specified top-level
component and arguments, and executes it. This is most often used
to perform an "internal redirect" to a new component such that
autohandlers and dhandlers take effect.

=for html <a name="item_request_args"></a>

=item request_args

Returns the arguments originally passed to the top level component
(see L<Request-E<gt>request_comp|HTML::Mason::Request/request_comp> for
definition).  When called in scalar context, a hash reference is
returned. When called in list context, a list of arguments (which may
be assigned to a hash) is returned.

=for html <a name="item_request_comp"></a>

=item request_comp

Returns the component originally called in the request. Without
autohandlers, this is the same as the first component executed.  With
autohandlers, this is the component at the end of the
C<$m-E<gt>call_next> chain.

=back

=head1 APACHE-ONLY METHODS

These additional methods are available when running Mason with mod_perl
and the ApacheHandler.

=over

=for html <a name="item_ah"></a>

=item ah

Returns the ApacheHandler object associated with this request.

=for html <a name="item_apache_req"></a>

=item apache_req

Returns the Apache request object.  This is also available in the
global $r.

=for html <a name="item_auto_send_headers"></a>

=item auto_send_headers

True or undef; default true.  Indicates whether Mason should
automatically send HTTP headers before sending content back to the
client. If you set to false, you should call $r->send_http_header
manually.

See the L<Sending HTTP Headers section of the I<Component Developer's
Guide>|HTML::Mason::Devel/"Sending HTTP Headers> for details about the
automatic header feature.

=back

=head1 APACHE- OR CGI-ONLY METHOD

This method is available when Mason is running under either the
ApacheHandler or CGIHandler modules.

=over 4

=for html <a name="item_cgi_object"></a>

=item cgi_object

Returns the CGI object used to parse any CGI parameters submitted to
the component, assuming that you have not changed the default value of
the ApacheHandler C<args_method> parameter.  If you are using the
'mod_perl' args method, then calling this method is a fatal error.
See the L<ApacheHandler|HTML::Mason::ApacheHandler> and
L<CGIHandler|HTML::Mason::CGIHandler> documentation for more details.

=for html <a name="item_redirect"></a>

=item redirect ($url)

Given a url, this generates a proper HTTP redirect for that URL. It
uses C<< $m->clear_buffer >> to clear out any previous output, and
C<< $m->abort >> to abort the request with an appropriate status code.

Since this is implemented using C<< $m->abort >>, it will be trapped
by an C< eval {} > block.  If you are using an C< eval {} > block in
your code to trap errors, you need to make sure to rethrow these
exceptions, like this:

  eval {
      ...
  };

  die $@ if $m->aborted;

  # handle other exceptions

=back

=head1 AUTHORS

Jonathan Swartz <swartz@pobox.com>, Dave Rolsky <autarch@urth.org>, Ken Williams <ken@mathforum.org>

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>,
L<HTML::Mason::Devel|HTML::Mason::Devel>,
L<HTML::Mason::Component|HTML::Mason::Component>

=cut
