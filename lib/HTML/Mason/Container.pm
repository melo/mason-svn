# Copyright (c) 1998-2001 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Container;

use strict;

# The create_contained_objects() method lets one object
# (e.g. Compiler) transparently create another (e.g. Lexer) by passing
# creator parameters through to the created object.
#
# Any auto-created objects should be declared in a class's
# %CONTAINED_OBJECTS hash.  The keys of this hash are objects which
# can be created and the values are the default classes to use.

# For instance, the key 'lexer' indicates that a 'lexer' parameter
# should be silently passed through, and a 'lexer_class' parameter
# will trigger the creation of an object whose class is specified by
# the value.  If no value is present there, the value of 'lexer' in
# the %CONTAINED_OBJECTS hash is used.  If no value is present there,
# no contained object is created.
#
# We return the list of parameters for the creator.  If contained
# objects were auto-created, their creation parameters aren't included
# in the return value.  This lets the creator be totally ignorant of
# the creation parameters of any objects it creates.

use Params::Validate qw(SCALAR HASHREF);

my %VALID_PARAMS = ();
my %CONTAINED_OBJECTS = ();


sub valid_params
{
    my $class = shift;
    $VALID_PARAMS{$class} = {@_};
}

sub contained_objects
{
    my $class = shift;
    $CONTAINED_OBJECTS{$class} = {@_};
}

sub create_contained_objects
{
    # Typically $self doesn't exist yet, $_[0] is a string classname
    my ($class, %args) = @_;

    my %c = $class->get_contained_objects;
    while (my ($name, $spec) = each %c) {
	my $default_class = ref($spec) ? $spec->{class}   : $spec;
	my $delayed       = ref($spec) ? $spec->{delayed} : 0;
	if (exists $args{$name}) {
	    # User provided an object
	    die "Cannot provide a '$name' object, its creation is delayed" if $delayed;
	    next;
	}

	# Figure out exactly which class to make an object of
	my $contained_class = delete $args{"${name}_class"} || $default_class;
	next unless $contained_class;

	if ($delayed) {
	    $args{"_delayed_$name"} = $class->_get_contained_args($name, $contained_class, \%args);
	    #warn "saving delayed '$name' args for $contained_class: (@{[ %{$args{qq[_delayed_$name]}} ]})";
	    $args{"_delayed_$name"}{_class} = $contained_class;
	} else {
	    $args{$name} = $class->_make_contained_object($name, $contained_class, \%args);
	}
    }

    return %args;
}

sub create_delayed_object
{
    my ($self, $name, %args) = @_;
    my $spec = $self->{"_delayed_$name"}
	or die "Unknown delayed object '$name'";
    my %saved_args = %$spec;
    my $class = delete $saved_args{_class}
	or die "Unknown class for delayed object '$name'";

    return $class->new(%saved_args, %args);
}

sub _get_contained_args
{
    my ($class, $name, $contained_class, $args) = @_;

    die "Invalid class name '$contained_class'" unless $contained_class =~ /^[\w:]+$/;

    unless ($contained_class->can('new'))
    {
	no strict 'refs';
	eval "use $contained_class";
	die $@ if $@;
    }

    # Everything this class will accept, including parameters it will
    # pass on to its own contained objects
    my $allowed = $contained_class->allowed_params($args);

    my %contained_args;
    foreach (keys %$allowed)
    {
	$contained_args{$_} = delete $args->{$_} if exists $args->{$_};
    }
    return \%contained_args;
}

sub _make_contained_object
{
    my ($class, $name, $contained_class, $args) = @_;

    my $contained_args = $class->_get_contained_args($name, $contained_class, $args);
    return $contained_class->new(%$contained_args);
}

# Iterate through this object's @ISA and find all entries in
# 'contained_objects' list.  Return as a hash.
sub get_contained_objects
{
    my $class = ref($_[0]) || shift;

    my %c = %{ $CONTAINED_OBJECTS{$class} || {} };

    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	next unless exists $CONTAINED_OBJECTS{$superclass};
	my %superparams = $superclass->get_contained_objects;
	@c{keys %superparams} = values %superparams;  # Add %superparams to %c
    }

    return %c;
}

sub allowed_params
{
    my $class = shift;
    my $args = ref($_[0]) ? shift : {@_};

    my %p = %{ $class->validation_spec };

    my %c = $class->get_contained_objects;

    foreach my $name (keys %c)
    {
	# Can accept a 'foo' parameter - should already be in the validation_spec.
	# Also, its creation parameters should already have been extracted from $args,
	# so don't extract any parameters.
	next if exists $args->{$name};

	# Can accept a 'foo_class' parameter instead of a 'foo' parameter
	# If neither parameter is present, give up - perhaps it's optional
	my $low_class = "${name}_class";

	if ( exists $args->{$low_class} )
	{
	    delete $p{$name};
	    $p{$low_class} = { type => SCALAR, parse => 'string' };  # A loose spec
	}

	# We have to get the allowed params for the contained object
	# class.  That class could be overridden, in which case we use
	# the new class provided.  Otherwise, we use our default.
	my $spec = exists $args->{$low_class} ? $args->{$low_class} : $c{$name};
	my $contained_class = ref($spec) ? $spec->{class}   : $spec;

	# we have to make sure it is loaded before we try calling
	# ->allowed_params
	unless ( $contained_class->can('allowed_params') )
	{
	    no strict 'refs';
	    eval "use $contained_class";
	    die $@ if $@;
	}

	my $subparams = $contained_class->allowed_params($args);
	@p{keys %$subparams} = values %$subparams;
    }

    return \%p;
}

sub validation_spec
{
    my $class = ref($_[0]) || shift;

    my %p = %{ $VALID_PARAMS{$class} || {} };

    no strict 'refs';
    foreach my $superclass (@{ "${class}::ISA" }) {
	next unless exists $VALID_PARAMS{$superclass};
	my $superparams = $superclass->validation_spec;
	@p{keys %$superparams} = values %$superparams;
    }

    # We may need to allow some '_delayed_$name' parameters
    my %specs = $class->get_contained_objects;
    while (my ($name, $spec) = each %specs) {
	next unless ref $spec;
	next unless $spec->{delayed};
	$p{"_delayed_$name"} = { type => HASHREF };
    }

    return \%p;
}

1;
