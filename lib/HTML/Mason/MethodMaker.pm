# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::MethodMaker;

use strict;

sub import
{
    my $caller = caller;
    shift; # don't need class name
    my %p = @_;

    if ($p{read_only})
    {
	foreach my $ro ( ref $p{read_only} ? @{ $p{read_only} } : $p{read_only} )
	{
	    no strict 'refs';
	    *{"$caller\::$ro"} = sub { return $_[0]->{$ro} };
	}
    }

    if ($p{read_write})
    {
	foreach my $rw ( ref $p{read_write} ? @{ $p{read_write} } : $p{read_write} )
	{
	    no strict 'refs';
	    *{"$caller\::$rw"} = sub { my $s = shift; $s->{$rw} = shift if @_; return $s->{$rw}; };
	}
    }

    if ($p{read_write_contained})
    {
	foreach my $object (keys %{ $p{read_write_contained} })
	{
	    foreach my $rwc (@{ $p{read_write_contained}{$object} })
	    {
		no strict 'refs';
		*{"$caller\::$rwc"} = sub { my $self = shift;
					    my %new = @_ ? ( $rwc => $_[0] ) : ();
					    my %args = $self->delayed_object_params( $object,
										     %new );
					    return $args{$rwc};
					};
	    }
	}
    }
}

1;

=pod

=head1 NAME

HTML::Mason::MethodMaker - Used to create simple get & get/set methods in other classes

=head1 SYNOPSIS

 use HTML::Mason::MethodMaker ( read_only => 'foo',
                                read_write => [ qw( bar baz ) ] );

=head1 DESCRIPTION

This automates the creation of simple accessor methods.

=head1 USAGE

This module creates methods when it is C<use>'d by another module.
There are two types of methods: 'read_only' and 'read_write'.

Attributes specified as 'read_only' get an accessor that only returns
the value of the attribute.  Presumably, these attributes are set via
more complicated methods in the class or as a side effect of one of
its methods.

Attributes specified as 'read_write' will take a single optional
parameter.  If given, this parameter will become the new value of the
attribute.  This value is then returned from the method.  If no
parameter is given, then the current value is returned.

=cut
