# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Buffer;

use strict;

use Class::Container;
use base qw(Class::Container);

use HTML::Mason::Exceptions( abbr => ['param_error'] );

use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error join '', @_ } );

use HTML::Mason::MethodMaker
    ( read_only => [ qw( sink
			 parent
                         filter
			 ignore_flush
		       ) ],
    );

__PACKAGE__->valid_params
    (
     sink         => { parse => 'code', type => SCALARREF | CODEREF, optional => 1,
		       descr => "A subroutine or scalar reference that will receive the output stream",
		       public => 0 },
     parent       => { isa => 'HTML::Mason::Buffer', optional => 1,
		       descr => "A parent buffer of the current buffer",
		       public => 0 },
     ignore_flush => { parse => 'boolean', type => SCALAR, default => 0,
		       descr => "Whether the flush() method is a no-op or actually flushes content",
		       public => 0 },
     filter       => { type => CODEREF, optional => 1,
		       descr => "A subroutine through which all output should pass",
		       public => 0 },
    );

sub new
{
    my $class = shift;
    my $self = $class->SUPER::new(@_);
    $self->_initialize;
    return $self;
}

# we set the {sink_is_scalar} flag as an optimization for sinks which
# are scalarrefs, the commonest case.  This lets us optimize the
# receive() method to simply concatenate onto the string rather than
# always calling a sub reference.
sub _initialize
{
    my $self = shift;

    if ( defined $self->{sink} )
    {
	if ( UNIVERSAL::isa( $self->{sink}, 'SCALAR' ) )
	{
            $self->{buffer} = delete $self->{sink};
	    $self->{sink_is_scalar} = 1;
	}
    }
    else
    {
	# create an empty string to use as buffer
	my $buf = '';
	$self->{buffer} = \$buf;
        $self->{sink_is_scalar} = 1;
    }

    $self->{ignore_flush} = 1 unless $self->{parent};
}

sub new_child
{
    my $self = shift;
    return ref($self)->new( parent => $self, @_ );
}

sub receive
{
    my $self = shift;

    return unless @_;

    if ( $self->{sink_is_scalar} )
    {
        $self->{sink} .= join '', grep { defined } @_;
    }
    else
    {
        $self->{sink}->(@_);
    }
}

sub flush
{
    my $self = shift;
    return if $self->ignore_flush;

    my $output = $self->output;
    return unless defined $output && $output ne '';

    $self->parent->receive( $output ) if $self->parent;
    $self->clear;
}

sub clear
{
    my $self = shift;
    return unless exists $self->{buffer};
    ${$self->{buffer}} = '';
}

sub output
{
    my $self = shift;
    return unless exists $self->{buffer};
    my $output = ${$self->{buffer}};
    return $self->filter->( $output ) if $self->filter;
    return $output;
}

1;

__END__

=head1 NAME

HTML::Mason::Buffer - Objects for Handling Component Output

=head1 SYNOPSIS

  my $buffer = HTML::Mason::Buffer->new( sink => sub { print @_ } );

  my $child = $buffer->new_child;

  $child->receive( 'foo', 'bar' );

=head1 DESCRIPTION

Mason's buffer objects handle all output generated by components.
They are used to implement C<< <%filter> >> blocks, the C<< $m->scomp >>
method, the C<store> component call modifier, and content-filtering
component feature.

Buffers can either store output in a scalar, internally, or they can
be given a callback to call immediately when output is generated.

Most users will never have to deal with buffer objects directly, but
will instead use Request object methods such as C<print> or
C<clear_buffer>.

=head1 CONSTRUCTOR

Buffer objects can be constructed in two different ways.  Like any
other Mason object, they can be created via their C<new> method.

This method takes several parameters, all of them optional.

=over

=item sink

This should be either a subroutine reference or a scalar reference.

If this is a subroutine reference, then any output received by the
buffer will be passed to this subroutine reference as a list of one or
more items.

If this is a scalar reference, then data will be concatenated onto
this scalar as it is received.

If no parameter is given, then output will be buffered internally, to
be retrieved by the C<output> method.

=item ignore_flush

If this parameter is true, then the created buffer will ignore calls
to its C<flush> method.  This parameter defaults to true for buffers
created via the C<new> method, and false for those created via the
C<new_child> method.

=item filter

This parameter should be a subroutine reference which should expect to
receive a single argument, the output to be filtered.  It should
return the output after transforming it in any way it desires.

=back

New buffers can also be created via the C<new_child> method, which
takes the same parameters as the C<new> method.  The C<new_child>
method is an I<object> method, not a class method.

It creates a new buffer object which will eventually pass its output
to the buffer object that created it.

This allows you to create a stack of buffers.

=head1 METHODS

=over

=item receive

This method takes a list of items to be sent to the buffer's sink.

=item flush

This method tells the buffer to pass any output it may currently have
stored internally to its parent, if it has one.  It then clears the
buffer.

This method does nothing if the buffer does not have any stored
output, which is the case for buffers that were given a subroutine
reference as their C<sink> argument.

=item clear

For buffers which store output internally, this clears any pending
output.

=item output

For buffers which store output internally, this returns any pending
output, possibly passing it through a buffer's filter, if it has one.
This does B<not> clear the buffer.

=back

=cut
