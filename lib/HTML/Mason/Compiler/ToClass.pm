# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Compiler::ToClass;

use strict;

use base qw( HTML::Mason::Compiler );

use Exception::Class qw( Mason::Exception::Compiler );

use HTML::Mason::MethodMaker
    ( read_write => [ qw( comp_class
                          in_package
			  postamble
			  preamble
			  use_strict
                        )
		    ],
    );

my %fields =
    ( allow_globals => [],
      comp_class => 'HTML::Mason::Component',
    );

sub new
{
    my $proto = shift;
    my $class = ref $proto || $proto;
    my %p = @_;

    my $self = bless {}, $class;
    $self = $self->SUPER::new(%p);

    foreach ( keys %p )
    {
	if ( exists $fields{$_} )
	{
	    $self->{$_} = $p{$_} || $fields{$_};
	}
    }
    foreach ( keys %fields )
    {
	$self->{$_} ||= $fields{$_};
    }

    return $self;
}

sub output
{
    my $self = shift;

    $self->{current_comp} = $self;
    my $class = join "\n", ( $self->_header,
			     $self->_body
			   );
    $self->{current_comp} = undef;

    return $class;
}

sub _header
{
    my $self = shift;

    my $class = $self->_class_name;

    my $base = 'HTML::Mason::ComponentClass';

    return ( "package $class;\n",
	     "use base qw( $base );\n",
	   );
}

sub _class_name
{
    my $self = shift;

    my $class_name = $self->{lexer}->file;
    $class_name =~ s,[/\\],::,g;
    $class_name =~ s/([^:\w])/'0x' . hex(ord($1))/eg;

    return $class_name;
}

sub _body
{
    my $self = shift;

    return ( $self->_init_method,
	     $self->_body_method,
	     $self->_subcomponents,
	     $self->_methods,
	   );
}

sub _init_method
{
    my $self = shift;

    return ( <<"EOF",
sub _init
{
    my \$self = shift;

    unless ( \$self->{_initialized} )
    {
        $self->{once};
        \$self->{initialized} = 1;
    }
    $self->{shared}
}
EOF
	   );
}

sub _body_method
{
    my $self = shift;

    my @args;
    if ( $self->{current_comp}{args} )
    {
	@args = ( <<'EOF',
    if (@_args % 2 == 0) { %ARGS = @_args } else { die "Odd number of parameters passed to component expecting name/value pairs" }
EOF
		  $self->_arg_declarations,
		);
    }
    else
    {
	@args = ( "    { local \$^W; \%ARGS = \@_args unless (\@_args % 2); }" );
    }

    return ( <<"EOF",
sub _body
{
    my \$self = shift;
    my \%ARGS;
    my \@_args = \@{ \$self->{args} }, \@_;
EOF
	     @args,
	     $self->{filter},
	     $self->{init},
	     $self->{body},
	     $self->{cleanup},
	     '}',
	   );
}

sub _arg_declarations
{
    my $self = shift;

    my @args;
    foreach ( values %{ $self->{current_comp}{args} } )
    {
	my $default_val = ( defined $_->{default} ?
			    $_->{default} :
			    qq|die "no value sent for required parameter '$_->{name}'"|,
			  );
	# allow for comments after default declaration
	$default_val .= "\n" if defined $_->{default} && $_->{default} =~ /\#/;

	if ( $_->{type} eq '$' )
	{
	    push @args,
		"    my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : \$ARGS{'$_->{name}'} );";
	}
	# Array
	elsif ( $_->{type} eq '@' )
	{
	    push @args, ( "    my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : ",
			  "    UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'}}  : ( \$ARGS{'$_->{name}'} ) );",
			);
	}
	# Hash
	elsif ($_->{type} eq "\%") {
	    push @args, ( "    my $_->{type}$_->{name} = ( !exists \$ARGS{'$_->{name}'} ? $default_val : ",
			  "    UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'ARRAY' ) ? \@{ \$ARGS{'$_->{name}'} } : ",
			  "    UNIVERSAL::isa( \$ARGS{'$_->{name}'}, 'HASH' ) ? \%{ \$ARGS{'$_->{name}'} } : ",
			  qq|    die "single value sent for hash parameter '$_->{type}$_->{name}'");|,
			);
	}
    }

    return @args;
}

sub _subcomponents
{
    my $self = shift;

    my $class = $self->_class_name;

    my @subcomp;
    while ( my ($name, $data) = each %{ $self->{subcomponents} } )
    {
	push @subcomp,
	    <<"EOF";
sub $name
{
    die "$name can't be called from outside $class class"
        unless (caller(1))[1] eq '$class';

    my \$self = shift;
}
EOF
    }

    return @subcomp;
}

sub _methods
{
    my $self = shift;

    my $class = $self->_class_name;

    my @methods;
    while ( my ($name, $data) = each %{ $self->{methods} } )
    {
	push @methods,
	    <<"EOF";
sub $name
{
    my \$self = shift;
}
EOF
    }

    return @methods;
}


1;
