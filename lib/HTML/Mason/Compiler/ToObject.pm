# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Compiler::ToObject;

use strict;

use Params::Validate qw(SCALAR validate);
use HTML::Mason::Tools qw(make_fh taint_is_on);

use HTML::Mason::Compiler;
use base qw( HTML::Mason::Compiler );

use HTML::Mason::Exceptions( abbr => [qw(wrong_compiler_error system_error)] );

use File::Path qw(mkpath rmtree);
use File::Basename qw(dirname);

BEGIN
{
    __PACKAGE__->valid_params
	(
	 comp_class =>
         { parse => 'string', type => SCALAR, default => 'HTML::Mason::Component',
           descr => "The class into which component objects will be blessed" },

	 subcomp_class =>
         { parse => 'string', type => SCALAR, default => 'HTML::Mason::Component::Subcomponent',
           descr => "The class into which subcomponent objects will be blessed" },

	 in_package =>
         { parse => 'string', type => SCALAR, default => 'HTML::Mason::Commands',
           descr => "The package in which component execution will take place" },

	 preamble =>
         { parse => 'string', type => SCALAR, default => '',
           descr => "A chunk of Perl code to add to the beginning of each compiled component" },

	 postamble =>
         { parse => 'string', type => SCALAR, default => '',
           descr => "A chunk of Perl code to add to the end of each compiled component" },

	 use_strict =>
         { parse => 'boolean', type => SCALAR, default => 1,
           descr => "Whether to turn on Perl's 'strict' pragma in components" },
	);
}

use HTML::Mason::MethodMaker
    ( read_write => [ map { [ $_ => __PACKAGE__->validation_spec->{$_} ] }
		      qw( comp_class
                          in_package
			  postamble
			  preamble
                          subcomp_class
			  use_strict
                        )
		    ],
    );


sub compile
{
    my $self = shift;
    my %p = @_;

    local $self->{comp_class} = $p{comp_class} if exists $p{comp_class};
    return $self->SUPER::compile( comp_source => $p{comp_source}, name => $p{name} );
}

#
# compile_to_file( source => ..., file => ... )
# Save object text in an object file.
#
# We attempt to handle several cases in which a file already exists
# and we wish to create a directory, or vice versa.  However, not
# every case is handled; to be complete, mkpath would have to unlink
# any existing file in its way.
#
sub compile_to_file
{
    my $self = shift;

    my %p = validate( @_, {   file => { type => SCALAR },
			    source => { isa => 'HTML::Mason::ComponentSource' } },
		    );

    my ($file, $source) = @p{qw(file source)};
    my $object_code = $self->compile( comp_source => $source->comp_source,
				      name => $source->friendly_name,
				      comp_class => $source->comp_class );

    my @newfiles = ($file);

    if (defined $file && !-f $file) {
	my ($dirname) = dirname($file);
	if (!-d $dirname) {
	    unlink($dirname) if (-e _);
	    push @newfiles, mkpath($dirname, 0, 0775);
	    system_error "Couldn't create directory $dirname: $!"
		unless -d $dirname;
	}
	rmtree($file) if (-d $file);
    }

    ($file) = $file =~ /^(.*)/s if taint_is_on;  # Untaint blindly

    my $fh = make_fh();
    open $fh, "> $file"
	or system_error "Couldn't write object file $file: $!";
    print $fh $$object_code
	or system_error "Couldn't write object file $file: $!";
    close $fh 
	or system_error "Couldn't close object file $file: $!";
    
    return \@newfiles;
}

sub object_id
{
    my $self = shift;

    local $self->{comp_class} = '';

    return $self->SUPER::object_id;
}

sub compiled_component
{
    my $self = shift;

    $self->{compiled_def} = $self->_compile_subcomponents if %{ $self->{def} };
    $self->{compiled_method} = $self->_compile_methods if %{ $self->{method} };

    $self->{current_comp} = $self;

    my $header = $self->_make_main_header;
    my $params = $self->_component_params;

    my $id = $self->object_id;
    $id =~ s,([\\']),\\$1,g;
    $params->{compiler_id} = "'$id'";
    $params->{load_time} = time;

    $params->{subcomps} = '\%_def' if %{ $self->{def} };
    $params->{methods} = '\%_method' if %{ $self->{method} };

    if ( $self->_blocks('shared') )
    {
	my %subs;
	while ( my ($name, $pref) = each %{ $self->{compiled_def} } )
	{
	    my $key = "subcomponent_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic('$key',\@_)\n}";
	}
	while (my ($name, $pref) = each %{ $self->{compiled_method} } )
	{
	    my $key = "method_$name";
	    $subs{$key} = $pref->{code};
	    $pref->{code} = "sub {\n\$m->call_dynamic( '$key', \@_ )\n}";
	}
	$subs{main} = $params->{code};
	$params->{code} = "sub {\n\$m->call_dynamic( 'main', \@_ )\n}";

	$params->{dynamic_subs_init} =
	    join '', ( "sub {\n",
		       $self->_blocks('shared'),
		       "return {\n",
		       map( "'$_' => $subs{$_},\n", sort keys %subs ),
		       "\n}\n}"
		     );
    }

    $params->{object_size} = 0;
    $params->{object_size} += length for ($header, %$params);

    my $obj_text = join('',
			"# MASON COMPILER ID: $id\n",
			$header,
			$self->_subcomponents_footer,
			$self->_methods_footer,
			$self->_constructor( $self->comp_class,
					     $params ),
			';');
    delete $self->{current_comp};
    return \$obj_text;
}

sub assert_creatorship
{
    my ($self, $p) = @_;
    my $id;
    if ($p->{object_code}) {
	# Read the object code as a string

	($id) = ${$p->{object_code}} =~ /\A# MASON COMPILER ID: (\S+)$/m
	    or wrong_compiler_error "Couldn't find a Compiler ID in compiled code.";
    } else {
	# Open the object file and read its first line

	my $fh = make_fh();
	open $fh, $p->{object_file} or die "Can't read $p->{object_file}: $!";
	($id) = <$fh> =~ /\A# MASON COMPILER ID: (\S+)$/m
	    or wrong_compiler_error "Couldn't find a Compiler ID in $p->{object_file}.";
	close $fh;
    }
    
    wrong_compiler_error 'This object file was created by an incompatible Compiler or Lexer.  Please remove the component files in your object directory.'
	unless $id eq $self->object_id;
}

sub _compile_subcomponents
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('def');
}

sub _compile_methods
{
    my $self = shift;

    return $self->_compile_subcomponents_or_methods('method');
}

sub _compile_subcomponents_or_methods
{
    my $self = shift;
    my $type = shift;

    my %compiled;
    foreach ( keys %{ $self->{$type} } )
    {
	$self->{current_comp} = $self->{$type}{$_};
	$compiled{$_} = $self->_component_params;
    }

    return \%compiled;
}

sub _make_main_header
{
    my $self = shift;

    my $pkg = $self->in_package;

    return join '', ( "package $pkg;\n",
		      $self->use_strict ? "use strict;\n" : "no strict;\n",
		      sprintf( "use vars qw(\%s);\n",
			       join ' ', '$m', $self->allow_globals ),
		      $self->_blocks('once'),
		    );
}

sub _subcomponents_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('def');
}

sub _methods_footer
{
    my $self = shift;

    return $self->_subcomponent_or_method_footer('method');
}

sub _subcomponent_or_method_footer
{
    my $self = shift;
    my $type = shift;

    return '' unless %{ $self->{current_comp}{$type} };

    return join('',
		"my %_$type =\n(\n",
		map( {("'$_' => " ,
		       $self->_constructor( $self->{subcomp_class},
					    $self->{"compiled_$type"}{$_} ) ,
		       ",\n")} keys %{ $self->{"compiled_$type"} } ) ,
		"\n);\n"
	       );
}

sub _constructor
{
    my ($self, $class, $params) = @_;

    return ("${class}->new(\n",
	    map( {("'$_' => ", $params->{$_}, ",\n")} sort keys %$params ),
	    "\n)\n",
	   );
}

sub _component_params
{
    my $self = shift;

    my %params = ( code => join ( '', "sub {\n", $self->_body, "}" ),
		 );

    $params{flags} = join '', "{\n", $self->_flags, "\n}"
        if keys %{ $self->{current_comp}{flags} };

    $params{attr}  = join '', "{\n", $self->_attr, "\n}"
        if keys %{ $self->{current_comp}{attr} };

    $params{declared_args} = join '', "{\n", $self->_declared_args, "\n}"
	if @{ $self->{current_comp}{args} };

    $params{has_filter} = 1 if $self->_blocks('filter');

    return \%params;
}

sub _body
{
    my $self = shift;

    my @args;
    if ( @{ $self->{current_comp}{args} } )
    {
	@args = ( <<'EOF',
if (@_ % 2 == 0) { %ARGS = @_ } else { HTML::Mason::Exception::Params->throw( error => "Odd number of parameters passed to component expecting name/value pairs" ) }
EOF
		  $self->_arg_declarations,
		);
    }
    else
    {
	@args = ( "{ local \$^W; \%ARGS = \@_ unless (\@_ % 2); }\n" );
    }

    return join '', ( $self->preamble,
		      "my \%ARGS;\n",
		      @args,
                      $self->_filter,
		      "\$m->debug_hook( \$m->current_comp->path ) if ( \%DB:: );\n\n",
		      $self->_blocks('init'),
		      $self->{current_comp}{body},
		      $self->_blocks('cleanup'),
		      $self->postamble,
		      "return undef;\n",
		    );
}

my %coercion_funcs = ( '@' => 'HTML::Mason::Tools::coerce_to_array',
		       '%' => 'HTML::Mason::Tools::coerce_to_hash',
		     );
sub _arg_declarations
{
    my $self = shift;

    my @decl;
    my @assign;
    my @required;

    foreach ( @{ $self->{current_comp}{args} } )
    {
	my $var_name = "$_->{type}$_->{name}";
	push @decl, $var_name;

	my $coerce;
	if ( $coercion_funcs{ $_->{type} } )
	{
	    $coerce = $coercion_funcs{ $_->{type} } . "(\$ARGS{'$_->{name}'}, '$var_name')";
	}
	else
	{
	    $coerce = "\$ARGS{'$_->{name}'}";
	}

	push @assign, "#line $_->{line} $_->{file}\n"
	    if defined $_->{line} && defined $_->{file};
	if ( defined $_->{default} )
	{
	    my $default_val = $_->{default};
	    # allow for comments after default declaration
	    $default_val .= "\n" if defined $_->{default} && $_->{default} =~ /\#/;

	    push @assign,
		"$_->{type}$_->{name} = exists \$ARGS{'$_->{name}'} ? $coerce : $default_val;\n";
	}
	else
	{
	    push @required, $_->{name};

	    push @assign,
		"$var_name = $coerce;\n";
	}
    }

    my @req_check;
    if (@required)
    {
        # just to be sure
        local $" = ' ';
        @req_check = <<"EOF";

foreach my \$arg ( qw( @required ) )
{
    HTML::Mason::Exception::Params->throw
        ( error => "no value sent for required parameter '\$arg'" )
        unless exists \$ARGS{\$arg};
}
EOF
    }

    my $decl = 'my ( ';
    $decl .= join ', ', @decl;
    $decl .= " );\n";

    return @req_check, $decl, @assign;
}

sub _filter
{
    my $self = shift;

    my @filter;
    @filter = $self->_blocks('filter')
        or return;

    return ( join '',
             "\$m->current_comp->filter( sub { local \$_ = shift;\n",
             ( join ";\n", @filter ),
             ";\n",
             "return \$_;\n",
             "} );\n",
           );

}

sub _flags
{
    my $self = shift;

    return $self->_flags_or_attr('flags');
}

sub _attr
{
    my $self = shift;

    return $self->_flags_or_attr('attr');
}

sub _flags_or_attr
{
    my $self = shift;
    my $type = shift;

    return join "\n,", ( map { "$_ => $self->{current_comp}{$type}{$_}" }
			 keys %{ $self->{current_comp}{$type} } );
}

sub _declared_args
{
    my $self = shift;

    my @args;

    foreach my $arg ( sort {"$a->{type}$a->{name}" cmp "$b->{type}$b->{name}" }
		      @{ $self->{current_comp}{args} } )
    {
	my $def = defined $arg->{default} ? "$arg->{default}" : 'undef';
	$def =~ s,([\\']),\\$1,g;
	$def = "'$def'" unless $def eq 'undef';

	push @args, "  '$arg->{type}$arg->{name}' => { default => $def }";
    }

    return join ",\n", @args;
}

1;

__END__

=head1 NAME

HTML::Mason::Compiler::ToObject - A Compiler subclass that generates Mason object code

=head1 SYNOPSIS

  my $compiler = HTML::Mason::Compiler::ToObject->new;

  my $object_code = $compiler->compile( comp_source => $source, name => $comp_name );

=head1 DESCRIPTION

This Compiler subclass generates Mason object code (Perl code).  It is
the default Compiler class used by Mason.

=head1 PARAMETERS TO THE new() CONSTRUCTOR

All of these parameters are optional.

=over

=item comp_class

The class into which component objects are blessed.  This defaults to
L<HTML::Mason::Component|HTML::Mason::Component>.

=item subcomp_class

The class into which subcomponent objects are blessed.  This defaults
to L<HTML::Mason::Component::Subcomponent|HTML::Mason::Component::Subcomponent>.

=item in_package

This is the package in which a component's code is executed.  For
historical reasons, this defaults to C<HTML::Mason::Commands>.

=item preamble

Text given for this parameter is placed at the beginning of each component. See also P<postamble>.

=item postamble

Text given for this parameter is placed at the end of each component. See also P<preamble>.

=item use_strict

True or false, default is true. Indicates whether or not a given
component should C<use strict>.

=back

=head1 METHODS

This class is primarily meant to be used by the Interpreter object,
and as such has a very limited public API.

=over

=item compile (comp_source => $source, name => $name, comp_class = $comp_class)

This method will take component source and return the compiled object
code for that source.  The C<comp_source> and C<name> parameters are
optional.  The C<comp_class> can be used to change the component class
for this one comonent.

=back

=cut
