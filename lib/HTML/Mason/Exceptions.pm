package HTML::Mason::Exceptions;

use strict;

use vars qw($VERSION);

$VERSION = sprintf '%2d.%02d', q$Revision$ =~ /(\d+)\.(\d+)/;

my %e;

BEGIN
{
    %e = ( 'HTML::Mason::Exception' =>
	   { description => 'generic base class for all Mason exceptions',
	     abbr => 'error'},

	   'HTML::Mason::Exception::Abort' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'abort_error',
	     fields => [qw(aborted_value)],
	     description => 'a component called $m->abort' },

	   'HTML::Mason::Exception::Compiler' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'compiler_error',
	     description => 'error thrown from the compiler' },

	   'HTML::Mason::Exception::Compilation' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'compilation_error',
	     fields => [qw(filename)],
	     description => "error thrown in eval of the code for a component" },

	   'HTML::Mason::Exception::Compilation::IncompatibleCompiler' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'wrong_compiler_error',
	     description => "a component was compiled by a compiler/lexer with incompatible options.  recompilation is needed" },

	   'HTML::Mason::Exception::Params' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'param_error',
	     description => 'invalid parameters were given to a method/function' },

	   'HTML::Mason::Exception::Syntax' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'syntax_error',
	     fields => [qw(source_line comp_name line_number)],
	     description => 'invalid syntax was found in a component' },

	   'HTML::Mason::Exception::System' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'system_error',
	     description => 'a system call of some sort failed' },

	   'HTML::Mason::Exception::TopLevelNotFound' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'top_level_not_found_error',
	     description => 'the top level component could not be found' },

	   'HTML::Mason::Exception::VirtualMethod' =>
	   { isa => 'HTML::Mason::Exception',
	     abbr => 'virtual_error',
	     description => 'a virtual method was not overridden' },

	 );
}

use Exception::Class (%e);

HTML::Mason::Exception->Trace(1);

my %abbrs = map { $e{$_}{abbr} => $_ } grep {exists $e{$_}{abbr}} keys %e;

# The import() method allows this:
#  use HTML::Mason::Exceptions(abbr => ['error1', 'error2', ...]);
# ...
#  error1 "something went wrong";

sub import
{
    my ($class, %args) = @_;

    my $caller = caller;
    if ($args{abbr})
    {
	foreach my $name (@{$args{abbr}})
	{
	    die "Unknown exception abbreviation '$name'" unless exists $abbrs{$name};
	    no strict 'refs';
	    *{"${caller}::$name"} = sub { $abbrs{$name}->throw( error => shift ) };
	}
    }
    {
	no strict 'refs';
	*{"${caller}::isa_mason_exception"} = \&isa_mason_exception;
    }
}

sub isa_mason_exception
{
    my ($err, $name) = @_;
    return unless defined $err;

    if ($name) {
	my $class = "HTML::Mason::Exception::$name";
	no strict 'refs';
	die "no such exception class $class" unless defined(${"${class}::VERSION"});
	return UNIVERSAL::isa($err, "HTML::Mason::Exception::$name");
    } else {
	return UNIVERSAL::isa($err, "HTML::Mason::Exception");
    }
}

package HTML::Mason::Exception;

use HTML::Mason::MethodMaker
    ( read_write => [ qw ( format ) ] );

sub new
{
    my ($class, %params) = @_;

    my $self = $class->SUPER::new(%params);
    $self->format('text');
    return $self;
}

# If we create a new exception from a Mason exception, just use the
# short error message, not the stringified exception. Otherwise
# exceptions can get stringified more than once.
sub throw
{
    my ($class, %params) = @_;

    if (HTML::Mason::Exceptions::isa_mason_exception($params{error})) {
	$params{error} = $params{error}->error;
    }
    if (HTML::Mason::Exceptions::isa_mason_exception($params{message})) {
	$params{message} = $params{message}->error;
    }
    $class->SUPER::throw(%params);
}

sub filtered_frames
{
    my ($self) = @_;

    my (@frames);
    my $trace = $self->trace;
    my %ignore_subs = map { $_ => 1 }
	qw[
	   (eval)
	   Exception::Class::Base::throw
	   HTML::Mason::Commands::__ANON__
	   HTML::Mason::Component::run
	   HTML::Mason::Exceptions::__ANON__
	   HTML::Mason::Request::_run_comp
	   ];
    while (my $frame = $trace->next_frame)
    {
	last if ($frame->subroutine eq 'HTML::Mason::Request::exec');
	unless ($frame->filename =~ /Mason\/Exceptions\.pm/ or
		$ignore_subs{ $frame->subroutine } or
		($frame->subroutine eq 'HTML::Mason::Request::comp' and $frame->filename =~ /Request\.pm/)) {
	    push(@frames, $frame);
	}
    }
    @frames = grep { $_->filename !~ /Mason\/Exceptions\.pm/ } $trace->frames if !@frames;
    return @frames;
}

sub analyze_error
{
    my ($self) = @_;
    my ($file, @lines, @frames);

    return $self->{_info} if $self->{_info};

    @frames = $self->filtered_frames;
    if ($self->isa('HTML::Mason::Exception::Syntax')) {
	$file = $self->comp_name;
	push(@lines, $self->line_number);
    } elsif ($self->isa('HTML::Mason::Exception::Compilation')) {
	$file = $self->filename;
	my $msg = $self->full_message;
	while ($msg =~ /at .* line (\d+)./g) {
	    push(@lines, $1);
	}
    } elsif (@frames) {
	$file = $frames[0]->filename;
	@lines = $frames[0]->line;
    }
    my @context;
    @context = $self->get_file_context($file, \@lines) if @lines;

    $self->{_info} = {
	file    => $file,
	frames  => \@frames,
	lines   => \@lines,
	context => \@context,
    };
    return $self->{_info};
}

sub get_file_context
{
    my ($self, $file, $line_nums) = @_;

    my @context;
    my $fh = do { local *FH; *FH; };
    unless (defined($file) and open($fh, $file)) {
	@context = (['unable to open file', '']);
    } else {
	# Put the file into a list, indexed at 1.
	my @file = <$fh>;
	chomp(@file);
	unshift(@file, undef);

	# Mark the important context lines.
	# We do this by going through the error lines and incrementing hash keys to
	# keep track of which lines we eventually need to print, and we color the
	# line which the error actually occured on in red.
	my (%marks, %red);
	my $delta = 4;
	foreach my $line_num (@$line_nums) {
	    foreach my $l (($line_num - $delta) .. ($line_num + $delta)) {
		next if ($l <= 0 or $l > @file);
		$marks{$l}++;
	    }
	    $red{$line_num} = 1;
	}

	# Create the context list.
	# By going through the keys of the %marks hash, we can tell which lines need
	# to be printed. We add a '...' line if we skip numbers in the context.
	my $last_num = 0;
	foreach my $line_num (sort { $a <=> $b } keys %marks) {
	    push(@context, ["...", "", 0]) unless $last_num == ($line_num - 1);
	    push(@context, ["$line_num:", $file[$line_num], $red{$line_num}]);;
	    $last_num = $line_num;
	}
	push(@context, ["...", "", 0]) unless $last_num == @file;
	close $fh;
    }
    return @context;
}

sub as_string
{
    my ($self) = @_;

    my $stringify_function = "as_" . $self->{format};
    return $self->$stringify_function();
}

sub as_brief
{
    my ($self) = @_;
    return $self->full_message;
}

sub as_line
{
    my ($self) = @_;
    my $info = $self->analyze_error;

    (my $msg = $self->full_message) =~ s/\n/\t/g;
    my $stack = join(", ", map { sprintf("[%s:%d]", $_->filename, $_->line) } @{$info->{frames}});
    return sprintf("%s\tStack: %s\n", $msg, $stack);
}

sub as_text
{
    my ($self) = @_;
    my $info = $self->analyze_error;

    my $msg = $self->full_message;
    my $stack = join("\n", map { sprintf("  [%s:%d]", $_->filename, $_->line) } @{$info->{frames}});
    return sprintf("%s\nStack:\n%s\n", $msg, $stack);
}

sub as_html
{
    my ($self) = @_;
    my $info = $self->analyze_error;

    my $out;
    my $msg = HTML::Mason::Tools::html_escape($self->full_message);
    $msg =~ s/\n/<br>/g;
    $out .= qq[<html><body>\n];
    $out .= qq[<p align="center"><font face="Verdana, Arial, Helvetica, sans-serif"><b>System error</b></font></p>\n];
    $out .= qq[<table border="0" cellspacing="0" cellpadding="1">\n];
    $out .= qq[<tr>];
    $out .= qq[<td nowrap align="left" valign="top"><b>error:</b>&nbsp;</td>\n];
    $out .= sprintf(qq[<td align="left" valign="top" nowrap>%s</td>\n], $msg);
    $out .= qq[</tr>\n];
    $out .= qq[<tr>\n];
    $out .= qq[<td nowrap align="left" valign="top"><b>context:</b>&nbsp;</td>\n];
    $out .= qq[<td align="left" valign="top" nowrap><table border="0" cellpadding="0" cellspacing="0">\n];
    foreach my $entry (@{$info->{context}}) {
	my ($line_num, $line, $highlight) = @$entry;
	$line = HTML::Mason::Tools::html_escape($line);
	$line =~ s/ /&nbsp;/g if defined $line;
	$out .= qq[<tr>\n];
	$out .= sprintf(qq[<td nowrap align="left" valign="top"><b>%s</b>&nbsp;</td>\n], $line_num);
	$out .= sprintf(qq[<td align="left" valign="top" nowrap>%s%s%s</td>\n],
			($highlight ? "<font color=red>" : ""),
			defined $line ? $line : '',
			($highlight ? "</font>" : ""));
	$out .= qq[</tr>\n];
    }
    $out .= qq[</table></td></tr>\n];
    $out .= qq[<tr>\n];
    $out .= qq[<td align="left" valign="top" nowrap><b>code stack:</b>&nbsp;</td>\n];
    $out .= qq[<td align="left" valign="top" nowrap><table border="0" cellpadding="0" cellspacing="0">\n];
    foreach my $frame (@{$info->{frames}}) {
	$out .= sprintf(qq[%s:%d\n<br>], HTML::Mason::Tools::html_escape($frame->filename), HTML::Mason::Tools::html_escape($frame->line));
    }
    $out .= qq[</td>\n];
    $out .= qq[</tr>\n];
    $out =~ s/(<td [^\>]+>)/$1<font face="Verdana, Arial, Helvetica, sans-serif" size="-2">/g;
    $out =~ s/<\/td>/<\/font><\/td>/g;

    return $out;
}

package HTML::Mason::Exception::Compilation;

sub full_message
{
    my $self = shift;

    return sprintf("Error during compilation of %s:\n%s\n", $self->filename || '', $self->message || '');
}

package HTML::Mason::Exception::Syntax;

sub full_message
{
    my $self = shift;

    return sprintf("%s at %s line %d", $self->message || '', $self->comp_name || '', $self->line_number);
}

1;

__END__

=head1 NAME

HTML::Mason::Exceptions - Exception objects thrown by Mason

=head1 DESCRIPTION

  use HTML::Mason::Exceptions qw(system_error);

  open FH, 'foo' or system_error "cannot open foo: $!";

=head1 IMPORT

When this module is imported, it is possible to specify a list of
abbreviated function names that you want to use to throw exceptions.
In the L<SYNOPSIS|/SYNOPSIS> example, we use the C<system_error>
function to throw a C<HTML::Mason::Exception::System> exception.

These abbreviated functions do not allow you to set additional fields
in the exception, only the message.

=head1 EXCEPTIONS

=over

=item HTML::Mason::Exception

This is the parent class for all exceptions thrown by Mason.  Mason
sometimes throws exceptions in this class when we could not find a
better category for the message.

Abbreviated as C<error>

=item HTML::Mason::Exception::Abort

The C<< $m->abort >> method was called.

Exceptions in this class contain the field C<aborted_value>.

Abbreviated as C<abort_error>.

=item HTML::Mason::Exception::Compilation

An exception occurred when attempting to C<eval> an existing object
file.

Exceptions in this class have the field C<filename>, which indicates
what file contained the code that caused the error.

Abbreviated as C<compilation_error>.

=item HTML::Mason::Exception::Compiler

The compiler threw an exception because it received incorrect input.
For example, this would be thrown if the lexer told the compiler to
initialize compilation while it was in the middle of compiling another
component.

Abbreviated as C<compiler_error>.

=item HTML::Mason::Exception::Compilation::IncompatibleCompiler

A component was compiled by a compiler or lexer with incompatible
options.  This is used to tell Mason to recompile a component.

Abbreviated as C<wrong_compiler_error>.

=item HTML::Mason::Exception::Params

Invalid parameters were passed to a method or function.

Abbreviated as C<param_error>.

=item HTML::Mason::Exception::Syntax

This exception indicates that a component contained invalid syntax.

Exceptions in this class have the fields C<source_line>, which is the
actual source where the error was found, C<comp_name>, and
C<line_number>.

Abbreviated as C<syntax_error>.

=item HTML::Mason::Exception::System

A system call of some sort, such as a file open, failed.

Abbreviated as C<system_error>.

=item HTML::Mason::Exception::TopLevelNotFound

The requested top level component could not be found.

Abbreviated as C<top_level_not_found_error>.

=item HTML::Mason::VirtualMethod

Some piece of code attempted to call a virtual method which was not
overridden.

Abbreviated as C<virtual_error>

=back

=head1 FIELDS

Some of the exceptions mentioned above have additional fields, which
are available via accessors.  For example, to get the line number of
an C<HTML::Mason::Exception::Syntax> exception, you call the
C<line_number> method on the exception object.

=head1 EXCEPTION METHODS

All of the Mason exceptions implement the following methods:

=over

=item as_brief

This simply returns the exception message, without any trace information.

=item as_line

This returns the exception message and its trace information, all on a
single line with tabs between the message and each frame of the stack
trace.

=item as_text

This returns the exception message and stack information, with each
frame on a separate line.

=item as_html

This returns the exception message and stack as an HTML page.

=back

Each of these methods corresponds to a valid error_format parameter
for the L<Request object|HTML::Mason::Request> such as C<text> or
C<html>.

You can create your own method in the C<HTML::Mason::Exception>
namespace, such as C<as_you_wish>, in which case you could set this
parameter to "you_wish".  This method will receive a single argument,
the exception object, and is expected to return some sort of string
containing the formatted error message.

=head1 EXCEPTION CLASS CHECKING

This module also exports the C<isa_mason_exception> function.  This
function takes the exception object and an optional string parameter
indicating what subclass to check for.

So it can be called either as:

  if ( isa_mason_exception($@) ) { ... }

or

  if ( isa_mason_exception($@, 'Syntax') ) { ... }

Note that when specifying a subclass you should not include the
leading "HTML::Mason::Exception::" portion of the class name.

=cut
