#!/usr/bin/perl -w
use Cwd;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "parser";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";
init();

sub try_parse {
    my ($options,$test,$expect_error,$error_pattern) = @_;
    my ($err,$comp);
    my $failed = sub {
	print "parsing $test";
	print " with options ".join(",",%$options) if %$options;
	print ": ";
	if ($expect_error) {
	    print "did not get expected error\n";
	} else {
	    print "got unexpected error:\n$err\n";
	}
	print "not ok\n";
	'';
    };
    my $parser = new HTML::Mason::Parser (%$options);
    my $obj_text = $parser->parse_component (script_file=>"$root/test/comps/parser/$test",error=>\$err);
    if ($obj_text) {
	$comp = $parser->eval_object_text(object_text=>$obj_text, error=>\$err);
    }
    if ($expect_error) {
	print ((!$err or ($error_pattern and $err !~ /$error_pattern/)) ? &$failed : "ok\n");
    } else {
	print (($err) ? &$failed : "ok\n");
    }
}

sub try_exec_with_parser {
    my ($options,$test,$iteration) = @_;
    # Create new parser and interp based on parser options.
    # Turn off object files so that we recompile every time.
    my $parser = new HTML::Mason::Parser (%$options);
    my $interp = new HTML::Mason::Interp(parser => $parser, comp_root => $comp_root, data_dir => $data_dir, use_object_files => 0);
    try_exec($interp,$test,$iteration);
}

print "1..13\n";

# allow_globals
undef(*HTML::Mason::Commands::global);
try_parse({},'allow_globals',1,'Global symbol .* requires explicit package name');
undef(*HTML::Mason::Commands::global);
try_parse({},'allow_globals',1,'Global symbol .* requires explicit package name');
undef(*HTML::Mason::Commands::global);
try_parse({allow_globals=>[qw($global)]},'allow_globals',0);

# default_escape_flags
try_exec_with_parser({},'default_escape_flags',1);
try_exec_with_parser({default_escape_flags=>'h'},'default_escape_flags',2);

# ignore_warnings_expr: Can't come up with a good example to test!

# in_package
try_exec_with_parser({allow_globals=>[qw($packvar)]},'in_package',1);
try_exec_with_parser({allow_globals=>[qw($packvar)],in_package=>'HTML::Mason::NewPackage'},'in_package',2);

# preamble/postamble
try_exec_with_parser({postamble=>'my $msg = "This is the postamble.\n"; $m->out($msg);'},'prepost',1);
try_exec_with_parser({preamble=>'my $msg = "This is the preamble.\n"; $m->out($msg);'},'prepost',2);

# preprocessing the component
sub bracket_to_lt_gt
{
    my $comp = shift;
    ${ $comp } =~ s/\[\%(.*?)\%\]/<\%$1\%>/g;
}

try_exec_with_parser( { preprocess => \&bracket_to_lt_gt }, 'preprocess', 1 );

# postprocessing alpha/perl code
sub uc_alpha
{
    return unless pop eq 'alpha';
    ${ $_[0] } = uc ${ $_[0] };
}

sub add_foo_to_perl
{
    return unless pop eq 'perl';
    ${ $_[0] } =~ s/\);/. 'FOO');/;
}

try_exec_with_parser( { postprocess => \&uc_alpha }, 'postprocess', 1 );
try_exec_with_parser( { postprocess => \&add_foo_to_perl }, 'postprocess', 2 );

# check that only valid variable names are allowed
try_parse( {}, 'bad_var_name', 1, 'Invalid variable name' );

1;
