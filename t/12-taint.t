#!/usr/bin/perl -wT

use strict;

BEGIN
{
    $ENV{PATH} = '';

    my $libs = 'use lib qw( ';
    $libs .= join ' ', "./blib/lib", "./t/lib";
    if ($ENV{PERL5LIB})
    {
	$libs .= ' ';
	$libs .= join ' ', (split /:|;/, $ENV{PERL5LIB});
    }
    $libs .= ' );';

    ($libs) = $libs =~ /(.*)/;

    # explicitly use these because otherwise taint mode causes them to
    # be ignored
    eval $libs;
}

use Cwd;

use HTML::Mason::Parser;
use HTML::Mason::Interp;
use HTML::Mason::Tools qw(read_file);

# Clear alarms, and skip test if alarm not implemented
eval {alarm 0};
if ($@) {
    print "1..0\n";
    exit;
}

print "1..1\n";

my $parser = HTML::Mason::Parser->new;

my $alarm;
$SIG{ALRM} = sub { $alarm = 1; die "alarm"; };

my $comp;
eval { alarm 5; local $^W; $comp = $parser->parse_component( script_file => 't/taint.comp' ); };

if ( $alarm || $@ || ! defined $comp )
{
    print "Taint test failed: ";
    if ($alarm)
    {
	print "entered endless while loop\n";
    }
    elsif ($@)
    {
	print "gave error during test: $@\n";
    }
    else
    {
	print "returned an undefined value from parsing\n";
    }
    print "not ok 1\n";
}
else
{
    print "ok 1\n";
}

# return val of getcwd() is tainted
my $data_dir = join '/', getcwd(), 'mason_tests', 'data';

# this is tainted
my $comp2 = read_file('t/taint.comp');
eval { $parser->write_object_file( object_text => $comp2,
				   object_file => "$data_dir/taint_write_test",
				 ); };

if (! $@)
{
    print "ok 2\n";
}
else
{
    print "not ok 2 - Unable to write a tainted object file to disk: $@\n";
}
