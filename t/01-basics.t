# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

use HTML::Mason;
use Cwd;
use strict;

######################### End of black magic.

my $pwd = cwd();
my $comp_root = "$pwd/test/comps";
my $data_dir = "$pwd/test/data";
my $results_dir = "test/results/";
my $tmp_dir = "test/tmp/";

my $buf;

my @comps = qw(replace init perl_init args perl_args doc perl_doc filter once
               text perl percent amper mc_cache mc_cache_self);

my $parser = new HTML::Mason::Parser;
my $interp = new HTML::Mason::Interp( parser=>$parser,
                                      comp_root => $comp_root,
                                      data_dir => $data_dir,
                                      out_method => \$buf, );

print "1..15\n";

foreach my $component ( @comps ) {
    undef $buf;
    my $result;
    eval { $interp->exec("/$component"); };
    my $err = $@;
    print STDERR "\n-----\nERROR during '$component' test\n$err-----\n" if $err;

    open(F, ">$tmp_dir$component");
    print F $buf if defined($buf);
    close F;
    my $error = compareFiles("$tmp_dir$component", "$results_dir$component");
    print ( $error == 0 ? "ok\n" : "not ok\n" );
}


# The file comparison subroutines below were taken from Gerald Richter's
# HTML::Embperl package.

sub chompcr {
    chomp ($_[0]) ;
    if ($_[0] =~ /(.*?)\s*\r$/) {
	$_[0] = $1;
    } elsif ($_[0] =~ /(.*?)\s*$/) {
	$_[0] = $1;
    }
}

sub compareFiles {
    my ($f1, $f2, $errin) = @_;
    my $line = 1;
    my $err  = 0;
    my($l1, $l2, $eq);

    open F1, $f1 || die "***Cannot open $f1"; 
    if (!$errin) {
	open F2, $f2 || die "***Cannot open $f2"; 
    }

    while (defined ($l1 = <F1>)) {
	chompcr ($l1);
	if (!$errin) {
	    $l2 = <F2>;
	    chompcr ($l2);
	}
	if (!defined ($l2)) {
	    print "\nError in Line $line\nIs:\t$l1\nShould:\t<EOF>\n";
	    return $line;
	}

	$eq = 0;
	while (((($l2 =~ /^\^\^(.*?)$/)) || ($l2 =~ /^\^\-(.*?)$/)) && !$eq) {
	    $l2 = $1;
	    if (($l1 =~ /^\s*$/) && ($l2 =~ /^\s*$/)) { 
		$eq = 1;
	    } else {
		$eq = $l1 =~ /$l2/;
	    }
	    $l2 = <F2> if (!$eq);
	    chompcr ($l2);
	}

	if (!$eq) {
	    if ($l2 =~ /^\^(.*?)$/) {
		$l2 = $1;
		$eq = $l1 =~ /$l2/;
	    } else {
		$eq = $l1 eq $l2;
	    }
	}

	if (!$eq) {
	    print "\nError in Line $line\nIs:\t>$l1<\nShould:\t>$l2<\n";
	    return $line;
	}
	$line++;
    }

    if (!$errin) {
	while (defined ($l2 = <F2>)) {
	    chompcr ($l2);
	    if (!($l2 =~ /^\s*$/)) {
		print "\nError in Line $line\nIs:\t\nShould:\t$l2\n";
		return $line;
	    }
	    $line++;
	}
    }

    close F1;
    close F2;

    return $err; 
}
