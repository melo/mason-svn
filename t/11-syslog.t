#!/usr/bin/perl -w
use Cwd;
use vars (qw($root $branch $comp_root $data_dir));

$branch = "interp";
my $pwd = cwd();
$root = (-f "test-common.pl") ? "$pwd/.." : (-f "t/test-common.pl") ? "$pwd" : die "ERROR: cannot find test-common.pl\n";
unshift(@INC,"$root/lib");

require "$root/t/test-common.pl";
init();

sub try_exec_with_interp {
    my ($path,$options,@patterns) = @_;

    # Clear system log file.
    my $sys_log_file = $options->{system_log_file} || "$root/test/data/etc/system.log";
    unlink($sys_log_file);
    
    # Create new interp based on options.
    my $buf;
    my $interp = new HTML::Mason::Interp(comp_root => $comp_root, data_dir => $data_dir, %$options);

    # Execute request.
    $interp->out_method(\$buf);
    eval { $interp->exec($path); };
    
    # This line wards off "Attempt to free unreferenced scalar during global
    # destruction" associated with system log files.
    undef $interp->{system_log_fh};

    if (my $err = $@) {
	print "ERROR:\n$err\nnot ok\n";
	return;
    }

    # Check if log contents match pattern.
    my $fh = new IO::File $sys_log_file;
    unless ($fh) {
	print "ERROR: Cannot open $sys_log_file for reading.\nnot ok\n";
	return;
    }
    my $content = do { local $/; <$fh> };
    foreach my $pattern (@patterns) {
	unless ($content =~ /$pattern/) {
	    print "ERROR: Log contents do not match pattern.\nnot ok\n";
	    return;
	}
    }

    print "ok\n";
}

print "1..3\n";

# autohandler_name/allow_recursive_autohandlers
try_exec_with_interp("/syntax/amper",{system_log_events=>'COMP_LOAD'},"COMP_LOAD.*/support/amper_test");
try_exec_with_interp("/syntax/amper",{system_log_file=>"$root/test/data/etc/alt.log",system_log_events=>'COMP_LOAD'},"COMP_LOAD.*/support/amper_test");
try_exec_with_interp("/cache/cache",{system_log_events=>'CACHE'},"CACHE_READ");

1;
