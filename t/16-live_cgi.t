#!/usr/bin/perl -w

use strict;

unless (-e "$ENV{APACHE_DIR}/CGIHandler.cgi")
{
    print "1..0\n";
    exit;
}

use vars qw($VERBOSE $DEBUG);

BEGIN
{
    $VERBOSE = $ENV{MASON_DEBUG} || $ENV{MASON_VERBOSE};
    $DEBUG = $ENV{MASON_DEBUG};
}

use File::Basename;
use File::Path;
use HTML::Mason::Tests;

use lib 'lib', 't/lib';

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

local $| = 1;

kill_httpd(1);
test_load_apache();

print "1..3\n";

print STDERR "\n";

write_test_comps();

# This is a hack but otherwise the following tests fail if the Apache
# server runs as any user other than root.  In real life, a user using
# the multi-config option with httpd.conf must handle the file
# permissions manually.
if ( $> == 0 || $< == 0 )
{
    chmod 0777, "$ENV{APACHE_DIR}/data";
}

run_tests();

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
EOF
	      );

    write_comp( 'headers', <<'EOF',


% $r->header_out('X-Mason-Test' => 'New value 2');
Blah blah
blah
% $r->header_out('X-Mason-Test' => 'New value 3');
<%init>
$r->header_out('X-Mason-Test' => 'New value 1');
$m->abort if $blank;
</%init>
<%args>
$blank=>0
</%args>
EOF
	      );

    write_comp( 'cgi_object', <<'EOF',
<% UNIVERSAL::isa(eval { $m->cgi_object }, 'CGI') ? 'CGI' : 'NO CGI' %>
EOF
	      );

    write_comp( 'params', <<'EOF',
% foreach (sort keys %ARGS) {
<% $_ %>: <% ref $ARGS{$_} ? join ', ', sort @{ $ARGS{$_} }, 'array' : $ARGS{$_} %>
% }
EOF
	      );

    write_comp( '_underscore', <<'EOF',
I am underscore.
EOF
	      );

    write_comp( 'dhandler/dhandler', <<'EOF',
I am the dhandler.
EOF
	      );

    write_comp( 'die', <<'EOF',
% die 'Mine heart is pierced';
EOF
	      );

    write_comp( 'apache_request', <<'EOF',
% if ($r->isa('Apache::Request')) {
Apache::Request
% }
EOF
		  );

    write_comp( 'multiconf1/foo', <<'EOF',
I am foo in multiconf1
comp root is <% $m->interp->resolver->comp_root =~ m,/comps/multiconf1$, ? 'multiconf1' : $m->interp->resolver->comp_root %>
EOF
	      );

    write_comp( 'multiconf1/autohandler', <<'EOF'
<& $m->fetch_next, autohandler => 'present' &>
EOF
	      );

    write_comp( 'multiconf1/autohandler_test', <<'EOF'
<%args>
$autohandler => 'absent'
</%args>
autohandler is <% $autohandler %>
EOF
	      );


    write_comp( 'multiconf2/foo', <<'EOF',
I am foo in multiconf2
comp root is <% $m->interp->resolver->comp_root =~ m,/comps/multiconf2$, ? 'multiconf2' : $m->interp->resolver->comp_root %>
EOF
	      );

    write_comp( 'multiconf2/dhandler', <<'EOF',
This should not work
EOF
	      );

    write_comp( 'allow_globals', <<'EOF',
% $foo = 1;
% @bar = ( qw( a b c ) );
$foo is <% $foo %>
@bar is <% @bar %>
EOF
	      );

    write_comp( '__top_level_predicate', <<'EOF',
Shouldn't ever run
EOF
	      );

    write_comp( 'decline_dirs', <<'EOF',
decline_dirs is <% $m->ah->decline_dirs %>
EOF
	      );

    write_comp( 'print', <<'EOF',
This is first.
% print "This is second.\n";
This is third.
EOF
	      );

    write_comp( 'r_print', <<'EOF',
This is first.
% $r->print("This is second.\n");
This is third.
EOF
	      );

    write_comp( 'flush_buffer', <<'EOF',
% $m->out("foo\n");
% $m->flush_buffer;
bar
EOF
	      );

    write_comp( 'head_request', <<'EOF',
<%init>
my $x = 1;
foreach (keys %ARGS) {
  $r->header_out( 'X-Mason-HEAD-Test' . $x++ => "$_: " . (ref $ARGS{$_} ? 'is a ref' : 'not a ref' ) );
}
</%init>
We should never see this.
EOF
	      );
}

sub write_comp
{
    my $name = shift;
    my $comp = shift;

    my $file = "$ENV{APACHE_DIR}/comps/$name";
    my $dir = dirname($file);
    mkpath( $dir, 0, 0755 ) unless -d $dir;

    open F, ">$file"
	or die "Can't write to '$file': $!";

    print F $comp;

    close F;
}

# by wiping out the subdirectories here we can catch permissions
# issues if some of the tests can't write to the data dir.
sub cleanup_data_dir
{
    local *DIR;
    opendir DIR, "$ENV{APACHE_DIR}/data"
	or die "Can't open $ENV{APACHE_DIR}/data dir: $!";
    foreach ( grep { -d "$ENV{APACHE_DIR}/data/$_" && $_ !~ /^\./ } readdir DIR )
    {
	rmtree("$ENV{APACHE_DIR}/data/$_");
    }
    closedir DIR;
}

sub run_tests
{
    start_httpd();

    {
	my $path = '/comps/basic';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
Basic test.
2 + 2 = 4.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/print/stream';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
						      );

	ok($success);
    }

    kill_httpd();
}

sub get_pid {
    local *PID;
    open PID, "$ENV{APACHE_DIR}/httpd.pid"
	or die "Can't open '$ENV{APACHE_DIR}/httpd.pid': $!";
    my $pid = <PID>;
    close PID;
    chomp $pid;
    return $pid;
}

sub test_load_apache
{
    print STDERR "\nTesting whether Apache can be started\n";
    start_httpd('');
    kill_httpd(1);
}

sub start_httpd
{
    my $cmd ="$ENV{APACHE_DIR}/httpd -DCGIHandler -f $ENV{APACHE_DIR}/httpd.conf";
    print STDERR "Executing $cmd\n";
    system ($cmd)
	and die "Can't start httpd server as '$cmd': $!";

    my $x = 0;
    print STDERR "Waiting for httpd to start.\n";
    until ( -e 't/httpd.pid' )
    {
	sleep (1);
	$x++;
	if ( $x > 10 )
	{
	    die "No t/httpd.pid file has appeared after 10 seconds.  ",
		"There is probably a problem with the configuration file that was generated for these tests.";
	}
    }
}

sub kill_httpd
{
    my $wait = shift;
    return unless -e "$ENV{APACHE_DIR}/httpd.pid";
    my $pid = get_pid();

    print STDERR "Killing httpd process ($pid)\n";
    my $result = kill 'TERM', $pid;
    if ( ! $result and $! =~ /no such (?:file|proc)/i )
    {
	# Looks like apache wasn't running, so we're done
	unlink "$ENV{APACHE_DIR}/httpd.pid" or warn "Couldn't remove '$ENV{APACHE_DIR}/httpd.pid': $!";
	return;
    }
    die "Can't kill process $pid: $!" if !$result;

    if ($wait)
    {
	print STDERR "Waiting for httpd to shut down\n";
	my $x = 0;
	while ( -e "$ENV{APACHE_DIR}/httpd.pid" )
	{
	    sleep (1);
	    $x++;
	    if ( $x > 10 )
	    {
		my $result = kill 'TERM', $pid;
		if ( ! $result and $! =~ /no such (?:file|proc)/i )
		{
		    # Looks like apache wasn't running, so we're done
		    unlink "$ENV{APACHE_DIR}/httpd.pid" or warn "Couldn't remove '$ENV{APACHE_DIR}/httpd.pid': $!";
		    return;
		}
		else
		{
		    die "$ENV{APACHE_DIR}/httpd.pid file still exists after 10 seconds.  Exiting.";
		}
	    }
	}
    }
}

use vars qw($TESTS);

sub ok
{
    my $ok = !!shift;
    print $ok ? 'ok ' : 'not ok ';
    print ++$TESTS, "\n";
}


__END__

    my $with_handler = shift;

    my $def = $with_handler ? 'CGI' : 'CGI_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

    my $path = '/comps/cgi_object';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
CGI
Status code: 0
EOF
						  );
    ok($success);

    if (! $with_handler && $mod_perl::VERSION >= 1.24)
    {
	# test that MasonAllowGlobals works (testing a list parameter
	# from httpd.conf)
	my $response = Apache::test->fetch('/comps/allow_globals');
	my $actual = filter_response($response, 0);
	my $success = HTML::Mason::Tests->check_output( actual => $actual,
							expect => <<'EOF',
X-Mason-Test: Initial value
$foo is 1
@bar is abc
Status code: 0
EOF
						      );
	ok($success);
    }

    $path = '/comps/head_request?foo=1&bar=1&bar=2';
    $path = "/ah=0$path" if $with_handler;
    $response = Apache::test->fetch( { uri => $path, method => 'HEAD' } );

    # We pretend that this request is always being done without in
    # order to make sure "Status code: 0" is appended onto the return.
    # This is because with a handler.pl (which normally calls
    # $r->print to append that text), $r->print won't actually do
    # anything for a HEAD request. - dave
    $actual = filter_response($response, 0);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
X-Mason-HEAD-Test1: foo: not a ref
X-Mason-HEAD-Test2: bar: is a ref
Status code: 0
EOF
					       );

    ok($success);

    my $path = '/comps/apache_request';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Apache::Request
Status code: 0
EOF
						  );
    ok($success);

    unless ($with_handler)
    {
	# test that MasonTopLevelPredicate works (testing a code
	# parameter from httpd.conf)
	my $response = Apache::test->fetch('/comps/__top_level_predicate');
	my $actual = filter_response($response, 0);
	ok( $actual =~ /404 not found/,
	    'top level predicate should have refused request' );

	$response = Apache::test->fetch('/comps/decline_dirs');
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: Initial value
decline_dirs is 0
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/basic';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF
						  );
    ok($success);

    $path = '/comps/headers';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 3


Blah blah
blah
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$response = Apache::test->fetch( "/ah=1/comps/headers" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: New value 1


Blah blah
blah
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/headers?blank=1';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$response = Apache::test->fetch( "/ah=1/comps/headers?blank=1" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: New value 1
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/_underscore';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
I am underscore.
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	# top_level_predicate should reject this request.
	$response = Apache::test->fetch( "/ah=2/comps/_underscore" );
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: 
Status code: 404
EOF
						   );
	ok($success);
    }

    $path = '/comps/die';
    $path = "/ah=0$path" if $with_handler;

    # error_mode is html so we get lots of stuff
    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    ok( $actual =~ m|error while executing /die:\s+Mine heart is pierced|,
	"Error should have said 'Mine heart is pierced'" );

    if ($with_handler)
    {
	# error_mode is fatal so we just get a 500
	$response = Apache::test->fetch( "/ah=4/comps/die" );
	$actual = filter_response($response, $with_handler);
	ok( $actual =~ m|500 Internal Server Error|,
	    "die should have generated 500 error" );
    }

    $path = '/comps/params?qs1=foo&qs2=bar&foo=A&foo=B';
    $path = "/ah=0$path" if $with_handler;

    # params in query string only
    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
foo: A, B, array
qs1: foo
qs2: bar
Status code: 0
EOF
						  );
    ok($success);

    $path = '/comps/params';
    $path = "/ah=0$path" if $with_handler;

    # params as POST only
    $response = Apache::test->fetch( { uri => $path,
				       method => 'POST',
				       content => 'post1=foo&post2=bar&foo=A&foo=B',
				     } );
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
foo: A, B, array
post1: foo
post2: bar
Status code: 0
EOF
						  );
    ok($success);

    $path = '/comps/params?qs1=foo&qs2=bar&mixed=A';
    $path = "/ah=0$path" if $with_handler;

    # params mixed in query string and POST
    $response = Apache::test->fetch( { uri => $path,
				       method => 'POST',
				       content => 'post1=a&post2=b&mixed=B',
				     } );
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
mixed: A, B, array
post1: a
post2: b
qs1: foo
qs2: bar
Status code: 0
EOF
						  );
    ok($success);

    $path = '/comps/print';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$path = '/ah=1/comps/print';

	$response = Apache::test->fetch($path);
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/r_print';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$path = '/ah=1/comps/r_print';

	$response = Apache::test->fetch($path);
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
						   );
	ok($success);
    }

    $path = '/comps/flush_buffer';
    $path = "/ah=0$path" if $with_handler;

    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
foo
bar
Status code: 0
EOF
					       );
    ok($success);

    if ($with_handler)
    {
	$path = '/ah=1/comps/flush_buffer';

	$response = Apache::test->fetch($path);
	$actual = filter_response($response, $with_handler);
	$success = HTML::Mason::Tests->check_output( actual => $actual,
						     expect => <<'EOF',
X-Mason-Test: Initial value
foo
bar
Status code: 0
EOF
						   );
	ok($success);
    }
