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
use File::Spec;
use HTML::Mason::Tests;

use lib 'lib', File::Spec->catdir('t', 'lib');

require File::Spec->catfile( 't', 'live_server_lib.pl' );

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

local $| = 1;

kill_httpd(1);
test_load_apache();

print "1..7\n";

print STDERR "\n";

write_test_comps();

run_tests();

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
EOF
	      );

    write_comp( 'cgi_foo_param', <<'EOF',
CGI foo param is <% $r->query->param('foo') %>
EOF
	      );

    write_comp( 'print', <<'EOF',
This is first.
% print "This is second.\n";
This is third.
EOF
	      );

    write_comp( 'redirect', <<'EOF',
<%init>
$m->redirect('/comps/basic');
</%init>
EOF
	      );
}

sub run_tests
{
    start_httpd('CGIHandler');

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
	my $path = '/comps/print/autoflush';
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
	my $path = '/comps/print/handle_comp';
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
	my $path = '/comps/print/handle_cgi_object';
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
	my $path = '/comps/cgi_foo_param/handle_cgi_object';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
CGI foo param is bar
EOF
						      );

	ok($success);
    }

    {
	my $path = '/comps/redirect';
	my $response = Apache::test->fetch($path);
	my $success = HTML::Mason::Tests->check_output( actual => $response->content,
							expect => <<'EOF',
Basic test.
2 + 2 = 4.
EOF
						      );

	ok($success);
    }

    kill_httpd();
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
