#!/usr/bin/perl -w

# Skip test if no mod_perl
eval { require mod_perl };
# need to use it twice to avoid annoying warning
unless ($mod_perl::VERSION || $mod_perl::VERSION)
{
    print "1..0\n";
    exit;
}

use strict;

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

my $tests = 4; # multi conf tests
$tests += 45 if my $have_libapreq = have_module('Apache::Request');
$tests += 34 if my $have_cgi      = have_module('CGI');
$tests++ if $have_cgi && $mod_perl::VERSION >= 1.24;
print "1..$tests\n";

print STDERR "\n";

write_test_comps();

if ($have_libapreq) {        # 45 tests
    cleanup_data_dir();
    apache_request_tests(1); # 19 tests

    cleanup_data_dir();
    apache_request_tests(0); # 14 tests

    cleanup_data_dir();
    no_config_tests();    # 12 tests
}

if ($have_cgi) {             # 34 tests (+ 1?)
    cleanup_data_dir();
    cgi_tests(1);            # 19 tests + 1 if mod_perl version > 1.24

    cleanup_data_dir();
    cgi_tests(0);            # 15 tests
}

cleanup_data_dir();

# This is a hack but otherwise the following tests fail if the Apache
# server runs as any user other than root.  In real life, a user using
# the multi-config option with httpd.conf must handle the file
# permissions manually.
if ( $> == 0 || $< == 0 )
{
    chmod 0777, File::Spec->( $ENV{APACHE_DIR}, 'data' );
}

multi_conf_tests();     # 4 tests

sub write_test_comps
{
    write_comp( 'basic', <<'EOF',
Basic test.
2 + 2 = <% 2 + 2 %>.
uri = <% $r->uri =~ /basic$/ ? '/basic' : $r->uri %>.
method = <% $r->method %>.


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
<% UNIVERSAL::isa(eval { $m->cgi_object } || undef, 'CGI') ? 'CGI' : 'NO CGI' %><% $@ || '' %>
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
$autohandler => 'misnamed'
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
% $m->print("foo\n");
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

    write_comp( 'redirect', <<'EOF',
<%init>
$m->redirect('/comps/basic');
</%init>
EOF
	      );

=pod

Hmm, what are the expect correct and double-header-broken outputs
here?  I don't even get 'I am the target.'

    write_comp( 'internal_redirect', <<'EOF',
<%init>
$m->auto_send_headers(0);
$r->internal_redirect('/comps/redirect_target');
$m->auto_send_headers(1);
</%init>
EOF
	      );

    write_comp( 'redirect_target', <<'EOF',
I am the target.
EOF
	      );

=cut

}

sub cgi_tests
{
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

    kill_httpd(1);
}

sub apache_request_tests
{
    my $with_handler = shift;

    my $def = $with_handler ? 'mod_perl' : 'mod_perl_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

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

    kill_httpd(1);
}

sub no_config_tests
{
    start_httpd('no_config');

    standard_tests(0);

    kill_httpd(1);
}

sub standard_tests
{
    my $with_handler = shift;

    my $path = '/comps/basic';
    $path = "/ah=0$path" if $with_handler;

    my $response = Apache::test->fetch($path);
    my $actual = filter_response($response, $with_handler);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
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
	$response = Apache::test->fetch( "/ah=3/comps/die" );
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

    $path = '/comps/redirect';
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

=pod

    $path = '/comps/internal_redirect';
    $path = "/ah=0$path" if $with_handler;
    $response = Apache::test->fetch($path);
    $actual = filter_response($response, $with_handler);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
I dunno
EOF
					       );

=cut

}

sub multi_conf_tests
{
    start_httpd('multi_config');

    my $response = Apache::test->fetch('/comps/multiconf1/foo');
    my $actual = filter_response($response, 0);
    my $success = HTML::Mason::Tests->check_output( actual => $actual,
						    expect => <<'EOF',
X-Mason-Test: Initial value
I am foo in multiconf1
comp root is multiconf1
Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf1/autohandler_test');
    $actual = filter_response($response, 0);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
autohandler is misnamed
Status code: 0
EOF
						  );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf2/foo');
    $actual = filter_response($response, 0);
    $success = HTML::Mason::Tests->check_output( actual => $actual,
						 expect => <<'EOF',
X-Mason-Test: Initial value
I am foo in multiconf2
comp root is multiconf2
Status code: 0
EOF
					       );
    ok($success);

    $response = Apache::test->fetch('/comps/multiconf2/dhandler_test');
    $actual = filter_response($response, 0);
    ok( $actual =~ /404 not found/i,
	"Attempt to request a non-existent component should not work with incorrect dhandler_name" );

    kill_httpd(1);
}

# We're not interested in headers that are always going to be
# different (like date or server type).
sub filter_response
{
    my $response = shift;

    my $with_handler = shift;

    # because the header or content may be undef
    local $^W = 0;
    my $actual = ( 'X-Mason-Test: ' .
		   # hack until I make a separate test
		   # suite for the httpd.conf configuration
		   # stuff
		   ( $with_handler ?
		     $response->headers->header('X-Mason-Test') :
		     ( $response->headers->header('X-Mason-Test') ?
		       $response->headers->header('X-Mason-Test') :
		       'Initial value' ) ) );
    $actual .= "\n";

    # Any headers starting with X-Mason are added, excluding
    # X-Mason-Test, which is handled above
    my @headers;
    $response->headers->scan( sub { return if $_[0] eq 'X-Mason-Test' || $_[0] !~ /^X-Mason/;
				    push @headers, [ $_[0], "$_[0]: $_[1]\n" ] } );

    foreach my $h ( sort { $a->[0] cmp $b->[0] } @headers )
    {
	$actual .= $h->[1];
    }

    $actual .= $response->content;

    my $code = $response->code == 200 ? 0 : $response->code;
    $actual .= "Status code: $code" unless $with_handler;

    return $actual;
}
