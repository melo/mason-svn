#!/usr/bin/perl -w

# Skip test if no mod_perl
eval { require mod_perl };
# need to use it twice to avoid annoying warning
unless ( $ENV{MASON_MAINTAINER} &&
         ( $mod_perl::VERSION || $mod_perl::VERSION ) )
{
    print "1..0\n";
    exit;
}

use strict;

use File::Spec;
use HTML::Mason::Tests;
use Test;

use lib 'lib', File::Spec->catdir('t', 'lib');

require File::Spec->catfile( 't', 'live_server_lib.pl' );

use Apache::test qw(skip_test have_httpd have_module);
skip_test unless have_httpd;

local $| = 1;

kill_httpd(1);
test_load_apache();

my $tests = 20; # multi conf & taint tests
$tests += 63 if my $have_libapreq = have_module('Apache::Request');
$tests += 41 if my $have_cgi      = have_module('CGI');
$tests += 16 if my $have_tmp      = (-d '/tmp' and -w '/tmp');
$tests++ if $have_cgi && $mod_perl::VERSION >= 1.24;
$tests++ if my $have_filter = have_module('Apache::Filter');

plan( tests => $tests);

print STDERR "\n";

write_test_comps();

if ($have_libapreq) {        # 63 tests
    cleanup_data_dir();
    apache_request_tests(1); # 23 tests

    cleanup_data_dir();
    apache_request_tests(0); # 23 tests

    cleanup_data_dir();
    no_config_tests();       # 16 tests

    if ($have_filter) {
        cleanup_data_dir();
        filter_tests();      # 1 test
    }
}

if ($have_tmp) {
    cleanup_data_dir();
    single_level_serverroot_tests();  # 16 tests
}

cleanup_data_dir();
taint_tests();           # 16 tests

if ($have_cgi) {             # 41 tests (+ 1?)
    cleanup_data_dir();
    cgi_tests(1);            # 22 tests + 1 if mod_perl version > 1.24

    cleanup_data_dir();
    cgi_tests(0);            # 19 tests
}

cleanup_data_dir();

# This is a hack but otherwise the following tests fail if the Apache
# server runs as any user other than root.  In real life, a user using
# the multi-config option with httpd.conf must handle the file
# permissions manually.
if ( $> == 0 || $< == 0 )
{
    chmod 0777, File::Spec->catdir( $ENV{APACHE_DIR}, 'data' );
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
foreach (sort keys %ARGS) {
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

    write_comp( 'redirect_with_scomp', <<'EOF',
Some content
% $m->scomp('.redirect');
<%def .redirect>
% $m->redirect('/comps/basic');
</%def>
EOF
	      );

    write_comp( 'internal_redirect', <<'EOF',
<%init>
$r->internal_redirect('/comps/internal_redirect_target?foo=17');
$m->auto_send_headers(0);
$m->clear_buffer;
$m->abort;
</%init>
EOF
	      );

    write_comp( 'internal_redirect_target', <<'EOF',
The number is <% $foo %>.
<%args>
$foo
</%args>
EOF
	      );

    write_comp( 'error_as_html', <<'EOF',
% my $x = undef; @$x;
EOF
              );

    write_comp( 'interp_class', <<'EOF',
Interp class: <% ref $m->interp %>
EOF
              );

    write_comp( 'old_html_escape', <<'EOF',
<% '<>' | old_h %>
EOF
              );

    write_comp( 'old_html_escape2', <<'EOF',
<% '<>' | old_h2 %>
EOF
              );

    write_comp( 'uc_escape', <<'EOF',
<% 'upper case' | uc %>
EOF
              );

    write_comp( 'data_cache_defaults', <<'EOF',
is memory: <% $m->cache->isa('Cache::MemoryCache') ? 1 : 0 %>
namespace: <% $m->cache->get_namespace %>
EOF
              );

    write_comp( 'test_code_param', <<'EOF',
preprocess changes lc fooquux to FOOQUUX
EOF
              );
}

sub cgi_tests
{
    my $with_handler = shift;

    my $def = $with_handler ? 'CGI' : 'CGI_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

    one_test( $with_handler, '/comps/cgi_object', 0, <<'EOF' );
X-Mason-Test: Initial value
CGI
Status code: 0
EOF

    if (! $with_handler && $mod_perl::VERSION >= 1.24)
    {
        one_test( $with_handler, '/comps/allow_globals', 0, <<'EOF' );
X-Mason-Test: Initial value
$foo is 1
@bar is abc
Status code: 0
EOF
    }

    # We pretend that this request is always being done without a
    # handler in order to make sure "Status code: 0" is appended onto
    # the return.  This is because with a handler.pl (which normally
    # calls $r->print to append that text), $r->print won't actually
    # do anything for a HEAD request. - dave
    one_test( 0, { uri => '/comps/head_request?foo=1&bar=1&bar=2',
                   method => 'HEAD' },
              0, <<'EOF' );
X-Mason-Test: Initial value
X-Mason-HEAD-Test1: bar: is a ref
X-Mason-HEAD-Test2: foo: not a ref
Status code: 0
EOF

    kill_httpd(1);
}

sub apache_request_tests
{
    my $with_handler = shift;

    my $def = $with_handler ? 'mod_perl' : 'mod_perl_no_handler';
    start_httpd($def);

    standard_tests($with_handler);

    one_test( $with_handler, '/comps/apache_request', 0, <<'EOF' );
X-Mason-Test: Initial value
Apache::Request
Status code: 0
EOF

    unless ($with_handler)
    {
        one_test( $with_handler, '/comps/decline_dirs', 0, <<'EOF' );
X-Mason-Test: Initial value
decline_dirs is 0
Status code: 0
EOF

        one_test( $with_handler, '/comps/old_html_escape', 0, <<'EOF' );
X-Mason-Test: Initial value
&lt;&gt;
Status code: 0
EOF

        one_test( $with_handler, '/comps/old_html_escape2', 0, <<'EOF' );
X-Mason-Test: Initial value
&lt;&gt;
Status code: 0
EOF

        one_test( $with_handler, '/comps/uc_escape', 0, <<'EOF' );
X-Mason-Test: Initial value
UPPER CASE
Status code: 0
EOF

        one_test( $with_handler, '/comps/data_cache_defaults', 0, <<'EOF' );
X-Mason-Test: Initial value
is memory: 1
namespace: foo
Status code: 0
EOF

        one_test( $with_handler, '/comps/test_code_param', 0, <<'EOF' );
X-Mason-Test: Initial value
preprocess changes lc FOOQUUX to FOOQUUX
Status code: 0
EOF
    }

    kill_httpd(1);
}

sub no_config_tests
{
    start_httpd('no_config');

    standard_tests(0);

    kill_httpd(1);
}

sub single_level_serverroot_tests
{
    start_httpd('single_level_serverroot');
    standard_tests(0);
    kill_httpd(1);
}

sub taint_tests
{
    start_httpd('taint');
    standard_tests(0);
    kill_httpd(1);
}

sub standard_tests
{
    my $with_handler = shift;

    my $path = '/comps/basic';
    $path = "/ah=0$path" if $with_handler;

    one_test( $with_handler, '/comps/basic', 0, <<'EOF' );
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF


    one_test( $with_handler, '/comps/headers', 0, <<'EOF' );
X-Mason-Test: New value 3


Blah blah
blah
Status code: 0
EOF

    if ($with_handler)
    {
        one_test( $with_handler, '/comps/headers', 1, <<'EOF' );
X-Mason-Test: New value 1


Blah blah
blah
Status code: 0
EOF
    }

    one_test( $with_handler, '/comps/headers?blank=1', 0, <<'EOF' );
X-Mason-Test: New value 1
Status code: 0
EOF

    if ($with_handler)
    {
        one_test( $with_handler, '/comps/headers?blank=1', 1, <<'EOF' );
X-Mason-Test: New value 1
Status code: 0
EOF
    }


    one_test( $with_handler, '/comps/_underscore', 0, <<'EOF' );
X-Mason-Test: Initial value
I am underscore.
Status code: 0
EOF

    one_test( $with_handler, '/comps/die', 0, qr{error.*Mine heart is pierced}s );

    if ($with_handler)
    {
	# error_mode is fatal so we just get a 500
        one_test( $with_handler, '/comps/die', 3, qr{500 Internal Server Error} );
    }

    one_test( $with_handler, '/comps/params?qs1=foo&qs2=bar&foo=A&foo=B', 0, <<'EOF' );
X-Mason-Test: Initial value
foo: A, B, array
qs1: foo
qs2: bar
Status code: 0
EOF

    one_test( $with_handler, { uri    => '/comps/params',
                               method => 'POST',
                               content => 'post1=foo&post2=bar&foo=A&foo=B' },
              0, <<'EOF' );
X-Mason-Test: Initial value
foo: A, B, array
post1: foo
post2: bar
Status code: 0
EOF

    one_test( $with_handler, { uri    => '/comps/params?qs1=foo&qs2=bar&mixed=A',
                               method => 'POST',
                               content => 'post1=a&post2=b&mixed=B' },
              0, <<'EOF' );
X-Mason-Test: Initial value
mixed: A, B, array
post1: a
post2: b
qs1: foo
qs2: bar
Status code: 0
EOF

    one_test( $with_handler, '/comps/print', 0, <<'EOF' );
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF

    if ($with_handler)
    {
        one_test( $with_handler, '/comps/print', 1, <<'EOF' );
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
    }

    one_test( $with_handler, '/comps/r_print', 0, <<'EOF' );
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF

    if ($with_handler)
    {
        one_test( $with_handler, '/comps/r_print', 1, <<'EOF' );
X-Mason-Test: Initial value
This is first.
This is second.
This is third.
Status code: 0
EOF
    }

    one_test( $with_handler, '/comps/flush_buffer', 0, <<'EOF' );
X-Mason-Test: Initial value
foo
bar
Status code: 0
EOF

    if ($with_handler)
    {
        one_test( $with_handler, '/comps/flush_buffer', 1, <<'EOF' );
X-Mason-Test: Initial value
foo
bar
Status code: 0
EOF
    }

    one_test( $with_handler, '/comps/redirect', 0, <<'EOF' );
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF

    one_test( $with_handler, '/comps/internal_redirect', 0, <<'EOF' );
X-Mason-Test: Initial value
The number is 17.
Status code: 0
EOF

    one_test( $with_handler, '/comps/error_as_html', 0, qr{<b>error:</b>.*Error during compilation}s );

    my $expected_class = $with_handler ? 'My::Interp' : 'HTML::Mason::Interp';

    one_test( $with_handler, '/comps/interp_class', 0, <<"EOF" );
X-Mason-Test: Initial value
Interp class: $expected_class
Status code: 0
EOF

    one_test( $with_handler, '/comps/redirect_with_scomp', 0, <<"EOF" );
X-Mason-Test: Initial value
Basic test.
2 + 2 = 4.
uri = /basic.
method = GET.


Status code: 0
EOF
}

sub multi_conf_tests
{
    start_httpd('multi_config');

    one_test( 0, '/comps/multiconf1/foo', 0, <<'EOF' );
X-Mason-Test: Initial value
I am foo in multiconf1
comp root is multiconf1
Status code: 0
EOF

    one_test( 0, '/comps/multiconf1/autohandler_test', 0, <<'EOF' );
X-Mason-Test: Initial value
autohandler is misnamed
Status code: 0
EOF

    one_test( 0, '/comps/multiconf2/foo', 0, <<'EOF' );
X-Mason-Test: Initial value
I am foo in multiconf2
comp root is multiconf2
Status code: 0
EOF

    one_test( 0, '/comps/multiconf2/dhandler_test', 0, qr{404 not found}i );

    kill_httpd(1);
}

sub one_test
{
    my ($with_handler, $fetch, $ah_num, $expect) = @_;

    if ( ref $fetch )
    {
        $fetch->{uri} = "/ah=$ah_num$fetch->{uri}" if $with_handler;
    }
    else
    {
        $fetch = "/ah=$ah_num$fetch" if $with_handler;
    }

    my $response = Apache::test->fetch($fetch);
    my $actual = filter_response($response, $with_handler);

    if ( ref $expect )
    {
        ok( $actual, $expect );
    }
    else
    {
        my $success = HTML::Mason::Tests->tests_class->check_output( actual => $actual,
                                                                     expect => $expect,
                                                                   );
        ok($success);
    }
}

sub filter_tests
{
    start_httpd('filter_tests');

    one_test( 0, '/comps/basic', 0, <<'EOF' );
X-Mason-Test: Initial value
BASIC TEST.
2 + 2 = 4.
URI = /BASIC.
METHOD = GET.


Status code: 0
EOF

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
