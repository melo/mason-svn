#!/usr/bin/perl -w

use strict;
use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'request',
					 description => 'request object functionality' );


#------------------------------------------------------------

    $group->add_support( path => '/support/abort_test',
			 component => <<'EOF',
<%args>
$val => 50
</%args>
Some more text

% $m->abort($val);

But this will never be seen
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => '/support/display_req_obj',
			 component => <<'EOF',
My depth is <% $m->depth %>.

The top-level component is <% $m->request_comp->title %>.

My stack looks like:
-----
% foreach my $comp ($m->callers) {
<% $comp->title %>
% }
-----

EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => '/support/subrequest_error_test',
			 component => <<'EOF',
<& display_req_obj &>
% die "whoops!";
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => '/sections/perl',
			 component => <<'EOF',
foo
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => '/support/various_test',
			 component => <<'EOF',
Caller is <% $m->caller->title %> or <% $m->callers(1)->title %>.
The top level component is <% $m->callers(-1)->title %> or <% $m->request_comp->title %>.
The full component stack is <% join(",",map($_->title,$m->callers)) %>.
My argument list is (<% join(",",$m->caller_args(0)) %>).
The top argument list is (<% join(",",$m->request_args()) %>) or (<% join(",",$m->caller_args(-1)) %>).

% foreach my $path (qw(various_test /request/sections/perl foobar /shared)) {
%   my $full_path = HTML::Mason::Tools::absolute_comp_path($path, $m->current_comp->dir_path);
Trying to fetch <% $path %> (full path <% $full_path %>):
%   if ($m->comp_exists($path)) {
%     if (my $comp = $m->fetch_comp($path)) {
<% $path %> exists with title <% $comp->title %>.
%     } else {
<% $path %> exists but could not fetch object!
%     }
%   } else {
<% $path %> does not exist.
%   }
% }

% $m->print("Output via the out function.");

/request/file outputs <% int(length($m->scomp("/request/file"))/10) %>0+ characters.
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'various_helper',
			 component => <<'EOF',
<& support/various_test, %ARGS &>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'abort',
		      description => 'test $m->abort method (autoflush on)',
		      interp_params => { autoflush => 1 },

		      component => <<'EOF',
Some text

% eval {$m->comp('support/abort_test')};
% if (my $err = $@) {
%   if ($m->aborted) {
Component aborted with value <% $err->aborted_value %>
%   } else {
Got error
%   }
% }
EOF
		      expect => <<'EOF',
Some text

Some more text

Component aborted with value 50
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'abort_0',
		      description => 'test $m->abort method with value of 0',

		      component => <<'EOF',
Some text

% eval {$m->comp('support/abort_test', val => 0)};
% if (my $err = $@) {
%   if ($m->aborted($err)) {
Component aborted with value <% $err->aborted_value %>
%   } else {
Got error
%   }
% }
EOF
		      expect => <<'EOF',
Some text

Some more text

Component aborted with value 0
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'abort',
		      description => 'test $m->abort method (autoflush off)',
		      component => <<'EOF',
Some text

% eval {$m->comp('support/abort_test')};
% if (my $err = $@) {
%   if ($m->aborted) {
Component aborted with value <% $err->aborted_value %>
%   } else {
Got error
%   }
% }
EOF
		      expect => <<'EOF',
Some text

Some more text

Component aborted with value 50
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'file',
		      description => 'tests $m->file method',
		      component => <<'EOF',
Now I will print myself:

% my $output = $m->file("file");
% $output =~ s/\cM//g;
<% $output %>
EOF
		      expect => <<'EOF',
Now I will print myself:

Now I will print myself:

% my $output = $m->file("file");
% $output =~ s/\cM//g;
<% $output %>
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'list_out',
		      description => 'tests that $m->out can handle a list of arguments',
		      component => <<'EOF',
Sending list of arguments:

<% 'blah','boom','bah' %>

<%perl>
 $m->print(3,4,5);
</%perl>
EOF
		      expect => <<'EOF',
Sending list of arguments:

blahboombah

345
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'req_obj',
		      description => 'tests various operations such as $m->out, comp calls, $m->current_comp',
		      component => <<'EOF',
<%def .subcomp>
% if ($count < 5) {
<& $m->current_comp, count=>$count+1 &>
% } else {
<& support/display_req_obj &>
% }
<%args>
$count
</%args>
</%def>

<% '-' x 10 %>

One level request:
<& support/display_req_obj &>

<% '-' x 10 %>

Many level request:
<& .subcomp, count=>0 &>

<% '-' x 10 %>
EOF
		      expect => <<'EOF',

----------

One level request:
My depth is 2.

The top-level component is /request/req_obj.

My stack looks like:
-----
/request/support/display_req_obj
/request/req_obj
-----



----------

Many level request:






My depth is 8.

The top-level component is /request/req_obj.

My stack looks like:
-----
/request/support/display_req_obj
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj:.subcomp
/request/req_obj
-----









----------
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'subrequest',
		      description => 'tests the official subrequest mechanism',
		      component => <<'EOF',
<%def .helper>
Executing subrequest
% my $buf;
% my $req = $m->make_subrequest(comp=>'/request/support/display_req_obj', out_method => \$buf);
% $req->exec();
<% $buf %>
</%def>

Calling helper
<& .helper &>
EOF
		      expect => <<'EOF',

Calling helper

Executing subrequest
My depth is 1.

The top-level component is /request/support/display_req_obj.

My stack looks like:
-----
/request/support/display_req_obj
-----



EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/dir/autohandler',
			 component => <<'EOF',
I am the autohandler.
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/support/dir/comp',
			 component => <<'EOF',
I am the called comp (no autohandler).
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'subrequest_with_autohandler',
		      description => 'tests the subrequest mechanism with an autohandler',
		      component => <<'EOF',
Executing subrequest
% my $buf;
% my $req = $m->make_subrequest(comp=>'/request/support/dir/comp', out_method => \$buf);
% $req->exec();
<% $buf %>
EOF
		      expect => <<'EOF',
Executing subrequest
I am the autohandler.
EOF
		    );


#------------------------------------------------------------

    # 5.6.0 is evil
    unless ($] == 5.006)
    {
	$group->add_test( name => 'subrequest_error',
			  description => 'check error handling for subrequest mechanism',
			  component => <<'EOF',
<%def .helper>
% $m->subexec('/request/support/subrequest_error_test');
</%def>

Calling helper
% eval {$m->comp('.helper')};
% my $error = $@;
<& /shared/check_error, error=>$error, lines=>1 &>

% if ($error) {
Back from error, checking request state:
<& support/display_req_obj &>
% }
EOF
			  expect => <<'EOF',

Calling helper
Error: whoops!


Back from error, checking request state:
My depth is 2.

The top-level component is /request/subrequest_error.

My stack looks like:
-----
/request/support/display_req_obj
/request/subrequest_error
-----


EOF
			);
    }

#------------------------------------------------------------

    $group->add_test( name => 'various',
		      call_args => {junk=>5},
		      description => 'tests caller, callers, fetch_comp, process_comp_path, comp_exists and scomp',
		      component => <<'EOF',
<& various_helper, junk=>$ARGS{junk}+1 &>
EOF
		      expect => <<'EOF',
Caller is /request/various_helper or /request/various_helper.
The top level component is /request/various or /request/various.
The full component stack is /request/support/various_test,/request/various_helper,/request/various.
My argument list is (junk,6).
The top argument list is (junk,5) or (junk,5).

Trying to fetch various_test (full path /request/support/various_test):
various_test exists with title /request/support/various_test.
Trying to fetch /request/sections/perl (full path /request/sections/perl):
/request/sections/perl exists with title /request/sections/perl.
Trying to fetch foobar (full path /request/support/foobar):
foobar does not exist.
Trying to fetch /shared (full path /shared):
/shared does not exist.

Output via the out function.
/request/file outputs 120+ characters.
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test2/autohandler',
			 component => <<'EOF',
This is the first autohandler
Remaining chain: <% join(',',map($_->title,$m->fetch_next_all)) %>
<& $m->fetch_next, level => 1 &>\
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test2/dir1/autohandler',
			 component => <<'EOF',
This is the second autohandler
Remaining chain: <% join(',',map($_->title,$m->fetch_next_all)) %>
% foreach (@_) {
<% $_ %>
% }
<& $m->fetch_next, level => 2 &>\
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'fetch_next',
		      path => '/autohandler_test2/dir1/fetch_next',
		      call_path => '/autohandler_test2/dir1/fetch_next',
		      description => 'Test $m->fetch_next and $m->fetch_next_all',
		      component => <<'EOF',
This is the main component (called by level <% $ARGS{level} %>)
Remaining chain: <% join(',',map($_->title,$m->fetch_next_all)) %>
% foreach (@_) {
<% $_ %>
% }
EOF
		      expect => <<'EOF',
This is the first autohandler
Remaining chain: /request/autohandler_test2/dir1/autohandler,/request/autohandler_test2/dir1/fetch_next
This is the second autohandler
Remaining chain: /request/autohandler_test2/dir1/fetch_next
level
1
This is the main component (called by level 2)
Remaining chain: 
level
2
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'print',
		      description => 'Test print function from a component',
		      component => <<'EOF',
This is first.
% print "This is second.\n";
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'printf',
		      description => 'Test printf function from a component',
		      component => <<'EOF',
This is first.
% printf '%s', "This is second.\n";
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'autoflush_print',
		      description => 'Test print function from a component with autoflush on',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
This is first.
% print "This is second.\n";
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'autoflush_printf',
		      description => 'Test printf function from a component with autoflush on',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
This is first.
% printf '%s', "This is second.\n";
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'flush_print',
		      description => 'Test print function from a component in conjunction with $m->flush_buffer call',
		      component => <<'EOF',
This is first.
% print "This is second.\n";
% $m->flush_buffer;
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'flush_print_autoflush',
		      description => 'Test print function from a component with autoflush on in conjunction with $m->flush_buffer call',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
This is first.
% print "This is second.\n";
% $m->flush_buffer;
This is third.
EOF
		      expect => <<'EOF',
This is first.
This is second.
This is third.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'instance',
		      description => 'Test HTML::Mason::Request->instance',
		      component => <<'EOF',
<% $m eq HTML::Mason::Request->instance ? 'yes' : 'no' %>
EOF
		      expect => <<'EOF',
yes
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'abort_and_filter',
		      description => 'Test that an abort in a filtered component still generates _some_ output',
		      component => <<'EOF',
filter

% eval { $m->comp('support/abort_test') };
<%filter>
return uc $_;
</%filter>
EOF
		      expect => <<'EOF',
FILTER

SOME MORE TEXT

EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'abort_and_store',
		      description => 'Test that an abort in a store\'d component still generates _some_ output',
		      component => <<'EOF',
filter

% my $foo;
% eval { $m->comp( { store => \$foo }, 'support/abort_test') };
<% $foo %>
EOF
		      expect => <<'EOF',
filter

Some more text

EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/subrequest2/autohandler',
			 component => <<'EOF',
I am the autohandler for <% $m->base_comp->name %>.
% $m->call_next;
<%flags>
inherit => undef
</%flags>
EOF
		       );

#------------------------------------------------------------

    $group->add_support( path => '/subrequest2/bar',
			 component => <<'EOF',
I am bar.
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'subreq_exec_order',
		      path => '/subrequest2/subreq_exec_order',
		      call_path => '/subrequest2/subreq_exec_order',
		      description => 'Test that output from a subrequest comes out when we expect it to.',
		      component => <<'EOF',
% $m->subexec('/request/subrequest2/bar');
I am subreq_exec_order.
EOF
		      expect => <<'EOF',
I am the autohandler for subreq_exec_order.
I am the autohandler for bar.
I am bar.
I am subreq_exec_order.
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'reexec',
		      description => 'test that $m cannot be reexecuted',
		      component => <<'EOF',
<%init>
$m->exec;
</%init>
EOF
                      expect_error => qr/Can only call exec\(\) once/,
                    );

#------------------------------------------------------------

    $group->add_support( path => '/support/autoflush_subrequest',
			 component => <<'EOF',
here is the child
% $m->clear_buffer;
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'autoflush_subrequest',
		      description => 'make sure that a subrequest respects its parent autoflush setting',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
My child says:
% $m->subexec('/request/support/autoflush_subrequest');
EOF
		      expect => <<'EOF',
My child says:
here is the child
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'no_autoflush_subrequest',
		      description => 'make sure that a subrequest respects its parent autoflush setting',
		      interp_params => { autoflush => 0 },
		      component => <<'EOF',
My child says:
% $m->subexec('/request/support/autoflush_subrequest');
EOF
		      expect => <<'EOF',
My child says:
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/return/scalar',
			 component => <<'EOF',
% die "wantarray should be false" unless defined(wantarray) and !wantarray;
% return 'foo';
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_scalar',
		      description => 'tests that exec returns scalar return value of top component',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/request/support/return/scalar');
% my $value = $req->exec();
return value is <% $value %>
EOF
		      expect => <<'EOF',
return value is foo
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/return/list',
			 component => <<'EOF',
% die "wantarray should be true" unless wantarray;
% return (1, 2, 3);
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_list',
		      description => 'tests that exec returns list return value of top component',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/request/support/return/list');
% my @value = $req->exec();
return value is <% join(",", @value) %>
EOF
		      expect => <<'EOF',
return value is 1,2,3
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/support/return/nothing',
			 component => <<'EOF',
wantarray is <% defined(wantarray) ? "defined" : "undefined" %>
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'return_nothing',
		      description => 'tests exec in non-return context',
		      component => <<'EOF',
% my $req = $m->make_subrequest(comp=>'/request/support/return/nothing');
% $req->exec();
EOF
		      expect => <<'EOF',
wantarray is undefined
EOF
		    );


#------------------------------------------------------------

    return $group;
}
