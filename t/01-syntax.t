#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests
{

    my $group = HTML::Mason::Tests->tests_class->new( name => 'syntax',
						      description => 'Basic component syntax tests' );


#------------------------------------------------------------

    $group->add_support( path => '/support/amper_test',
			 component => <<'EOF',
amper_test.<p>
% if (%ARGS) {
Arguments:<p>
%   foreach my $key (sort keys %ARGS) {
<b><% $key %></b>: <% $ARGS{$key} %><br>
%   }
% }
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'ampersand syntax',
		      description => 'tests all variations of component call path syntax and arg passing',
		      component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
amper Test
</TITLE>
</HEAD>
<BODY>
<& support/amper_test &>
<& /syntax/support/amper_test, message=>'Hello World!'  &>
<& support/amper_test, message=>'Hello World!', to=>'Joe' &>
<& "support/amper_test" &>
% my $dir = "support";
% my %args = (a=>17, b=>32);
<& $dir . "/amper_test", %args &>
</BODY>
</HTML>
EOF
		      expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
amper Test
</TITLE>
</HEAD>
<BODY>
amper_test.<p>

amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>

amper_test.<p>
Arguments:<p>
<b>message</b>: Hello World!<br>
<b>to</b>: Joe<br>

amper_test.<p>

amper_test.<p>
Arguments:<p>
<b>a</b>: 17<br>
<b>b</b>: 32<br>

</BODY>
</HTML>
EOF
		 );


#------------------------------------------------------------

    $group->add_test( name => 'replace',
		      description => 'tests <% %> tag',
		      component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Replacement Test
</TITLE>
</HEAD>
<BODY>
<% "Hello World!" %>
</BODY>
</HTML>
EOF
		      expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Replacement Test
</TITLE>
</HEAD>
<BODY>
Hello World!
</BODY>
</HTML>
EOF
		    );


#------------------------------------------------------------

	$group->add_test( name => 'percent',
			  description => 'tests %-line syntax',
			  component => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Percent Test
</TITLE>
</HEAD>
<BODY>
% my $message = "Hello World!";
<% $message %>
</BODY>
</HTML>
EOF
			  expect => <<'EOF',
<HTML>
<HEAD>
<TITLE>
Percent Test
</TITLE>
</HEAD>
<BODY>
Hello World!
</BODY>
</HTML>
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'fake_percent',
			  description => 'tests % in text section',
			  component => 'some text, a %, and some text',
			  expect =>    'some text, a %, and some text',
			);

#------------------------------------------------------------
	$group->add_test( name => 'empty_percents',
			  description => 'tests empty %-lines',
			  component => <<'EOF',
some text,
%
and some more
EOF
			  expect =>    "some text,\nand some more\n",
			);
#------------------------------------------------------------

	$group->add_test( name => 'empty_percents2',
			  description => 'tests empty %-lines followed by other %-lines',
			  component => <<'EOF',
some text,
%
% $m->print('foo, ');
and some more
EOF
			  expect =>    "some text,\nfoo, and some more\n",
			);

#------------------------------------------------------------

	$group->add_test( name => 'space_after_method_name',
			  description => 'tests that spaces are allowed after method/subcomp names',
			  component => <<'EOF',
a
<%def foo  >
</%def>
<%method bar   
>
</%method>
b
EOF
			  expect => <<'EOF',
a
b
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'comment_in_attr_flags',
			  description => 'tests that comments are allowed at end of flag/attr lines',
			  component => <<'EOF',
a
<%flags>
inherit => undef # foo bar
</%flags>
<%attr>
a => 1 # a is 1
b => 2 # ya ay
</%attr>
b
EOF
			  expect => <<'EOF',
a
b
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'dash in subcomp named',
			  description => 'tests that dashes are allowed in subcomponent names',
			  component => <<'EOF',
a
<%def has-dashes>
foo
</%def>
b
EOF
			  expect => <<'EOF',
a
b
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'flags_on_one_line',
			  description => 'tests that a flags block can be one line',
			  component => <<'EOF',
a
<%flags>inherit => undef</%flags>
b
EOF
			  expect => <<'EOF',
a
b
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'attr_uc_ending',
			  description => 'tests that an attr ending tag can be upper-case',
			  component => <<'EOF',
<%ATTR>
thing => 1</%ATTR>
thing: <% $m->request_comp->attr('thing') %>
EOF
			  expect => <<'EOF',
thing: 1
EOF
			);

#------------------------------------------------------------

	$group->add_test( name => 'args_uc_ending',
			  description => 'tests that args ending tag can be mixed case',
			  component => <<'EOF',
<%ARGS>
$a => 1</%ARGS>
a is <% $a %>
b
EOF
			  expect => <<'EOF',
a is 1
b
EOF
			);

#------------------------------------------------------------

    return $group;
}
