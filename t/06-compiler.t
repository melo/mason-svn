#!/usr/bin/perl -w

use strict;

use HTML::Mason::Tests;

my $tests = make_tests();
$tests->run;

sub make_tests {
    my $group = HTML::Mason::Tests->new( name => 'compiler',
					 description => 'compiler and lexer object functionality' );


#------------------------------------------------------------

    $group->add_test( name => 'allowed_globals',
		      description => 'test that undeclared globals cause an error',
		      interp_params => { use_object_files => 0 }, # force it to parse comp each time
		      component => <<'EOF',
<% $global = 1 %>
EOF
		      expect_error => 'Global symbol .* requires explicit package name',
		    );


#------------------------------------------------------------

    $group->add_test( name => 'allowed_globals',
		      description => 'test that undeclared globals cause an error',
		      pretest_code => sub { undef *HTML::Mason::Commands::global; undef *HTML::Mason::Commands::global },  # repeated to squash a var used only once warning
		      interp_params => { use_object_files => 0 },
		      component => <<'EOF',
<% $global = 1 %>
EOF
		      expect_error => 'Global symbol .* requires explicit package name',
		    );


#------------------------------------------------------------

    $group->add_test( name => 'allowed_globals',
		      description => 'test that declared globals are allows',
		      compiler_params => { allowed_globals => ['$global'] },
		      interp_params => { use_object_files => 0 },
		      component => <<'EOF',
<% $global = 1 %>
EOF
		      expect => <<'EOF',
1
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'default_escape_flags',
		      description => 'test that no escaping is done by default',
		      interp_params => { use_object_files => 0 },
		      component => <<'EOF',
Explicitly HTML-escaped: <% $expr |h %><p>
Explicitly HTML-escaped redundantly: <% $expr |hh %><p>
Explicitly URL-escaped: <% $expr |u
%><p>
No flags: <% $expr %><p>
No flags again: <% $expr | %><p>
Explicitly not escaped: <% $expr | n%><p>
<%init>
my $expr = "<b><i>Hello there</i></b>.";
</%init>
EOF
		      expect => <<'EOF',
Explicitly HTML-escaped: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
Explicitly HTML-escaped redundantly: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
Explicitly URL-escaped: %3Cb%3E%3Ci%3EHello%20there%3C%2Fi%3E%3C%2Fb%3E.<p>
No flags: <b><i>Hello there</i></b>.<p>
No flags again: <b><i>Hello there</i></b>.<p>
Explicitly not escaped: <b><i>Hello there</i></b>.<p>
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'default_escape_flags_2',
		      description => 'test that turning on default escaping works',
		      interp_params => { use_object_files => 0 },
		      compiler_params => { default_escape_flags => 'h' },
		      component => <<'EOF',
Explicitly HTML-escaped: <% $expr |h %><p>
Explicitly HTML-escaped redundantly: <% $expr |hh %><p>
Explicitly URL-escaped: <% $expr |un
%><p>
No flags: <% $expr %><p>
No flags again: <% $expr | %><p>
Explicitly not escaped: <% $expr | n%><p>
<%init>
my $expr = "<b><i>Hello there</i></b>.";
</%init>
EOF
		      expect => <<'EOF',
Explicitly HTML-escaped: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
Explicitly HTML-escaped redundantly: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
Explicitly URL-escaped: %3Cb%3E%3Ci%3EHello%20there%3C%2Fi%3E%3C%2Fb%3E.<p>
No flags: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
No flags again: &lt;b&gt;&lt;i&gt;Hello there&lt;/i&gt;&lt;/b&gt;.<p>
Explicitly not escaped: <b><i>Hello there</i></b>.<p>
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'globals_in_default_package',
		      description => 'tests that components are executed in HTML::Mason::Commands package by default',
		      interp_params => { use_object_files => 0 },
		      compiler_params => { allowed_globals => ['$packvar'] },
		      component => <<'EOF',
<% $packvar %>
<%init>
$HTML::Mason::Commands::packvar = 'commands';
$HTML::Mason::NewPackage::packvar = 'newpackage';
</%init>
EOF
		      expect => <<'EOF',
commands
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'globals_in_different_package',
		      description => 'tests in_package compiler parameter',
		      interp_params => { use_object_files => 0 },
		      compiler_params => { allowed_globals => ['$packvar'],
					 in_package => 'HTML::Mason::NewPackage' },
		      component => <<'EOF',
<% $packvar %>
<%init>
$HTML::Mason::Commands::packvar = 'commands';
$HTML::Mason::NewPackage::packvar = 'newpackage';
</%init>
EOF
		      expect => <<'EOF',
newpackage
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preamble',
		      description => 'tests preamble compiler parameter',
		      compiler_params => { preamble => 'my $msg = "This is the preamble.\n"; $m->out($msg);
'},
		      component => <<'EOF',
This is the body.
EOF
		      expect => <<'EOF',
This is the preamble.
This is the body.
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'postamble',
		      description => 'tests postamble compiler parameter',
		      compiler_params => { postamble => 'my $msg = "This is the postamble.\n"; $m->out($msg);
'},
		      component => <<'EOF',
This is the body.
EOF
		      expect => <<'EOF',
This is the body.
This is the postamble.
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preprocess',
		      description => 'test preprocess compiler parameter',
		      compiler_params => { preprocess => \&brackets_to_lt_gt },
		      component => <<'EOF',
[% 'foo' %]
bar
EOF
		      expect => <<'EOF',
foo
bar
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'postprocess1',
		      description => 'test postprocess compiler parameter (alpha blocks)',
		      compiler_params => { postprocess_text => \&uc_alpha },
		      component => <<'EOF',
<% 'foo' %>
bar
EOF
		      expect => <<'EOF',
foo
BAR
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'postprocess2',
		      description => 'test postprocess compiler parameter (perl blocks)',
		      compiler_params => { postprocess_perl => \&add_foo_to_perl },
		      component => <<'EOF',
<% 'foo' %>
bar
EOF
		      expect => <<'EOF',
fooFOO
bar
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'bad_var_name',
		      description => 'test that invalid Perl variable names are caught',
		      component => <<'EOF',
<%args>
$foo
$8teen
%bar
</%args>
Never get here
EOF
		      expect_error => 'Invalid <%args> section line at line 3',
		    );

#------------------------------------------------------------

    return $group;
}

# preprocessing the component
sub brackets_to_lt_gt
{
    my $comp = shift;
    ${ $comp } =~ s/\[\%(.*?)\%\]/<\%$1\%>/g;
}

# postprocessing alpha/perl code
sub uc_alpha
{
    ${ $_[0] } = uc ${ $_[0] };
}

sub add_foo_to_perl
{
    ${ $_[0] } =~ s/\);/. 'FOO');/;
}
