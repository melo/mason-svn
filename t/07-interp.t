#!/usr/bin/perl -w

use strict;

use File::Spec;
use HTML::Mason::Tests;
use HTML::Mason::Tools qw(load_pkg);

my $tests = make_tests();
$tests->run;

sub make_tests
{
    my $group = HTML::Mason::Tests->new( name => 'interp',
					 description => 'interp object functionality' );

#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test/autohandler',
			 component => <<'EOF',
The recursive autohandler: <% $m->current_comp->path %>

% $m->call_next;
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'no recursive autohandlers',
		      description => 'tests turning off recursive autohandlers',
		      call_path => '/autohandler_test/subdir/hello',
		      component => <<'EOF',
Hello World!
EOF
		      expect => <<'EOF',
The recursive autohandler: /interp/autohandler_test/autohandler

Hello World!
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => '/autohandler_test/subdir/plainfile',
			 component => <<'EOF',
The local autohandler: <% $m->current_comp->path %>

% $m->call_next;
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'alternate autohandler name',
		      description => 'tests that providing an alternate name for autohandlers works',
		      call_path => '/autohandler_test/subdir/hello',
		      interp_params => { autohandler_name => 'plainfile' },
		      component => <<'EOF',
Hello World!
EOF
		      expect => <<'EOF',
The local autohandler: /interp/autohandler_test/subdir/plainfile

Hello World!
EOF
		    );

    my $alt_root = File::Spec->catdir( HTML::Mason::Tests->base_path, 'alt_root' );
    my @roots = ( [ main => HTML::Mason::Tests->comp_root],
		  [ alt => $alt_root ] );


    #HACK!
    HTML::Mason::Tests->write_comp( '/alt_root/interp/comp_root_test/private2',
				    File::Spec->catdir( $alt_root, 'interp', 'comp_root_test' ),
				    'private2',
				    <<'EOF' );
private2 in the alternate component root.
<& showcomp &>
EOF

    HTML::Mason::Tests->write_comp( '/alt_root/interp/comp_root_test/shared',
				    File::Spec->catdir( $alt_root, 'interp', 'comp_root_test' ),
				    'shared',
				    <<'EOF' );
shared.html in the alternate component root.
<& showcomp &>
EOF


#------------------------------------------------------------

    $group->add_support( path => '/comp_root_test/showcomp',
			 component => <<'EOF',
% my $comp = $m->callers(1);
<& /shared/display_comp_obj, comp=>$comp &>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'shared',
		      description => 'test that component in both comp_roots is called in first comp_root',
		      call_path => '/comp_root_test/shared',
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
shared in the main component root.
<& showcomp &>
EOF
		      expect => <<'EOF',
shared in the main component root.
Declared args:

I am not a subcomponent.
I am file-based.
My short name is shared.
My directory is /interp/comp_root_test.
I have 0 subcomponent(s).
My title is /interp/comp_root_test/shared [main].

My path is /interp/comp_root_test/shared.
My comp_id is /main/interp/comp_root_test/shared.



EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'private1',
		      description => 'test that component in first comp_root is found',
		      call_path => '/comp_root_test/private1',
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
private1 in the main component root.
<& showcomp &>
EOF
		      expect => <<'EOF',
private1 in the main component root.
Declared args:

I am not a subcomponent.
I am file-based.
My short name is private1.
My directory is /interp/comp_root_test.
I have 0 subcomponent(s).
My title is /interp/comp_root_test/private1 [main].

My path is /interp/comp_root_test/private1.
My comp_id is /main/interp/comp_root_test/private1.



EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'private2',
		      description => 'test that component in second comp_root is found',
		      call_path => '/comp_root_test/private2',
		      path => '/foo', # its already written.  HACK!
		      interp_params => { comp_root => \@roots },
		      component => <<'EOF',
foo
EOF
		      expect => <<'EOF',
private2 in the alternate component root.
Declared args:

I am not a subcomponent.
I am file-based.
My short name is private2.
My directory is /interp/comp_root_test.
I have 0 subcomponent(s).
My title is /interp/comp_root_test/private2 [alt].

My path is /interp/comp_root_test/private2.
My comp_id is /alt/interp/comp_root_test/private2.



EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'current_time',
		      description => 'test current_time interp param',
		      interp_params => { current_time => 945526402 },
		      component => <<'EOF',
<% $m->time %>
EOF
		      expect => <<'EOF',
945526402
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => 'support/recurse_test',
			 component => <<'EOF',
Entering <% $count %><p>
% if ($count < $max) {
<& recurse_test, count=>$count+1, max=>$max &>
% }
Exiting <% $count %><p>\
<%args>
$count=>0
$max
</%args>
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_1',
		      description => 'Test that recursion 8 levels deep is allowed',
		      component => <<'EOF',
% eval { $m->comp('support/recurse_test', max=>8) };
EOF
		      expect => <<'EOF',
Entering 0<p>
Entering 1<p>
Entering 2<p>
Entering 3<p>
Entering 4<p>
Entering 5<p>
Entering 6<p>
Entering 7<p>
Entering 8<p>
Exiting 8<p>
Exiting 7<p>
Exiting 6<p>
Exiting 5<p>
Exiting 4<p>
Exiting 3<p>
Exiting 2<p>
Exiting 1<p>
Exiting 0<p>
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_2',
		      description => 'Test that recursion is stopped after 32 levels',
		      interp_params => { autoflush => 1 },
		      component => '<& support/recurse_test, max=>48 &>',
		      expect_error => qr{32 levels deep in component stack \(infinite recursive call\?\)},
		    );


#------------------------------------------------------------

    $group->add_test( name => 'max_recurse_3',
		      description => 'Test interp max_recurse param',
		      interp_params => { max_recurse => 50 },
		      component => <<'EOF',
% eval { $m->comp('support/recurse_test', max=>48) };
<& /shared/check_error, error=>$@ &>
EOF
		      expect => <<'EOF',
Entering 0<p>
Entering 1<p>
Entering 2<p>
Entering 3<p>
Entering 4<p>
Entering 5<p>
Entering 6<p>
Entering 7<p>
Entering 8<p>
Entering 9<p>
Entering 10<p>
Entering 11<p>
Entering 12<p>
Entering 13<p>
Entering 14<p>
Entering 15<p>
Entering 16<p>
Entering 17<p>
Entering 18<p>
Entering 19<p>
Entering 20<p>
Entering 21<p>
Entering 22<p>
Entering 23<p>
Entering 24<p>
Entering 25<p>
Entering 26<p>
Entering 27<p>
Entering 28<p>
Entering 29<p>
Entering 30<p>
Entering 31<p>
Entering 32<p>
Entering 33<p>
Entering 34<p>
Entering 35<p>
Entering 36<p>
Entering 37<p>
Entering 38<p>
Entering 39<p>
Entering 40<p>
Entering 41<p>
Entering 42<p>
Entering 43<p>
Entering 44<p>
Entering 45<p>
Entering 46<p>
Entering 47<p>
Entering 48<p>
Exiting 48<p>
Exiting 47<p>
Exiting 46<p>
Exiting 45<p>
Exiting 44<p>
Exiting 43<p>
Exiting 42<p>
Exiting 41<p>
Exiting 40<p>
Exiting 39<p>
Exiting 38<p>
Exiting 37<p>
Exiting 36<p>
Exiting 35<p>
Exiting 34<p>
Exiting 33<p>
Exiting 32<p>
Exiting 31<p>
Exiting 30<p>
Exiting 29<p>
Exiting 28<p>
Exiting 27<p>
Exiting 26<p>
Exiting 25<p>
Exiting 24<p>
Exiting 23<p>
Exiting 22<p>
Exiting 21<p>
Exiting 20<p>
Exiting 19<p>
Exiting 18<p>
Exiting 17<p>
Exiting 16<p>
Exiting 15<p>
Exiting 14<p>
Exiting 13<p>
Exiting 12<p>
Exiting 11<p>
Exiting 10<p>
Exiting 9<p>
Exiting 8<p>
Exiting 7<p>
Exiting 6<p>
Exiting 5<p>
Exiting 4<p>
Exiting 3<p>
Exiting 2<p>
Exiting 1<p>
Exiting 0<p>No error!?

EOF
		    );


#------------------------------------------------------------

=pod

    $group->add_support( path => 'code_cache_test/show_code_cache',
			 component => <<'EOF',
Code cache contains these plain components:
% my %c = %{$m->interp->{code_cache}};
% foreach ( sort grep { /plain/ } keys %c ) {
<% $_ %>
% }
% if ( ! grep { /plain/ } keys %c ) {
Code cache contains no files matching /plain/
% }
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain1',
			 component => <<'EOF'
plain1
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain2',
			 component => <<'EOF'
plain2
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain3',
			 component => <<'EOF'
plain3
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain4',
			 component => <<'EOF'
plain4
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain5',
			 component => <<'EOF'
plain5
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain6',
			 component => <<'EOF'
plain6
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );


#------------------------------------------------------------

    $group->add_support( path => 'code_cache_test/plain7',
			 component => <<'EOF'
plain7
%#AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
%#BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
%#CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
%#DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD
EOF
		       );

#------------------------------------------------------------

    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
					   comp_root => $group->comp_root,
					   code_cache_max_size => 9400 );

    $group->add_test( name => 'code_cache_test/code_cache_1',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain1 &><& plain1 &><& plain1 &><& plain2 &><& plain2 &><& plain2 &><& plain3 &><& plain4 &><& plain2 &><& plain2 &><& plain1 &><& plain5 &><& plain2 &><& plain4 &><& plain4 &><& plain5 &><& plain5 &>

EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_1',
		      description => 'Show code cache size after first usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain3
/interp/code_cache_test/plain5
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/code_cache_2',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain6 &><& plain6 &><& plain6 &><& plain6 &>
EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_2',
		      description => 'Show code cache size after second usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain3
/interp/code_cache_test/plain5
/interp/code_cache_test/plain6
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/code_cache_3',
		      description => 'Run in order to load up code cache',
		      interp => $interp,
		      component => <<'EOF',
<& plain7 &><& plain7 &><& plain7 &>
EOF
		      skip_expect => 1
		    );


#------------------------------------------------------------

    $group->add_test( name => 'code_cache_test/show_code_cache_3',
		      description => 'Show code cache size after second usage',
		      interp => $interp,
		      component => <<'EOF',
<& show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains these plain components:
/interp/code_cache_test/plain1
/interp/code_cache_test/plain2
/interp/code_cache_test/plain5
/interp/code_cache_test/plain6
/interp/code_cache_test/plain7
EOF
		    );


=cut

#------------------------------------------------------------

    $group->add_test( name => 'dhandler_name',
		      description => 'Test that providing an alternate name for dhandlers works',
		      path => 'dhandler_test/plainfile',
		      call_path => 'dhandler_test/foo/blag',
		      interp_params => { dhandler_name => 'plainfile' },
		      component => <<'EOF',
dhandler arg = <% $m->dhandler_arg %>
EOF
		      expect => <<'EOF',
dhandler arg = foo/blag
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'mode_test',
			 component => <<'EOF',
First of all I'd
% $m->clear_buffer;
No what I really wanted to say was
% $m->clear_buffer;
Oh never mind.
EOF
		       );


#------------------------------------------------------------

    $group->add_test( name => 'no_autoflush_mode',
		      description => 'Test that no autoflush (batch) mode setting works',
		      component => <<'EOF',
<& mode_test &>
EOF
		      expect => <<'EOF',
Oh never mind.
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'autoflush_mode',
		      description => 'Test that autoflush setting works',
		      interp_params => { autoflush => 1 },
		      component => <<'EOF',
<& mode_test &>
EOF
		      expect => <<'EOF',
First of all I'd
No what I really wanted to say was
Oh never mind.
EOF
		    );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/show_code_cache',
			 component => <<'EOF',
Code cache contains:
% my %c = %{$m->interp->{code_cache}};
<% join("\n",sort(keys(%c))) %>
EOF
		    );



#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/hello',
			 component => 'hello',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/goodbye',
			 component => 'goodbye',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/howareyou',
			 component => 'howareyou',
		       );


#------------------------------------------------------------

    $group->add_support( path => 'preloads_test/subdir/in_a_subdir',
			 component => 'howareyou',
		       );

#------------------------------------------------------------

    $group->add_test( name => 'preload_1',
		      description => 'Make sure no preloading is done by default',
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_1
/interp/preloads_test/show_code_cache
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preload_2',
		      description => 'Preload a single component by filename',
		      interp_params => { preloads => [ '/interp/preloads_test/hello' ] },
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_2
/interp/preloads_test/hello
/interp/preloads_test/show_code_cache
EOF
		    );


#------------------------------------------------------------

    $group->add_test( name => 'preload_3',
		      description => 'Preload all components (including subdirectory) by glob pattern',
		      interp_params => { preloads => [ '/interp/preloads_test/*', '/interp/preloads_test/*/*' ] },
		      component => <<'EOF',
<& preloads_test/show_code_cache &>
EOF
		      expect => <<'EOF',
Code cache contains:
/interp/preload_3
/interp/preloads_test/goodbye
/interp/preloads_test/hello
/interp/preloads_test/howareyou
/interp/preloads_test/show_code_cache
/interp/preloads_test/subdir/in_a_subdir
EOF
		    );

#------------------------------------------------------------

    my $interp = HTML::Mason::Interp->new( data_dir => $group->data_dir,
					   comp_root => $group->comp_root,
					 );
    $interp->compiler->allow_globals( qw($global) );
    $interp->set_global( global => 'parsimmon' );


    $group->add_test( name => 'globals',
		      description => 'Test setting a global in interp & compiler objects',
		      interp => $interp,
		      component => <<'EOF',
<% $global %>
EOF
		      expect => <<'EOF',
parsimmon
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/comp_path_test/a/b/c/foo',
			 component => <<'EOF',
I am foo!
EOF
		       );

#------------------------------------------------------------

    $group->add_test( name => 'process_comp_path',
		      description => 'Test that component paths cannot be resolved outside the comp root',
		      component => <<'EOF',
<& ../../../../../interp/comp_path_test/a/b/c/../c/foo &>
EOF
		      expect => <<'EOF'
I am foo!

EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'process_comp_path2',
		      description => 'Test that component paths containing /../ work as long they stay in the comp root',
		      path => '/comp_path_test/a/b/d/process',
		      call_path => '/comp_path_test/a/b/d/process',
		      component => <<'EOF',
<& ../c/foo &>
EOF
		      expect => <<'EOF'
I am foo!

EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'default_warnings',
		      description => 'test that warnings during component compilation cause an exception except for redefined subs',
		      component => <<'EOF',
a global: <% $GLOBAL %>
<%once>
sub foo { 1 }
sub foo { 1 }
</%once>
EOF
		      expect_error => qr/Global symbol "\$GLOBAL" requires explicit package name/,
		    );

#------------------------------------------------------------

    $group->add_test( name => 'ignore_warnings',
		      description => 'test that setting ignore_warnings_exp works',
		      interp_params => { ignore_warnings_expr => qr/useless use of "re" pragma/i },
		      component => <<'EOF',
% use re;
foo
EOF
		      expect => <<'EOF',
foo
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'make_anonymous_component',
		      description => 'test make_component() without a path',
		      component => <<'EOF',
<%init>
my $ctext = q|
% my $x = 'Hello, ';
<% $x %>|;
my $comp = $m->interp->make_component( comp_source => $ctext );
</%init>
% $m->comp($comp);
World
EOF
		      expect => <<'EOF',

Hello, World
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'read_write_contained',
		      description => 'test that we can read/write contained object params',
		      component => <<'EOF',
% $m->autoflush(1);
% my $req = $m->make_subrequest(comp=>($m->interp->make_component(comp_source => 'hi')));
% $m->autoflush(0);
autoflush for new request is <% $req->autoflush %>
EOF
		      expect => <<'EOF',
autoflush for new request is 1
EOF
		    );

#------------------------------------------------------------

    $group->add_support( path => '/support/static_source',
		         component => <<'EOF',
STATIC 1
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'static_source',
		      description => 'test that static_source option works',
                      interp_params => { static_source => 1 },
		      component => <<'EOF',
<& support/static_source &>\
<%perl>
local *F;
my $comp_root = $m->interp->resolver->comp_root;
my $file = "$comp_root/interp/support/static_source";
open F, ">$file" or die "Cannot write to $file: $!";
print F "STATIC 2\n";
close F;
</%perl>
<& support/static_source &>
EOF
		      expect => <<'EOF',
STATIC 1
STATIC 1
EOF
		    );

#------------------------------------------------------------

    if ( load_pkg('Cache::Cache') )
    {
	$group->add_test( name => 'no_data_dir',
			  description => 'test interp without a data directory',
			  interp => HTML::Mason::Interp->new( comp_root => HTML::Mason::Tests->comp_root ),
			  component => <<'EOF',
Hello World!
<% ref $m->cache %>
EOF
			  expect => <<'EOF',
Hello World!
HTML::Mason::Cache::MemoryCache
EOF
			  );
    }

#------------------------------------------------------------

    $group->add_support( path => 'no_comp_root_helper',
                         component => <<'EOF',
I am rootless
EOF
                       );

#------------------------------------------------------------

    $group->add_test( name => 'no_comp_root',
		      description => 'test interp without a comp root or data dir',
		      component => <<'EOF',
% my $buffer;
% my $interp = HTML::Mason::Interp->new( out_method => \$buffer );
% $interp->exec( '/mason_tests/comps/interp/no_comp_root_helper' );
<% $buffer %>
EOF
		      expect => <<'EOF',
I am rootless
EOF
		    );

#------------------------------------------------------------

    $group->add_test( name => 'make_component_error',
		      description => 'make sure a proper exception is thrown with make_component syntax errors',
		      component => <<'EOF',
% $m->interp->make_component(comp_source => '<% &>');
EOF
                      # Would be better to do $@->isa(syntax-error) or the like.
		      expect_error => qr/without matching/,
		    );

#------------------------------------------------------------

    if ( load_pkg('Switch') )
    {
        $group->add_test( name => 'source_filter',
                          description => 'make sure source filters work',
                          interp_params =>
                          { ignore_warnings_expr =>
                            qr/uninitialized|Subroutine .* redefined/i },
                          component => <<'EOF',
no explosion
<%init>
use Switch;

my $x = 1;

switch ($x) { case 1 { $x = 2 } }
</%init>
EOF
                          expect => <<'EOF',
no explosion
EOF
                        );
    }

#------------------------------------------------------------

    $group->add_test( name => 'escape_flags',
		      description => 'test setting escape flags via constructor',
                      interp_params =>
                      { escape_flags => { uc => sub { ${$_[0]} = uc ${$_[0]} } } },
		      component => <<'EOF',
<% 'upper case' | uc %>
EOF
		      expect => <<'EOF',
UPPER CASE
EOF
		    );


#------------------------------------------------------------


    return $group;
}
