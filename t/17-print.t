use strict;

use Cwd;
use File::Spec;

use HTML::Mason::Interp;

print "1..4\n";

my $comp_root = File::Spec->catdir( getcwd(), 'mason_tests', 'comps' );
($comp_root) = $comp_root =~ /(.*)/;
my $data_dir = File::Spec->catdir( getcwd(), 'mason_tests', 'data' );
($data_dir) = $data_dir =~ /(.*)/;

my $interp = HTML::Mason::Interp->new( comp_root => $comp_root,
				       data_dir => $data_dir,
				     );

{
    my $source = <<'EOF';
ok 1
% print "ok 2\n";
EOF

    my $comp = $interp->make_component( comp_source => $source );

    my $req = $interp->make_request;

    $req->exec($comp);
}

# same stuff but with autoflush
{
    my $source = <<'EOF';
ok 3
% print "ok 4\n";
EOF

    my $comp = $interp->make_component( comp_source => $source );

    my $req = $interp->make_request( autoflush => 1 );

    $req->exec($comp);
}
