# Copyright (c) 1998-2003 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Resolver::File;

use strict;

use Cwd;

BEGIN
{
    # Only available with 5.006 +
    if ( eval { require File::Glob } )
    {
        *_glob = sub { File::Glob::bsd_glob($_[0]) };
    }
    else
    {
        *_glob = sub { glob($_[0]) };
    }
}

use File::Spec;
use HTML::Mason::Tools qw(read_file_ref paths_eq);
use Params::Validate qw(:all);

use HTML::Mason::ComponentSource;
use HTML::Mason::Resolver;
use base qw(HTML::Mason::Resolver);

use HTML::Mason::Exceptions (abbr => ['param_error']);

__PACKAGE__->valid_params
    (
     comp_root =>
     { parse => 'list',
       type => SCALAR|ARRAYREF,
       default => File::Spec->rel2abs( Cwd::cwd ),
       descr => "A string or array of arrays indicating the search path for component calls" },
    );

sub new {
    my $package = shift;

    my $self = $package->SUPER::new(@_);

    # Force comp_root into lol format
    $self->{comp_root} = [[ MAIN => $self->{comp_root} ]] unless ref $self->{comp_root};

    # Check that directories are absolute.
    foreach my $pair ($self->comp_root_array) {
	param_error "Multiple-path component root must consist of a list of two-element lists; see documentation"
	    if ref($pair) ne 'ARRAY';
	$pair->[1] = File::Spec->canonpath( $pair->[1] );
	param_error "comp_root '$pair->[1]' is not an absolute directory"
	    unless File::Spec->file_name_is_absolute( $pair->[1] );
    }

    return $self;
}

sub comp_root_array
{
    return @{ $_[0]->{comp_root} };
}

sub comp_root
{
    my $self = shift;
    die "Resolver comp_root is read-only" if @_;
    return $self->{comp_root}[0][1] if @{$self->{comp_root}} == 1 and $self->{comp_root}[0][0] eq 'MAIN';
    return $self->{comp_root};
}

sub get_info {
    my ($self, $path) = @_;

    foreach my $pair ($self->comp_root_array) {
	my $srcfile = File::Spec->canonpath( File::Spec->catfile( $pair->[1], $path ) );
	next unless -f $srcfile;

	my $key = $pair->[0];

	my $modified = (stat _)[9];
	my $base = $key eq 'MAIN' ? '' : "/$key";
	$key = undef if $key eq 'MAIN';

	return
            HTML::Mason::ComponentSource->new
                    ( friendly_name => $srcfile,
                      comp_id => "$base$path",
                      last_modified => $modified,
                      comp_path => $path,
                      comp_class => 'HTML::Mason::Component::FileBased',
                      extra => { comp_root => $key },
                      source_callback => sub { read_file_ref($srcfile) },
                    );
    }

    if ( $path ne '/' )
    {
        # see if path corresponds to real filesystem path, a common new user mistake
        my $fs_path = File::Spec->catfile( split /\//, $path );
        if ( defined $fs_path && -e $fs_path )
        {
            warn "Your component path ($path) matches a real file on disk ($fs_path).  Have you read about the component root in the Administrator's Manual (HTML::Mason::Admin)?";
        }
    }

    return;
}

#
# Given a glob pattern of url_paths, return all existing url_paths for that glob.
#
sub glob_path {
    my ($self,$pattern) = @_;
    my @roots = map $_->[1], $self->comp_root_array or return;

    my %path_hash;
    foreach my $root (@roots) {
	my @files = _glob($root.$pattern);
	foreach my $file (@files) {
            next unless -f $file;
	    if (substr($file, 0, length $root) eq $root) {
		$path_hash{ substr($file, length $root) } = 1;
	    }
	}
    }
    return keys(%path_hash);
}


1;

__END__

=head1 NAME

HTML::Mason::Resolver::File - Component path resolver for file-based components

=head1 SYNOPSIS

  my $resolver = HTML::Mason::Resolver::File->new( comp_root => '/var/www/mason' );

  my $info = $resolver->get_info('/some/comp.html');
  my $comp_root = $resolver->comp_root;

=head1 DESCRIPTION

This HTML::Mason::Resolver subclass is used when components are stored
on the filesystem, which is the norm for most Mason-based applications.

=head1 PARAMETERS TO THE new() CONSTRUCTOR

The C<new> method takes a single parameter, C<comp_root>.

=over

=item comp_root

The component root marks the top of your component hierarchy and
defines how component paths are translated into real file paths. For
example, if your component root is F</usr/local/httpd/docs>, a component
path of F</products/index.html> translates to the file
F</usr/local/httpd/docs/products/index.html>.

Under L<Apache|HTML::Mason::ApacheHandler> and
L<CGI|HTML::Mason::CGIHandler>, comp_root defaults to the server's
document root. In standalone mode comp_root defaults to the current
working directory.

This parameter may be either a scalar or an array reference.  If it is
a scalar, it should be a filesystem path indicating the component
root. If it is an array reference, it should be of the following form:

 [ [ key1 => '/path/to/root' ],
   [ key2 => '/path/to/other/root' ] ]

The "keys" for each path must be unique names and their "values" must
be filesystem paths.  These paths will be searched in the provided
order whenever a component path must be resolved to a filesystem path.

If no component root is specified, the current working directory is
used.

=back

=head1 ADDITIONAL METHODS

=over 4

=item comp_root

This method returns the component root, which will either be a scalar
or an array reference, as documented L<here|HTML::Mason::Resolver::File/item_comp_root>.

=item comp_root_array

Returns the component root as an array of name/path pairs.  Can be
used like the following:

  foreach my $pair ($self->comp_root_array) {
    my ($name, $path) = @$pair;
    ...
  }

=back

=head1 SEE ALSO

L<HTML::Mason|HTML::Mason>

=cut
