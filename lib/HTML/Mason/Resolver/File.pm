# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver::File;

use strict;

use File::Spec;
use HTML::Mason::Resolver;
use HTML::Mason::Tools qw(paths_eq);

use vars qw(@ISA);

@ISA = qw(HTML::Mason::Resolver);

#
# Public API
#
# Given a component path, return the fully-qualified path, plus
# auxiliary information that will be passed to the get_* methods
# below.
#
# With a single component root, the fully-qualified path is just
# the component path. With multiple component roots, we search
# through each root in turn, and the fully-qualified path is
# uc(root key) + component path.
#
sub lookup_path {
    my ($self,$path,$interp) = @_;
    my $comp_root = $interp->comp_root;
    if (!ref($comp_root)) {
	my $srcfile = File::Spec->catdir( $comp_root, $path );
	my @srcstat = stat $srcfile;
	return (-f _) ? ($path, $srcfile, $srcstat[9]) : undef;
    } elsif (UNIVERSAL::isa($comp_root, 'ARRAY')) {
	foreach my $lref (@$comp_root) {
	    my ($key,$root) = @$lref;
	    $key = uc($key);   # Always make key uppercase in fqpath
	    my $srcfile = File::Spec->catfile( $root, $path );
	    my @srcstat = stat $srcfile;
	    my $fq_path = File::Spec->canonpath( File::Spec->catfile( File::Spec->rootdir, $key, $path ) );
	    return ($fq_path, $srcfile, $srcstat[9]) if (-f _);  # File::Spec?
	}
	return undef;
    } else {
	HTML::Mason::Exception::Params->throw( error => "comp_root must be a scalar or listref" );
    }
}

#
# Given a glob pattern, return all existing paths.
#
sub glob_path {
    my ($self,$pattern,$interp) = @_;
    my $comp_root = $interp->comp_root;
    my @roots;
    if (!ref($comp_root)) {
	@roots = ($comp_root);
    } elsif (ref($comp_root) eq 'ARRAY') {
	@roots = map($_->[1],@{$comp_root});
    } else {
	HTML::Mason::Exception::Params->throw( error => "comp_root must be a scalar or listref" );
    }
    my %path_hash;
    foreach my $root (@roots) {
	my @files = glob($root.$pattern);
	foreach my $file (@files) {
	    if (my ($path) = ($file =~ m/$root(\/.*)$/)) {  # File::Spec?
		$path_hash{$path}++;
	    }
	}
    }
    return keys(%path_hash);
}

#
# Given a filename, return the associated component path or undef if
# none exists. This is called for top-level web requests that resolve
# to a particular file.
#
sub file_to_path {
    my ($self,$file,$interp) = @_;
    my $comp_root = $interp->comp_root;
    my @roots;
    
    if (!ref($comp_root)) {
	@roots = ($interp->comp_root);
    } elsif (ref($comp_root) eq 'ARRAY') {
	@roots = map($_->[1],@{$comp_root});
    } else {
	HTML::Mason::Exception::Params->throw( error => "comp_root must be a scalar or listref" );
    }
    foreach my $root (@roots) {
	if (paths_eq($root,substr($file,0,length($root)))) {
	    my $path = substr($file, ($root eq '/' ? 0 : length($root)));
	    $path =~ s/\/$// unless $path eq '/';
	    return $path;
	}
    }
    return undef;
}

#
# Return the last modified time of the source file.
#
sub get_last_modified {
    return $_[3];
}

#
# Return filename for error messages etc.
#
sub get_source_description {
    return $_[2];
}

#
# Return parameters to pass to make_component.
#
sub get_source_params {
    return (script_file=>$_[2], comp_class=>'HTML::Mason::Component::FileBased');
}

1;
