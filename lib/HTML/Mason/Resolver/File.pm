# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

package HTML::Mason::Resolver::File;
require 5.004;
require Exporter;
@ISA = qw(HTML::Mason::Resolver);
@EXPORT = qw();
@EXPORT_OK = qw();

use HTML::Mason::Resolver;
use strict;

#
# Public API

#
# Given a component path, return the fully-qualified path (which in
# our case is the same) plus auxiliary information that will be passed
# to the get_* methods below.
#
sub lookup_path {
    my ($self,$path,$interp) = @_;
    my $srcfile = $interp->comp_root . $path;
    my @srcstat = stat $srcfile;
    return (-f _) ? ($path, $srcfile, $srcstat[9]) : undef;
}

#
# Given a filename, return the associated component path or undef if
# none exists. This is called for top-level web requests that resolve
# to a particular file.
#
sub file_to_path {
    my ($self,$file,$interp) = @_;
    my $compRoot = $interp->comp_root;
    return undef unless ((my $compPath = $file) =~ s/^$compRoot//);
    $compPath =~ s/\/$// unless $compPath eq '/';
    return $compPath;
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

