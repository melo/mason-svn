# Copyright (c) 1998-2002 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify
# it under the same terms as Perl itself.

package HTML::Mason::Component::FileBased;

use strict;

use File::Basename;
use File::Spec;

use HTML::Mason::Component;
use base qw(HTML::Mason::Component);

use HTML::Mason::Exceptions( abbr => ['error'] );

use HTML::Mason::MethodMaker ( read_only => [ qw( path source_file name dir_path ) ] );

sub is_file_based { 1 }
sub persistent { 1 }
sub source_dir {
    my $dir = dirname($_[0]->source_file);
    return File::Spec->canonpath($dir);
}
sub title {
    my ($self) = @_;
    return $self->path . ($self->{source_root_key} ? " [".lc($self->{source_root_key})."]" : "");
    #return $self->path . ($self->{source_root_key} ? " [$self->{source_root_key}]" : "");
}

# Ends up setting $self->{path, source_root_key, source_file} and a few in the parent class
sub assign_runtime_properties {
    my ($self, $interp, $info) = @_;

    $self->{source_file} = $info->friendly_name;
    $self->{source_root_key} = $info->extra->{comp_root};

    # We used to use File::Basename for this but that is broken
    # because URL paths always use '/' as the dir-separator but we
    # could be running on any OS.
    #
    # The regex itself it taken from File::Basename.
    #
    @{$self}{ 'dir_path', 'name'} = $info->comp_path =~ m,^(.*/)?(.*),s;

    $self->SUPER::assign_runtime_properties($interp, $info);
}

1;
