package HTML::Mason;
# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use 5.005;

$HTML::Mason::VERSION = '1.1';

use HTML::Mason::Interp;

use constant DEBUG => 1;

sub version
{
    shift;

    return $HTML::Mason::VERSION;
}

1;
