package HTML::Mason;
# Copyright (c) 1998-99 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

use 5.004;

$HTML::Mason::VERSION = '1.1';

use HTML::Mason::Parser;
use HTML::Mason::Interp;

if ($ENV{MOD_PERL} || $HTML::Mason::IN_DEBUG_FILE) {
    eval 'use HTML::Mason::ApacheHandler'; die $@ if $@;
} else {
    *HTML::Mason::ApacheHandler::new = sub {
	die "HTML::Mason::ApacheHandler class is not defined.  If you are in a mod_perl environment, this should have been brought in automatically.  Send a bug report to the Mason development team, and for now, try using HTML::Mason::ApacheHandler manually in your handler.pl.\n";
    }
}

1;
