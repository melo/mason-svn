# Copyright (c) 1998-2000 by Jonathan Swartz. All rights reserved.
# This program is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.

#
# Miscellaneous tools used by the other Mason modules.  Some of these
# admittedly exist in better versions on CPAN but we rewrite them so
# as to minimize external package requirements.
#

package HTML::Mason::Tools;

use strict;

use Cwd;

require Exporter;

use vars qw(@ISA @EXPORT_OK);

@ISA = qw(Exporter);
@EXPORT_OK = qw(read_file chop_slash html_escape url_escape url_unescape date_delta_to_secs dumper_method paths_eq is_absolute_path make_absolute_path compress_path pkg_loaded pkg_installed);

#
# Return contents of file. If $binmode is 1, read in binary mode.
#
sub read_file
{
    my ($file,$binmode) = @_;
    die "read_file: '$file' does not exist" if (!-e $file);
    die "read_file: '$file' is a directory" if (-d _);
    my $fh = do { local *FH; *FH; };
    open $fh, $file
	or die "read_file: could not open file '$file' for reading: $!";
    binmode $fh if $binmode;
    local $/ = undef;
    my $text = <$fh>;
    return $text;
}

#
# Remove final slash from string, if any; return resulting string.
#
sub chop_slash
{
    my ($str) = (@_);
    $str =~ s@/$@@;
    return $str;
}

#
# Escape HTML &, >, <, and " characters. Borrowed from CGI::Base.
#
sub html_escape
{
    my ($text) = @_;
    my %html_escape = ('&' => '&amp;', '>'=>'&gt;', '<'=>'&lt;', '"'=>'&quot;');
    my $html_escape = join('', keys %html_escape);
    $text =~ s/([$html_escape])/$html_escape{$1}/mgoe;
    return $text;
}

#
# Unescape URL-encoded data. Borrowed from CGI.
#
sub url_unescape {
    my $todecode = shift;
    return undef unless defined($todecode);
    $todecode =~ tr/+/ /;       # pluses become spaces
    $todecode =~ s/%([0-9a-fA-F]{2})/pack("c",hex($1))/ge;
    return $todecode;
}

#
# URL-encode data. Borrowed from CGI.
#
sub url_escape {
    my $toencode = shift;
    return undef unless defined($toencode);
    $toencode=~s/([^a-zA-Z0-9_.-])/uc sprintf("%%%02x",ord($1))/eg;
    return $toencode;
}

#
# Convert a "date delta string" (e.g. 1sec, 3min, 2h) to a number of
# seconds. Based on Date::Manip date delta concept.
#
my %date_delta = ('y'=>31557600, yr=>31557600, year=>31557600, years=>31557600,
		  'm'=>2592000, mon=>2592000, month=>2592000, months=>2592000,
		  'w'=>604800, wk=>604800, ws=>604800, wks=>604800, week=>604800, weeks=>604800,
		  'd'=>86400, day=>86400, days=>86400,
		  'h'=>3600, hr=>3600, hour=>3600, hours=>3600,
		  mn=>60, min=>60, minute=>60, minutes=>60,
		  's'=>1, sec=>1, second=>1, seconds=>1
		 );
sub date_delta_to_secs
{
    my ($delta) = @_;
    my $usage = "date_delta_to_secs: invalid argument '$delta'";
    my ($num,$unit,$sign);
    if ($delta =~ /^([-+]?)\s*([0-9]+)\s*([a-zA-Z]*)\s*$/) {
	($sign,$num,$unit) = ($1,$2,lc($3));
    } else {
	die $usage;
    }
    $unit = "s" if !$unit;
    my $mult = $date_delta{$unit};
    die $usage if !$mult;
    return $num * $mult * ($sign eq '-' ? -1 : 1);
}

#
# Call the XS or normal version of Data::Dumper::Dump depending on what's installed.
#
sub dumper_method {
    my ($d) = @_;
    return ($HTML::Mason::Config{use_data_dumper_xs} ? $d->Dumpxs : $d->Dump);
}

#
# Determines whether two paths are equal, taking into account
# case-insensitivity in Windows O/S.
#
sub paths_eq {
    return (lc($^O) =~ /^ms(dos|win32)/) ? (lc($_[0]) eq lc($_[1])) : $_[0] eq $_[1];
}

#
# Determines whether a pathname is absolute: beginning with / or ~/ or a
# drive letter (e.g. C:/).
#
sub is_absolute_path
{
    return $_[0] =~ /^(([A-Za-z]:)|~\w*)?\//;
}
    
#
# Return an absolute version of a pathname.  No change if already absolute.
#
sub make_absolute_path
{
    my ($path) = @_;
    unless (is_absolute_path($path)) {
	$path = cwd() . $path;
    }
    return $path;
}

sub compress_path
{
    my ($path) = @_;
    for ($path) {
	s@^/@@;
	s/([^\w\.\-\~])/sprintf('+%02x', ord $1)/eg;
    }
    return $path;
}

sub mason_canonpath {
    # Just like File::Spec::canonpath, but we're having trouble
    # getting a patch through to them.
    shift;
    my $path = shift;
    $path =~ s|/+|/|g unless($^O eq 'cygwin');       # xx////yy  -> xx/yy
    $path =~ s|(/\.)+/|/|g;                          # xx/././yy -> xx/yy
    {
	$path =~ s|^(\./)+||s unless $path eq "./";  # ./xx      -> xx
	$path =~ s|^/(\.\./)+|/|s;                   # /../../xx -> xx
	$path =~ s|/\Z(?!\n)|| unless $path eq "/";  # xx/       -> xx
	$path =~ s|[^/]+/\.\./|| && redo;            # /xx/../yy -> /yy
    }
    return $path;
}

no strict 'refs';

#
# Determine if package is installed without loading it, by checking
# the INC path.
#
sub pkg_installed
{
    my ($pkg) = @_;

    (my $pkgfile = "$pkg.pm") =~ s{::}{/}g;
    return grep(-f "$_/$pkgfile",@INC);
}

#
# Determined if package is loaded by checking for its version.
#
sub pkg_loaded
{
    my ($pkg) = @_;

    my $varname = "${pkg}::VERSION";
    return $$varname ? 1 : 0;
}

