#!/usr/bin/perl
package HTML::Mason;

#
# Sample Mason handler.
#
use HTML::Mason;
use strict;

# Uncomment the next line if you plan to use the Mason previewer.
#use HTML::Mason::Preview;

# List of modules that you want to use from components (see Admin
# manual for details)
#{  package HTML::Mason::Commands;
#   use CGI;
#   use Apache::Session::File;
#}

# Create Mason objects
#
my $parser = new HTML::Mason::Parser;
my $interp = new HTML::Mason::Interp (parser=>$parser,
                                      comp_root=>'<component root>',
                                      data_dir=>'<data directory>');
my $ah = new HTML::Mason::ApacheHandler (interp=>$interp);

# Activate the following if running httpd as root (the normal case).
# Resets ownership of all files created by Mason at startup. Change
# these to match your server's 'User' and 'Group'.
#
#chown ( [getpwnam('nobody')]->[2], [getgrnam('nobody')]->[2],
#        $interp->files_written );

sub handler
{
    my ($r) = @_;

    # If you plan to intermix images in the same directory as
    # components, activate the following to prevent Mason from
    # evaluating image files as components.
    #
    #return -1 if $r->content_type && $r->content_type !~ m|^text/|io;
    
    # This block of code can be enabled to create a session-hash that every
    # component can access.  This is useful for maintaining state across
    # multiple requests.  The Apache::Session module is required.
    
    #my %session;
    #my $cookie = $r->header_in('Cookie');
    #$cookie =~ s#SESSION_ID=(\w*)#$1#;
    #tie %session, 'Apache::Session::File', $cookie, {'Directory' => '/tmp/session'};
    #if ( !$cookie ) {
    #  $r->header_out("Set-Cookie" => "SESSION_ID=$session{_session_id};");
    #}
    
    # This creates a global called %session that is accessible in all components.
    # Feel free to rename this as needed (%udat, anyone)?
    #local *HTML::Mason::Commands::session = \%session;
    
    $ah->handle_request($r);
    
    #untie %HTML::Mason::Commands::session;
}

1;
