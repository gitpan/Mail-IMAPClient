#!/usr/local/bin/perl

use Mail::IMAPClient;    # available from http://search.cpan.org/search?mode=module&query=IMAPClient
use IO::File;
use Getopt::Std;
use vars qw/ $opt_d $opt_s $opt_p $opt_u $opt_P $opt_h /;

&getopts('d:s:u:p:P:h');	# -d days_to_keep -u cyrys_user -p cyrus_pswd -s cyrus_server -P port

my $days_to_keep = $opt_d||365;       			# Delete msgs older than -d arg or 365 days
my $cutoff = time - ( $days_to_keep * 24 * 60 * 60 ) ;  # time - arg * 24 * 60 * 60 = cutoff date in seconds 

# Change the following line (or replace it with something better):
$opt_h and die help()."\n";
my $h = $opt_s || "localhost" ; 
my $u = $opt_u || "cyrys" ;
my $p = $opt_p or die "Unable to continue. No password provided.\n" . help();

my $imap = Mail::IMAPClient->new( 
     Server  => "$h", 
     User    => "$u",      		# $u, 
     Password=> "$p",			# $p,
     Uid => 1,				# True value
     Port    => $opt_P||143,            # imapd
     Debug => 0,                        # Make true to debug
     Buffer => 4096*10,    		# True value; decrease on machines w/little memory
     Fast_io => 1,              	# True value
     Timeout => 30,          		# True value
     # Debug_fh=> IO::File->new(">out.db"), # fhandle
    ) 
or die "$@";
 my $mcnt = my $fcnt = 0;
print "Deleting messages older than ",$imap->Rfc2060_date($cutoff),"\n";
for my $f ( $imap->folders ) {
 print "Expiring $f\n";
 unless ($imap->select($f) ) {
  $imap->setacl($f,$u,"lrswipcda") or warn "Cannot setacl for $f: $@\n" and next;
  $imap->select($f) or warn "Cannot select $f: $@" and next;
 }
 my @expired = $imap->search("SENTBEFORE",$imap->Rfc2060_date($cutoff));
 next unless @expired;
 $mcnt += scalar(@expired); $fcnt ++;
 print "Deleting ",scalar(@expired)," messages from $f\n";
 $imap->delete_message(@expired);
 $imap->expunge;
 $imap->close;
}
 $imap->logout;
 print "Deleted a total of $mcnt messages in $fcnt folders.\n";
exit;


sub help {
	return <<"EOHELP";

Usage:

	$0 [ -d days_to_keep ] [ -s mail_server ] [ -u cyrus_admin_id ] -p cyrus_password
	$0 -h 

	-h 		  -- prints this here help message
	-d days_to_keep   -- $0 will delete messages older than "days_to_keep". (Default is 365)
	-s mail_server    -- hostname or IP Address of IMAP mail server (defaults to "localhost")
	-u cyrus_admin_id -- user name of Unix account that owns Cyrus server (defaults to "cyrus")
	-p cyrus_password -- password for the "cyrus_admin_id" user account (no default)
	-P cyrus_port     -- port where the cyrus imapd daemon is listening (defaults to value from 
				/etc/services or '143')

EOHELP

}
