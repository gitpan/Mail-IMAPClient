#!/usr/local/bin/perl

use Mail::IMAPClient;
use Getopt::Std;
use File::Basename;
getopts('s:u:p:f:dh');

if ($opt_h) {
		
	print STDERR "$0	-- example of how to select shared folder\n",
	"\n\nUsage:\n",
	"\t-s	server	-- specify name or ip address of mail server\n",
	"\t-u	userid	-- specify login name of authenticating user\n",
	"\t-p	passwd	-- specify login password of authenticating user\n",
	"\t-f	folder	-- specify shared folder to access (i.e. '-f frank/INBOX')\n",
	"\t-h		   display this help message\n\n";
	"\t-d		   turn on debugging output\n\n";
	exit;	
}
		
my $server = $opt_s or die "No server name specified\n";
my $user   = $opt_u or die "No user name specified\n";
my $pass   = $opt_p or die "No password specified\n";
my $folder = $opt_f or die "No shared folder specified\n";

chomp $pass;
my $imap = Mail::IMAPClient->new(Server=>$server,User=>$user,Password=>$pass,Debug=>$opt_d)
	or die "Can't connect to $user\@$server: $@ $!\n";

my($prefix,$prefSep) = @{$imap->namespace->[1][0]}
	or die "Can't get shared folder namespace or separator: $@\n";


my $target =  	$prefix .  
		( $prefix =~ /\Q$prefSep\E$/ || $opt_f =~ /^\Q$prefSep/ ? "" : $prefSep ) . 
		$opt_f ;

print "Selecting $target\n";

$imap->select($target)
	or die "Cannot select $target: $@\n";

print "Ok: $target has ", $imap->message_count($target)," messages.\n";

$imap->logout;
exit;
