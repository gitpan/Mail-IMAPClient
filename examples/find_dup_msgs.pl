#!/usr/local/bin/perl
# $Id: find_dup_msgs.pl,v 19991216.3 2000/12/11 21:58:52 dkernen Exp $

use Mail::IMAPClient;
use Getopt::Std;

#
# This script accepts three arguments, a uid, a password, and a mailhost.
# It then connects to that uid's mailhost and rummages around, looking for duplicate messages.
# It will optionally delete messages that are duplicates (based on msg-id header 
#         and number of bytes).
# It is an example of how to use various imap commands, including search, parse_headers, and
# 	size. It is also an example of how this module was used to fix a real-life problem.
#

getopts('dv');
my $uid = $ARGV[0] or die "Invalid argument.\n\nUsage:\n\t$0 [-d] uid\n\t-d\tdelete duplicates\n\n";
my $psw = $ARGV[1] or die "Invalid argument.\n\nUsage:\n\t$0 [-d] uid\n\t-d\tdelete duplicates\n\n";
my $host = $ARGV[2] or die "Invalid argument.\n\nUsage:\n\t$0 [-d] uid\n\t-d\tdelete duplicates\n\n";

print "Connecting to $host:143\n" if $opt_v;
my $imap = Mail::IMAPClient->new(	
			Server	=> $host,
			User	=> $uid,
			Password=> $psw,
) or die "couldn't connect to $host port 143: $!\n";

my %folders; my %counts;
FOLDER: foreach my $f ($imap->folders) {
	$folders{$f} = 0;
	$counts{$f} = $imap->message_count($f);
	print "Processing folder $f\n" if $opt_v;
	unless ( $imap->select($f)) {
		warn "Error selecting $f: " . $imap->LastError . "\n";
		next FOLDER;
	}
	my @msgs = $imap->search("ALL");
	my %hash = ();
	foreach my $m (@msgs) {
		my $mid = $imap->parse_headers($m,"Message-ID")->{'Message-ID'}[0];
		my $size = $imap->size($m);
		if ( exists $hash{$mid} and $hash{$mid} == $size ) { 
			$imap->delete_message($m) if $opt_d;
			$folders{$f}++;
			print "Found a duplicate in $f\n" if $opt_v;
		} else {
			$hash{$mid} = $size;
		}
	}
}
foreach my $k (keys %folders) { $total += $folders{$k}}
my $totms; 
map { $totms += $_ } values %counts;

print 	"Found $total duplicate messages in $uid's mailbox. The breakdown is:\n",
	"\tFolder\tNumber of Duplicates\tNumber of Msgs in Folder\n",
	"\t------\t--------------------\t------------------------\n",
	map { "\t$_\t$folders{$_}\t$counts{$_}\n" } keys %folders,
	"\tTOTAL\t$total\t$totms\n";


# History:
# $Log: find_dup_msgs.pl,v $
# Revision 19991216.3  2000/12/11 21:58:52  dkernen
#
# Modified Files:
# 	build_dist.pl build_ldif.pl copy_folder.pl find_dup_msgs.pl
# 	imap_to_mbox.pl populate_mailbox.pl
# to add CVS data
#
