#!/usr/local/bin/perl

use Mail::IMAPClient;
use IO::File;

# Change the following line (or replace it with something better):
my($h,$u,$p) = ('cyrus_host','cyrus_admin_id','cyrus_admin_pswd');

my $imap = Mail::IMAPClient->new(	Server  => "$h",			# "nsusmsg02.net.bms.com",
					User    => "$u",			# $u,	
					Password=> "$p",			# $p,
					Uid	=> 0,				# True value
					Port    => 2143,			# Cyrus
					Debug	=> 0,				# True value
					Buffer	=> 4096*10,				# True value
					Fast_io	=> 0,				# True value
					Timeout	=> 30,				# True value
					# Debug_fh=> IO::File->new(">out.db"),	# fhandle
				) 
or die "$@";

for my $f ( $imap->folders ) {
	print "Expunging $f\n";
	unless ($imap->select($f) ) {
		$imap->setacl($f,$u,"lrswipcda") or warn "Cannot setacl for $f: $@\n" and next;
		$imap->select($f) or warn "Cannot select $f: $@" and next;
	}
	$imap->expunge;
}

