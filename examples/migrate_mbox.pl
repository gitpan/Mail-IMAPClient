#!/usr/local/bin/perl
#
# This is an example demonstrating the use of the migrate method.
# Note that the migrate method is considered experimental and should 
# be used with caution. 
#
# 

use Mail::IMAPClient;
use IO::File;
use File::Basename ;
use Getopt::Std;
use warnings;
use vars qw/$opt_h $opt_H 
	$opt_s $opt_u $opt_p $opt_d $opt_b $opt_o
	$opt_S $opt_U $opt_P $opt_D $opt_B $opt_O
/;

getopts('Hhs:S:u:U:p:P:d:D:b:B:o:O:');
if ($opt_h or $opt_H ) {
print << "HELP";


Usage:

$0 -[h|H]	-- prints this message

Lower-case options are for source server; upper-case options are for the target server.

$0 	-s server -S server -u uid -U uid -p passwd -P passwd \
	-b buffersize -B buffersize -o debugFile -O debugFile > error_file

All uppercase options except -O default to the lowercase option that was specified. 
If you don't specify any uppercase options at all then God help you, I don't know 
what will happen.

Always capture STDERR so that you'll be able to resolve any problems that come up.


HELP

exit;
}

my $imap = Mail::IMAPClient->new(
	Server  => $opt_s,
	User    => $opt_u,
	Password=> $opt_p,
	Uid	=> 1,
	Debug	=> $opt_d,
	Buffer	=> $opt_b||4096,
	Fast_io	=> 1,
	Timeout	=> 160,			   # True value
	Debug_fh=> ( 
		$opt_o ? IO::File->new(">$opt_o")||die "can't open $opt_o: $!\n" : undef ) 
) or die "Error opening source connection: $@\n";

my $imap2 = Mail::IMAPClient->new(	
	Server  => $opt_S||$opt_s,
	User    => $opt_U||$opt_u,
	Password=> $opt_P||$opt_p,
	Uid	=> 1,
	Debug	=> $opt_D||$opt_d,
	Buffer	=> $opt_B||$opt_b||4096,
	Fast_io	=> 1,
	Timeout	=> 160,	
	Debug_fh=> ( 
		$opt_O ? IO::File->new(">$opt_O")||die "can't open $opt_O: $!\n" : undef ) 
) or die "Error opening target connection: $@\n";


$imap->Debug_fh->autoflush;
$imap2->Debug_fh->autoflush;

for my $f ($imap->folders) { $imap->select($f) ; $imap->migrate($imap2,"ALL") ;}
