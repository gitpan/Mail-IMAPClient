#!/home/dkernen/usr/local/bin/perl

use Mail::IMAPClient;
use MIME::Lite;

=head1 DESCRIPTION

B<build_ldif.pl> accepts the name of a target folder as an argument. It
then opens that folder and rummages through all the mail files in it, looking
for "Reply-to:" headers (or "From:" headers, where there is no "Reply-to:").
It then prints to STDOUT a file in ldif format containing entries for all of the
addresses that it finds. It also appends a message into the specified folder containing
all of the addresses in both the B<To:> field of the message header and in an 
LDIF-format attachment.

B<build_ldif.pl> requires B<MIME::Lite>.

=head1 SYNTAX

b<build_ldif.pl> I<-h>

b<build_ldif.pl> I<-s servername -u username -p password -f folder [ -d ]>

=over 4

=item -f The folder name to process.

=item -s The servername of the IMAP server 

=item -u The user to log in as

=item -p The password for the user specified in the I<-u> option

=item -d Tells the IMAP client to turn on debugging info

=item -h Prints out this document

=back

B<NOTE:> You can supply defaults for the above options by updating the script.

=cut

use Getopt::Std;

getopts('s:u:p:f:d');

# Update the following to supply defaults:

$opt_f ||= "default folder";
$opt_s ||= "default server";
$opt_u ||= "default user";
$opt_p ||= "default password";	# security risk: use with caution!

# Let the compiler know we're serious about these two variables:
$opt_h = $opt_h or $opt_d = $opt_d ; 

exec "perldoc $0" if $opt_h;

my $imap = Mail::IMAPClient->new( 
		Server 	=> $opt_s ,
		User 	=> $opt_u ,
		Password=> $opt_p ,
		Debug	=> $opt_d||0 ,
) or die "can't connect to server\n";

$imap->select($opt_f);

my @msgs = $imap->search("NOT SUBJECT",qq("buid_ldif.pl $opt_f Output"));
my %list;
foreach my $m (@msgs) {

	my $ref = $imap->parse_headers($m,"Reply-to","From");
	
	warn "Couldn't get recipient address from msg#$m\n" 
		unless 	scalar(@{$ref->{'Reply-to'}})   ||
			scalar(@{$ref->{'From'}})	;	

	my $from = scalar(@{$ref->{'Reply-to'}}) 	? 
			  $ref->{'Reply-to'}[0]		: 
			  $ref->{'From'}[0] 		;
	my $name = $from				;

	$name =~ s/<.*//				;
	if ($name =~ /\@/) {
		$name = $from				;
		$name =~ s/\@.*//;			;
	}
	$name =~ s/\"//g				;
	$name =~ s/^\s+|\s+$//g				;
	my $addr = $from				; 
	$addr =~ s/.*<//				;
	$addr =~ s/[\<\>]//g				;
	$list{lc($addr)} = [ $addr, $name ]
		unless exists $list{lc($addr)} 		;
}

my $text = join "",map {
	qq{dn: cn="} . $list{$_}[1] . 
	qq{", mail=$list{$_}[0]\n} .
	qq{cn: } . $list{$_}[1] . qq{\n} .
	qq{mail: $list{$_}[0]\n} .
	qq{objectclass: top\nobjectclass: person\n\n};
} keys %list ;

# Create a new multipart message:
my $msg = MIME::Lite->new(
        From    => $opt_u,
        map({ ("To" => $list{$_}[0]) } keys %list),
        Subject => "LDIF file from $opt_f",
        Type    =>'TEXT',
        Data    =>"Attached is the LDIF file of addresses from folder $opt_f."
);
$msg->attach(	Type     =>'text/ldif',
		Filename => "$opt_f.ldif",
                Data 	 => $text ,
);
print $text;
$imap->append($opt_f, $msg->as_string);
$imap->logout;
