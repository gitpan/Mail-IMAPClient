use ExtUtils::MakeMaker;
use Cwd;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
$DO_BODYSTRUCTURE	= eval "require 'Parse/RecDescent.pm'" ;
unless ($DO_BODYSTRUCTURE) {
	print 
		"*** NOTE ***\n\n",
		"Unable to find and load Parse::RecDescent.\n",
		"Mail::IMAPClient will be installed without support for\n",
		"the get_bodystructure method and without the \n",
		"Mail::IMAPClient::BodyStructure subclasses. \n\n",
		"If you need these subclasses, install Parse::RecDescent \nand ",
		"re-run Makefile.PL. \n\n";
}

WriteMakefile(
    'DIR'		=> [ $DO_BODYSTRUCTURE ? ( 'BodyStructure' , 'Thread' ) : ()  ],
    'NAME'		=> 'Mail::IMAPClient',
    'VERSION_FROM' 	=> 'IMAPClient.pm', # finds $VERSION
    'clean' 		=> {	FILES 	=> 'test.txt'	},
    'PREREQ_PM'		=> { 	
				'Errno'		=> 0,
				'IO::Socket' 	=> 0,
				'Fcntl'		=> 0,
				'IO::Select'	=> 0,
				'IO::File'	=> 0,
				'Data::Dumper'	=> 0,
				'Carp'		=> 0,
    },

);

=begin prereqs

The Mail::IMAPClient module uses the IO::Socket module to make a socket
connection to an IMAP server and the Socket module to get some constants.
It also uses Errno, Fcntl (for faster I/O) and IO::Select, IO::File, 
Data::Dumper, and Carp.

=end prereqs

=cut

&set_test_data;

sub set_test_data {
	unless (-f "./IMAPClient.pm") { warn "ERROR: not in installation directory\n"; return }
	return if -f "./test.txt";
	print 	"You have the option of running an extended suite of tests during\n",
		"'make test'. This requires an IMAP server name, user account, and ",
		"password to test with.","\n","\n",
		"Do you want to run the extended tests? (n/y) ==> ";
	my $yes = <STDIN>;
	return unless $yes =~ /^[Yy](?:[Ee]:[Ss]?)?$/ ;
	unless (open TST,">./test.txt") { warn "ERROR: couldn't open ./test.txt: $!\n"; return }
	print "\nPlease provide the hostname or IP address of a host running an IMAP server \n",
		"(or QUIT to skip the extended tests) ==> ";
	my $server = <STDIN>;
	chomp $server;
	return if $server =~ /^\s+$|^quit$/i ;
	print TST "server=$server\n";
	print "\nPlease provide the username of an account\non $server (or QUIT) ==> ";
	my $user = <STDIN>;
	chomp $user;
	return if $user =~ /^\s+$|^quit$/i ;
	print TST "user=$user\n";
	print "\nPlease provide the password for $user\n(or QUIT) ==> ";
	my $passed = <STDIN>;
	chomp $passed;
	return if $passed =~ /^\s+$|^quit$/i ;
	print TST "passed=$passed\n";
	print "\nPlease provide the port to connect to on $server to run the test \n",
		"(default is 143) ==> ";
	my $port = <STDIN>;
	chomp $port;
	$port ||= 143; 
	print TST "port=$port\n";
	close TST;
	print "\nGracias! The information you provided (including the \n",
		" password!) has been stored in ",cwd,"/test.txt and should be\n",
		" removed (either by hand or by 'make clean') after testing.\n";
}

