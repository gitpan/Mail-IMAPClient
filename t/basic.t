# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# $Id: basic.t,v 19991216.7 2000/01/12 18:58:05 dkernen Exp $
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

END {print "not ok 1\n" unless $main::loaded;}
use Mail::IMAPClient;

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my $test = 1;
my %parms;
my $imap;
my @tests;
my $uid;

BEGIN { 
	my $target; my $sep;

	
	push @tests, sub {	# 2
		if (ref($imap)) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 3
		if ($sep = $imap->separator) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 4
		my $isparent;
		$isparent = $imap->is_parent(INBOX);
		if (defined($isparent)) {
			$target = "INBOX${sep}IMAPClient_$$";
			print "ok ",++$test,"\n";
		} else {	
			$target = "IMAPClient_$$";
			print "ok ",++$test,"\n";
		}
		# print "target is $target\n";
	};

	
	push @tests, sub {	# 5
		if ( eval { $imap->select('inbox') } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			# print $imap->History,"\n";
		}
	};

	push @tests, sub {	# 6
		if ( eval { $imap->create("$target") } ) {
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 7,8,9
		if (defined($imap->is_parent($target))) {
			if ( eval { $imap->create(qq($target${sep}has "quotes")) } ) {
				print "ok ",++$test,"\n";
			} else {
                          if ($imap->LastError =~ /NO Invalid.*name/) {
                                print "ok skipping ",++$test,
				 " $parms{server} doesn't support quotes in folder names--",
				 "skipping next 2 tests\n";
                                print "ok ", ++$test," (skipped)\n";
                                print "ok ", ++$test," (skipped)\n";
                                return;
                          } else {
                                print "not ok ",++$test,"\n";
                                print "ok ", ++$test," (skipped)\n";
                                print "ok ", ++$test," (skipped)\n";
				return;
                          }

			}
			if ( eval { $imap->select(qq($target${sep}has "quotes")) } ) {
				print "ok ",++$test,"\n";
			} else {
				print "not ok ",++$test,"\n";
			}
			$imap->select('inbox');
			if ( eval { $imap->delete(qq($target${sep}has "quotes")) } ) {
				print "ok ",++$test,"\n";
			} else {
				print "not ok ",++$test,"\n";
			}
			# print $imap->Report;
		} else { 
			print "ok ",++$test,"\n";
			print "ok ",++$test,"\n";
			print "ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 10
		if ( eval { $imap->exists("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};


	push @tests, sub {	# 11
		if ( eval { $uid = $imap->append("$target",&testmsg)} ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 12
		if ( eval { $imap->select("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 13, 14
		unless ($imap->has_capability(UIDPLUS)) {
		      	print "ok ", ++$test," (skipped)\n";
                      	print "ok ", ++$test," (skipped)\n";
			return;
		}
		my $m1 ; 
		eval { $imap->Uid(1); $m1 = $imap->message_string($uid) ; $imap->Uid(0)} ;
		if ( !$? ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	
		if ( length($m1))	{
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
	};
	
	push @tests, sub {	# 15
		my @res;
		if ( eval { @res = $imap->fetch(1,"RFC822.TEXT") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 16
		my $h;
		if ( eval {  $h = $imap->parse_headers(1,"Subject")  
			and $h->{Subject}[0] =~ /^Testing from pid/} ) {
			print "ok ",++$test,"\n";
		} else {	
			# use Data::Dumper;
			# print Dumper($h);
			# print "$h->{Subject}[0] \n";
			print "not ok ",++$test,"\n";
			# print $imap->Results;
		}
	};

	my @hits = ();
	push @tests, sub {	# 17
		eval { @hits = $imap->search('SUBJECT','Testing') } ;
		if ( scalar(@hits) == 1 ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			print "Found ",scalar(@hits), 
			  " hits (",join(", ",@hits),")-- expected 1\n";
			# print $imap->Results;
			exit;
		}
	};
	push @tests, sub {	# 18, 19
		eval { $imap->Uid(1); @uidhits = $imap->search('SUBJECT','Testing') } ;
		if ( scalar(@uidhits) == 1 ) {
			print "ok ",++$test,"\n";
			if ($uidhits[0] == $uid) { 
				print "ok ",++$test,"\n" ;
			} else {
				print "not ok ",++$test,"\n";
			}
		} else {	
			print "not ok ",++$test,"\n";
			print "ok (skipped)",++$test,"\n";
		}
		eval { $imap->Uid(0) };
	};

	push @tests, sub {	# 20
		if ( $imap->delete_message(@hits) ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 21
		eval { @hits = $imap->search(qq(SUBJECT "Productioning")) } ;
		unless ( scalar(@hits)  ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			#print "Found ",scalar(@hits), 
			#  " hits (",join(", ",@hits),")-- expected 1\n";
			#print $imap->Report;
			#exit;
		}
	};

	push @tests, sub {	# 22,23
		$imap->select('inbox');
		if ( $imap->rename($target,"${target}NEW") ) {
			print "ok ",++$test,"\n";
			if ( $imap->delete("${target}NEW") ) {
				print "ok ",++$test,"\n";
			} else {	
				print "not ok ",++$test,"\n";
			}
		} else {	
			print "not ok ",++$test,"\n";
			if ( $imap->delete("$target") ) {
				print "ok ",++$test,"\n";
			} else {	
				print "not ok ",++$test,"\n";
			}
		}
	};

	if ( -f "./.test" ) { 
		print "1..${\(scalar @tests + 6)}\n";  # update here if adding test to existing sub
	} else {		
		print "1..1\n"; 	
	}	

	$main::loaded = 1;
	print "ok 1\n";
	$| = 1; 

	unless ( -f "./.test" ) { exit;}

	open TST,"./.test" or exit;
	while (defined(my $l = <TST>)) {
		my($p,$v)=split(/=/,$l);
		chomp $v if $v;
		$parms{$p}=$v if $v;
	}
	close TST;

}
eval { $imap = Mail::IMAPClient->new( 
		Server 	=> "$parms{server}"||"localhost",
		Port 	=> "$parms{port}"  || '143',
		User 	=> "$parms{user}"  || scalar(getpwuid($<)),
		Password=> "$parms{passed}"|| scalar(getpwuid($<)),
		Clear   => 0,
		Debug   => 0,
		Fast_IO   => 1,
		Timeout	=> 4,
) } ;


for my $test (@tests) { $test->(); }
# print $imap->Report,"\n";

sub testmsg {
		my $m = qq{Date:  @{[$imap->Rfc822_date(time)]}
To: <$parms{user}\@$parms{server}>
From: Perl <$parms{user}\@$parms{server}>
Subject: Testing from pid $$

This is a test message generated by 'make test' during the installation of 
that nifty Mail::IMAPClient module from CPAN. Like all things perl, it's 
way cool.

};

	return $m;
}

# History:
# $Log: basic.t,v $
# Revision 19991216.7  2000/01/12 18:58:05  dkernen
# *** empty log message ***
#
# Revision 19991216.6  1999/12/28 13:57:22  dkernen
# tested with v1.08
#
# Revision 19991216.5  1999/12/16 17:19:17  dkernen
# Bring up to same level
#
# Revision 19991124.7  1999/12/16 17:14:27  dkernen
# Incorporate changes for exists method performance enhancement
#
# Revision 19991124.6  1999/12/01 22:11:06  dkernen
# Enhance support for UID and add tests to t/basic for same
#
# Revision 19991124.5  1999/11/30 20:41:55  dkernen
# Bring CVS repository up to latest level
#
# Revision 19991124.4  1999/11/24 19:58:45  dkernen
#
# Modified Files:
# basic.t  - to add $Id and $Log data in comments
#
