# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# $Id: basic.t,v 19991216.12 2000/06/23 19:08:40 dkernen Exp $
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .

END {print "not ok 1\n" unless $main::loaded;}
use Mail::IMAPClient;

######################### End of black magic.


my $test = 0;
my %parms;
my $imap;
my @tests;
my $uid;

BEGIN { 
	$^W++;
	my $target; my $sep; my $target2;

	push @tests, sub { $test++ } ; # 
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
			$target2 = "INBOX${sep}IMAPClient_2_$$";
			print "ok ",++$test,"\n";
		} else {	
			$target = "IMAPClient_$$";
			$target2 = "IMAPClient_2_$$";
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
	push @tests, 
		sub { return "dummy test 8" },
		sub { return "dummy test 9" };

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

	push @tests, 	sub {	# 11
		if ( eval { $imap->create($target2) } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	}, 		sub {	# 12
		if ( eval { $imap->exists($target2) } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};
			
	push @tests, sub {	# 13
		if ( eval { $uid = $imap->append("$target",&testmsg)} ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 14
		if ( eval { $imap->select("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 15
		if ( eval { my $uid2 = $imap->copy($target2,1)} ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	
	push @tests, sub {	# 16
		my @res;
		if ( eval { @res = $imap->fetch(1,"RFC822.TEXT") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 17
		my $h;
		if ( eval {  $h = $imap->parse_headers(1,"Subject")  
			and $h->{Subject}[0] =~ /^Testing from pid/o } ) {
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
	push @tests, sub {	# 18
		$imap->select("$target");
		eval { @hits = $imap->search('SUBJECT','Testing') } ;
		if ( scalar(@hits) == 1 ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			print "Found ",scalar(@hits), 
			  " hits (",join(", ",@hits),")-- expected 2\n";
		}
	};

	push @tests, sub {	# 19
		if ( $imap->delete_message(@hits) ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 20
		$imap->select($target2);
		if ( $imap->delete_message($imap->search("ALL")) and $imap->delete($target2) ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			print $imap->Report;
		}
	};

	push @tests, sub {	# 21
		eval { @hits = $imap->search(qq(SUBJECT "Productioning")) } ;
		unless ( scalar(@hits)  ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
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
	}, sub { return "dummy test 21" };

	if ( -f "./test.txt" ) { 
		print "1..${\(scalar @tests)}\n";  # update here if adding test to existing sub
	} else {		
		print "1..1\n"; 	
	}	

	$main::loaded = 1;
	print "ok 1\n";
	$| = 1; 

	unless ( -f "./test.txt" ) { exit;}

	open TST,"./test.txt" or exit;
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
		Fast_IO => 0,
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
# Revision 19991216.12  2000/06/23 19:08:40  dkernen
#
# Modified Files:
# 	Changes IMAPClient.pm Makefile test.txt  -- for v1.16
# Modified Files: basic.t  -- to remove uidplus tests and to make copy test copy to different folder
# Added Files: 	uidplus.t -- moved all uidplus tests here
#
# Revision 19991216.11  2000/06/21 21:07:44  dkernen
#
# Modified Files: Changes IMAPClient.pm Makefile
# Modified Files: basic.t
#
# Revision 19991216.10  2000/04/27 18:00:15  dkernen
# Modified Files: basic.t
#
# Revision 19991216.9  2000/03/10 16:04:39  dkernen
#
# Renamed .test file to test.txt to support weird platforms that don't support filenames
# beginning with a dot.
#
# Modified Files: Changes INSTALL MANIFEST Makefile Makefile.PL
#
# Added Files: test.txt test_template.txt
#
# Removed Files: .test .test_template Makefile.old
#
# Revision 19991216.8  2000/03/02 19:59:15  dkernen
#
# Modified Files: build_ldif.pl -- to support new option to all "To:" and "Cc:" to be included in ldif file
# Modified Files: basic.t -- to work better with UW IMAP server
#
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
