# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# $Id: basic.t,v 19991216.17 2000/11/10 22:08:15 dkernen Exp $
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
my $fast||=0;
my $uidplus||=0;
use vars qw/*TMP/;

BEGIN {
	$^W++;
	my $target; my $sep; my $target2;

	push @tests, sub { $test++ } ; # Dummy test 1
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
		if (defined($imap->is_parent($target))) {	#7 
			if ( eval { $imap->create(qq($target${sep}has "quotes")) } ) {
				print "ok ",++$test,"\n";	
			} else {
                          if ($imap->LastError =~ /NO Invalid.*name/) {
                                print "ok ",++$test,
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
			if ( eval { $imap->select(qq($target${sep}has "quotes")) } ) { #8
				print "ok ",++$test,"\n";
			} else {
				print "not ok ",++$test,"\n";
			}
			$imap->select('inbox');
			if ( eval { $imap->delete(qq($target${sep}has "quotes")) } ) { #9
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
			print STDERR $imap->Results;
		}
	};

	push @tests, sub {	# 14
		if ( eval { $imap->select("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};
	{
	my $size; my $string; my $target; 
	my $file = "./test_message_to_file";
	push @tests, sub {	# 15, 16, 17, 18, 19
		$target = ref($uid) ? ($imap->search("ALL"))[0] : $uid;
		if ( eval { $size = $imap->size($target) } ) { # 15  test size
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		} 
 	}, sub {
		if ( eval { $string = $imap->message_string($target) } ) { # 16  test message_string
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	}, sub {
		if ( $size == length($string) ) {	# 17 test size = length of string
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};
	push @tests, sub {
		eval { $imap->message_to_file($file,$target)};
		if ( $@ ) {					# 18 test message_to_file success
			print "not ok ",++$test,"\n";	
		} else {
			print "ok ",++$test,"\n";	
		}
	};
	push @tests, sub {
		if ( -s $file == $size ) {			# 19 test message_to_file size
			print "ok ",++$test,"\n";	#
		} else {
			print "not ok ",++$test,"\n";	#
		}
		unlink "$file" or warn "$! unlinking $file\n";
	};
	}						# wrap up closure

	push @tests, sub {	# 20, 21, 22, 23, 24, 25 26
		my @unseen; my @seen;
		if ( eval { @seen = $imap->seen } ) { # 20	test seen's success
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
		if ( @seen == 1 ) 			{ # 21	test seen's results
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
		
		if ( eval { $imap->deny_seeing(\@seen) } ) { # 22 test deny_seeing's success
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
		if ( eval { @unseen = $imap->unseen } ) { # 23 test unseen's success
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}

		if ( @unseen == 1 ) 		    { # 24 test deny_seeing's and unseen's results
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
		if ( eval { $imap->see(\@seen) } ) { # 25 test see's success
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
		if ( @seen == 1 ) 			{ # 26 test seen's and see's success
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
		eval { $imap->deny_seeing(@seen)  };
		my $subject;
		eval { $imap->Peek(1) };
		eval { $subject = $imap->parse_headers($seen[0],"Subject")->{Subject}[0] };
		if ( join("",$imap->flags($seen[0])) =~ /\\Seen/i ) { 	# 27 test "Peek = 1"
			print "not ok ",++$test,"\n";	
		} 	else {
			print "ok ",++$test,"\n";	
		}
		eval { $imap->deny_seeing(@seen)  };
		eval { $imap->Peek(0) };
		eval { $subject = $imap->parse_headers($seen[0],"Subject")->{Subject}[0] };
		if ( join("",$imap->flags($seen[0])) =~ /\\Seen/i ) { 	# 28 test "Peek = 0"
			print "ok ",++$test,"\n";	
		}	else {
			print "not ok ",++$test,"\n";	
		}
		eval { $imap->deny_seeing(@seen)  };
		eval { $imap->Peek(undef) };
		eval { $subject = $imap->parse_headers($seen[0],"Subject")->{Subject}[0] };
		if ( join("",$imap->flags($seen[0])) =~ /\\Seen/i ) { 	# 29 test "Peek = undef"
			print "not ok ",++$test,"\n";	
		}	else {
			print "ok ",++$test,"\n";	
		}
		
		
	};
	# Add dummy tests to come up to 29 test routines:
	push @tests, 	sub { 21 }, sub { 22 }, sub { 23 } , sub { 24 }, sub { 25 }, sub { 26 }, sub {27},
			sub {28}, sub {29};

	push @tests, sub {	# 30
		if ( eval { my $uid2 = $imap->copy($target2,1)} ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	
	push @tests, sub {	# 31
		my @res;
		if ( eval { @res = $imap->fetch(1,"RFC822.TEXT") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 32
		my $h;
		if ( eval {  $h = $imap->parse_headers(1,"Subject")  
			and $h->{Subject}[0] =~ /^Testing from pid/o } ) {
			print "ok ",++$test,"\n";
		} else {	
			 use Data::Dumper;
			print Dumper($h);
			print "$h->{Subject}[0] \n";
			print "not ok ",++$test,"\n";
		 	print $imap->Results;
		}
	};

	my @hits = ();
	push @tests, sub {	# 33
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

	push @tests, sub {	# 34, 35
		if ( $imap->delete_message(@hits) ) {
			print "ok ",++$test,"\n";
			my $flaghash = $imap->flags(\@hits);
			my $flagflag = 0;
			foreach my $v ( values %$flaghash ) { 
				foreach my $f (@$v) { $flagflag++ if $f =~ /\\Deleted/}
			}
			if ( $flagflag == scalar(@hits) ) {
				print "ok ", ++$test,"\n";
			} else {
				print "not ok ", ++$test,"\n";
			}
		} else {	
			print "not ok ",++$test,"\n";
			print "not ok ",++$test,"\n";
		}
	}, sub { return "Dummy test 35"} ;

	push @tests, sub {	# 36, 37
		if ( $imap->restore_message(@hits) ) {
			print "ok ",++$test,"\n";
			my $flaghash = $imap->flags(\@hits);
			my $flagflag = scalar(@hits);
			foreach my $v ( values %$flaghash ) { 
				foreach my $f (@$v) { $flagflag-- if $f =~ /\\Deleted/}
			}
			if ( $flagflag == scalar(@hits) ) {
				print "ok ", ++$test,"\n";
			} else {
				print "not ok ", ++$test,"\n";
			}
		} else {	
			print "not ok ",++$test,"\n";
			print "not ok ",++$test,"\n";
		}
	}, sub { $imap->delete_message(@hits) } ;	# dummy 37
	push @tests, sub {	# 38
		$imap->select($target2);
		if ( $imap->delete_message($imap->search("ALL")) and $imap->delete($target2) ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			print $imap->Report;
		}
	};

	push @tests, sub {	# 39
		eval { @hits = $imap->search(qq(SUBJECT "Productioning")) } ;
		unless ( scalar(@hits)  ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {	# 40, 41
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
	}, sub { "Dummy 41" } ; 	

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

=begin debugging

$db = IO::File->new(">/tmp/de.bug");
local *TMP = $db ;
open(STDERR,">&TMP");
select(((select($db),$|=1))[0]);

=end debugging

=cut

eval { $imap = Mail::IMAPClient->new( 
		Server 	=> "$parms{server}"||"localhost",
		Port 	=> "$parms{port}"  || '143',
		User 	=> "$parms{user}"  || scalar(getpwuid($<)),
		Password=> "$parms{passed}"|| scalar(getpwuid($<)),
		Clear   => 0,
		Timeout => 30,
		Debug   => 0,
		Debug_fh   => undef,	#	$db,
		Fast_IO => $fast,
		Uid 	=> $uidplus,
) 	or 
	print STDERR "\nCannot log into $parms{server} as $parms{user}. Are server/user/password correct?\n" 
	and exit
} ;


for my $test (@tests) { $test->(); }
#print $db $imap->Report,"\n";

sub testmsg {
		my $m = qq{Date:  @{[$imap->Rfc822_date(time)]}
To: <$parms{user}\@$parms{server}>
From: Perl <$parms{user}\@$parms{server}>
Subject: Testing from pid $$

This is a test message generated by $0 during a 'make test' as part of the installation of
that nifty Mail::IMAPClient module from CPAN. Like all things perl, it's 
way cool.

};

	return $m;
}

# History:
# $Log: basic.t,v $
# Revision 19991216.17  2000/11/10 22:08:15  dkernen
#
# Modified Files: Changes IMAPClient.pm Makefile t/basic.t -- to add Peek parm and to make several bug fixes
#
# Revision 19991216.16  2000/10/30 21:04:11  dkernen
#
# Modified Files: Changes IMAPClient.pm  -- to update documentation
# Modified Files: basic.t -- added tests for message_to_string.
#
# Revision 19991216.15  2000/10/30 18:40:50  dkernen
#
# Modified Files: Changes IMAPClient.pm INSTALL MANIFEST Makefile README test.txt  -- for 2.0.1
# Added Files:
# 	rfc1731.txt rfc1732.txt rfc1733.txt rfc2061.txt rfc2062.txt
# 	rfc2086.txt rfc2087.txt rfc2088.txt rfc2177.txt rfc2180.txt
# 	rfc2192.txt rfc2193.txt rfc2195.txt rfc2221.txt rfc2222.txt
# 	rfc2234.txt rfc2245.txt rfc2342.txt rfc2359.txt rfc2683.txt
#
# Revision 19991216.14  2000/10/27 14:43:59  dkernen
#
# Modified Files: Changes IMAPClient.pm Todo -- major rewrite of I/O et al.
# Modified Files: basic.t fast_io.t uidplus.t -- more tests in basic.t. Other
# tests just "do basic.t" with different options set.
#
# Revision 19991216.13  2000/07/10 20:54:19  dkernen
#
# Modified Files: Changes IMAPClient.pm MANIFEST Makefile README
# Modified Files: find_dup_msgs.pl
# : Modified Files: basic.t fast_io.t
#
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
