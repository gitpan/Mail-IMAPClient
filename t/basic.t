# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

END {print "not ok 1\n" unless $loaded;}
use Mail::IMAPClient;


######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my $test = 1;
my %parms;
my $imap;
my @tests;

BEGIN { 
	my $target; my $sep;

	
	push @tests, sub {
		if (ref($imap)) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
		if ($sep = $imap->separator) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
		my $isparent;
		$isparent = $imap->is_parent(INBOX);
		if (defined($isparent)) {
			$target = "INBOX${sep}IMAPClient_$$";
			print "ok ",++$test,"\n";
		} else {	
			$target = "IMAPClient_$$";
			print "ok ",++$test,"\n";
		}
		print "target is $target\n";
	};

	
	push @tests, sub {
		if ( eval { $imap->select('inbox') } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
			# print $imap->History,"\n";
		}
	};

	push @tests, sub {
		if ( eval { $imap->create("$target") } ) {
			print "ok ",++$test,"\n";
		} else {
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
		if (defined($imap->is_parent($target))) {
			if ( eval { $imap->create(qq($target${sep}has "quotes")) } ) {
				print "ok ",++$test,"\n";
			} else {
                          if ($imap->LastError =~ /NO Invalid.*name/) {
                                print "skipping ",++$test,
				 " $parms{server} doesn't support quotes in folder names--",
				 "skipping next 2 tests\n";
                                print "skipping ",++$test,"\n";
                                print "skipping ",++$test,"\n";
                                return;
                          } else {
                                print "not ok ",++$test,"\n";
                                print "skipping ",++$test,"\n";
                                print "skipping ",++$test,"\n";
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

	push @tests, sub {
		if ( eval { $imap->exists("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};


	push @tests, sub {
		if ( eval { $imap->append("$target",&testmsg)} ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
		if ( eval { $imap->select("$target") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
		my @res;
		if ( eval { @res = $imap->fetch(1,"RFC822.TEXT") } ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
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
	push @tests, sub {
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

	push @tests, sub {
		if ( $imap->delete_message(@hits) ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	push @tests, sub {
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

	push @tests, sub {
		$imap->select('inbox');
		if ( $imap->delete("$target") ) {
			print "ok ",++$test,"\n";
		} else {	
			print "not ok ",++$test,"\n";
		}
	};

	if ( -f "./.test" ) { 
		print "1..${\(scalar @tests + 3)}\n"; 
	} else {		
		print "1..1\n"; 	
	}	

	$loaded = 1;
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
		Debug   => 1,
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
