# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'
# $Id: basic.t,v 19991216.20 2001/02/07 20:20:43 dkernen Exp $
######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .

use Mail::IMAPClient::Parse;

BEGIN {
	print "1..1\n";
        $main::loaded = 1;
        $| = 1;
        print "ok 1\n";
}
END {print "not ok 1\n" unless $main::loaded;}


# History:
# $Log$
