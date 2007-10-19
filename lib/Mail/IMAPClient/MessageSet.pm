
package Mail::IMAPClient::MessageSet;

use warnings;   # possible?
use strict;

=head1 NAME

Mail::IMAPClient::MessageSet -- ranges of message sequence nummers

=cut

sub str { 
	# print "Overloaded ", overload::StrVal(${$_[0]}),"\n";
	return overload::StrVal(${$_[0]}); 
}
sub rem {
	my $self = shift;
	my $minus = ref($self)->new(@_);
	my %deleted = map { $_ => 1 } @{$minus->unfold} ;
	${$self} = $self->range(
		map { exists $deleted{$_} ? () : $_ } @{$self->unfold}
	);
	return $self;	
}
sub cat {
	my $self = shift;
	my @a = ("$self",@_);
	${$self} = $self->range(@a);
	return $self;	
}
use overload 	qq/""/ => "str" ,
		qq/.=/=>"cat", 
		qq/+=/=>"cat", 
		qq/-=/=>"rem", 
		q/@{}/=>"unfold", 
		fallback => "TRUE";

sub new {
	my $class = shift;
	my $range = $class->range(@_);
	my $object = \$range;
	bless $object, $class;
	return $object ;	
}

sub range {
	my $class = shift;	
	if ( 	scalar(@_) == 1 and 
		ref($_[0]) =~ /Mail::IMAPClient::MessageSet/
	) {
		return $_[0] ;
	}

	my @msgs = ();
	for my $m (@_) {
		next if !defined($m) or $m eq "";
		if ( ref($m) ) {
		   foreach my $mm (@$m) {
			foreach my $c ( split(/,/,$mm) ) {
			 	if ( $c =~ /:/ ) {
					my($l,$h) = split(/:/,$c) ;
					push @msgs,$l .. $h ;
				} else {
					push @msgs,$c;
				}
			}
		   }
		} else {
			#print STDERR "m=$m\n";
			foreach my $c ( split(/,/,$m) ) {
			 	if ( $c =~ /:/ ) {
					my($l,$h) = split(/:/,$c) ;
					push @msgs,$l .. $h ;
				} else {
					push @msgs,$c;
				}
			}
		}
	} 
	return undef unless @msgs;
	my @range = ();
	my $high = my $low = "";
	for my $m (sort {$a<=>$b} @msgs) {
		$low = $m if $low eq "";
		next if $high ne "" and $high == $m ; # been here, done this
		if ( $high eq "" ) { 
			$high = $m ;
		} elsif ( $m == $high + 1 ) {
			$high = $m ;
		} else {
			push @range, $low == $high ? "$low," : "$low:$high," ;
			$low = $m ;
			$high = $m ;
		}
	}
	push @range, $low == $high ? "$low" : "$low:$high" ;
	my $range = join("",@range);
	return $range;
}

sub unfold {
	my $self = $_[0];
	return wantarray ? 
		(	map { my($l,$h)= split(/:/,$_) ; $h?($l..$h):$l }
			split(/,/,$$self) 	
		) : 
		[	map { my($l,$h)= split(/:/,$_) ; $h?($l..$h):$l }
			split(/,/,$$self) 	
		]
	;
}

=head2 DESCRIPTION

The B<Mail::IMAPClient::MessageSet> module is designed to make life easier
for programmers who need to manipulate potentially large sets of IMAP
message UID's or sequence numbers.

This module presents an object-oriented interface into handling your message
sets. The object reference returned by the L<new> method is an overloaded 
reference to a scalar variable that contains the message set's compact
RFC2060 representation. The object is overloaded so that using it like a string
returns this compact message set representation. You can also add messages to
the set (using either a '.=' operator or a '+=' operator) or remove messages
(with the '-=' operator). And if you use it as an array reference, it will 
humor you and act like one by calling L<unfold> for you. (But you need perl 5.6
or above to do this.)

RFC2060 specifies that multiple messages can be provided to certain IMAP
commands by separating them with commas. For example, "1,2,3,4,5" would 
specify messages 1, 2, 3, 4, and (you guessed it!) 5. However, if you are
performing an operation on lots of messages, this string can get quite long.
So long that it may slow down your transaction, and perhaps even cause the
server to reject it. So RFC2060 also permits you to specifiy a range of
messages, so that messages 1, 2, 3, 4 and 5 can also be specified as
"1:5". 

This is where B<Mail::IMAPClient::MessageSet> comes in. It will convert your
message set into the shortest correct syntax. This could potentially save you 
tons of network I/O, as in the case where you want to fetch the flags for
all messages in a 10000 message folder, where the messages are all numbered
sequentially. Delimited as commas, and making the best-case assumption that 
the first message is message "1", it would take 48893 bytes to specify the 
whole message set using the comma-delimited method. To specify it as a range, 
it takes just seven bytes (1:10000). 

=head2 SYNOPSIS

To illustrate, let's take the trivial example of a search that returns these
message uids: 1,3,4,5,6,9,10, as follows:
	
	@msgs = $imap->search("SUBJECT","Virus"); # returns 1,3,4,5,6,9,10
	my $msgset = Mail::IMAPClient::MessageSet->new(@msgs);
	print "$msgset\n";  # prints "1,3:6,9:10\n"
	# add message 14 to the set:
	$msgset += 14;	
	print "$msgset\n";  # prints "1,3:6,9:10,14\n"
	# add messages 16,17,18,19, and 20 to the set:
	$msgset .= "16,17,18:20";	
	print "$msgset\n";  # prints "1,3:6,9:10,14,16:20\n"
	# Hey, I didn't really want message 17 in there; let's take it out:
	$msgset -= 17;
	print "$msgset\n";  # prints "1,3:6,9:10,14,16,18:20\n"
	# Now let's iterate over each message:
	for my $msg (@$msgset) {
		print "$msg\n";
	}       # Prints: "1\n3\n4\n5\n6\n9\n10\n14\n16\n18\n19\n20"

(Note that the L<Mail::IMAPClient> B<Range> method can be used as 
a short-cut to specifying C<Mail::IMAPClient::MessageSet-E<gt>new(@etc)>.) 

=cut

=head1 CLASS METHODS

The only class method you need to worry about is B<new>. And if you create
your B<Mail::IMAPClient::MessageSet> objects via L<Mail::IMAPClient>'s 
B<Range> method then you don't even need to worry about B<new>.

=head2 new

Example:

 my $msgset = Mail::IMAPClient::MessageSet->new(@msgs);

The B<new> method requires at least one argument. That argument can be 
either a message, a comma-separated list of messages, a colon-separated 
range of messages, or a combination of comma-separated messages and 
colon-separated ranges. It can also be a reference to an array of messages,
comma-separated message lists, and colon separated ranges.

If more then one argument is supplied to B<new>, then those arguments should
be more message numbers, lists, and ranges (or references to arrays of them)
just as in the first argument.

The message numbers passed to B<new> can really be any kind of number at
all but to be useful in a L<Mail::IMAPClient> session they should be either
message UID's (if your I<Uid> parameter is true) or message sequence numbers.

The B<new> method will return a reference to a B<Mail::IMAPClient::MessageSet>
object. That object, when double quoted, will act just like a string whose
value is the message set expressed in the shortest possible way, with the
message numbers sorted in ascending order and with duplicates removed. 

=head1 OBJECT METHODS

The only object method currently available to a B<Mail::IMAPClient::MessageSet>
object is the L<unfold> method.

=head2 unfold

Example:

	my $msgset = $imap->Range( $imap->messages ) ;
	my @all_messages = $msgset->unfold;

The B<unfold> method returns an array of messages that belong to the 
message set. If called in a scalar context it returns a reference to the 
array instead.

=head1 OVERRIDDEN OPERATIONS

B<Mail::IMAPClient::MessageSet> overrides a number of operators in order
to make manipulating your message sets easier. The overridden operations are:

=head2 stringify

Attempts to stringify a B<Mail::IMAPClient::MessageSet> object will result in
the compact message specification being returned, which is almost certainly
what you will want.

=head2 Auto-increment

Attempts to autoincrement a B<Mail::IMAPClient::MessageSet> object will 
result in a message (or messages) being added to the object's message set. 

Example:

	$msgset += 34;
	# Message #34 is now in the message set 

=head2 Concatenate

Attempts to concatenate to a B<Mail::IMAPClient::MessageSet> object will 
result in a message (or messages) being added to the object's message set. 

Example:

	$msgset .= "34,35,36,40:45";
	# Messages 34,35,36,40,41,42,43,44,and 45 are now in the message set 

The C<.=> operator and the C<+=> operator can be used interchangeably, but
as you can see by looking at the examples there are times when use of one
has an aesthetic advantage over use of the other.

=head2 Autodecrement

Attempts to autodecrement a B<Mail::IMAPClient::MessageSet> object will 
result in a message being removed from the object's message set. 

Examples:

	$msgset -= 34;
	# Message #34 is no longer in the message set 
	$msgset -= "1:10";
	# Messages 1 through 10 are no longer in the message set 

If you attempt to remove a message that was not in the original message set
then your resulting message set will be the same as the original, only more
expensive. However, if you attempt to remove several messages from the message
set and some of those messages were in the message set and some were not,
the additional overhead of checking for the messages that were not there
is negligable. In either case you get back the message set you want regardless
of whether it was already like that or not.

=head1 AUTHOR

 David J. Kernen
 The Kernen Consulting Group, Inc

=head1 COPYRIGHT

 Copyright 1999, 2000, 2001, 2002 The Kernen Group, Inc.
 All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the terms of either:

=over 4

=item a) the "Artistic License" which comes with this Kit, or

=item b) the GNU General Public License as published by the Free Software 
Foundation; either version 1, or (at your option) any later version.

=back

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See either the GNU
General Public License or the Artistic License for more details. All your
base are belong to us.

=cut

1;
