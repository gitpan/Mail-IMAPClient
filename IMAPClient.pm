package Mail::IMAPClient;

$VERSION = '.09';
$VERSION = '.09';  	# do it twice to make sure it takes

use Socket;
use IO::Socket;

=head1 NAME 

Mail::IMAPClient - An IMAP Client API

=cut


sub Unconnected 	{ return 0 ; }		# Object not connected
sub Connected 		{ return 1 ; } 		# connected; not logged in
sub Authenticated 	{ return 2 ; }		# logged in; no mailbox selected
sub Selected 		{ return 3 ; }		# mailbox selected



# the following for loop sets up eponymous accessor methods for 
# the object's parameters:

for my $datum (
		qw( 	State Port Server Folder
			User Password Socket 
			Debug LastError Count	)
) {
	no strict 'refs';
        *$datum = sub {
                if ($_[1]) {
                        return $_[0]->{$datum} = $_[1] ;
                } else {
                        return $_[0]->{$datum};
                }
        };
}

# The following defines a special method to deal with the Clear parameter:

sub Clear { 
	my $self = shift;
	defined(my $clear = shift) or return $self->{Clear}; 
	
	if ($clear) {
		$self->{"History"}  = {};
	}
	$self->Count(0) ;

	my $oldclear = $self->{Clear};
	$self->{Clear} = $clear;
	
	return $oldclear;
}

my $not_void_context = '0 but true'; 		# return true value

# read-only access to the transaction number:
sub Transaction { shift->Count };

# the constructor:
sub new {
	my $class 	= shift;
	my $self  	= {};

	bless $self, ref($class)||$class;


	$self->State(Unconnected);
	if (@_) {
		my %conf = @_;
		while (my($k,$v)=each %conf) { 
			$k = ucfirst lc($k) ; 
			$self->$k($v);
		}
	}	
	$self->Clear(5) unless $self->Clear;
	$self->connect if $self->Server;
	return $self;
}


sub connect {
	my $self = shift;
	
	my $sock = IO::Socket::INET->new(
		PeerAddr => $self->Server		,
                PeerPort => $self->Port||'imap(143)'	,
                Proto    => 'tcp' 			,
		Debug	=> $self->Debug 		,
	)						
	or return undef					;

	$sock->autoflush(1)				;
	
	unless ( defined($sock) ) {
		
		$self->LastError( "Unable to connect to host: $!\n");	
		return undef;
	}
	$self->Socket($sock);
	$self->State(Connected);
	if ($self->User and $self->Password) {
		return $self->login ;
	} else {
		return $self;	
	}
}
	

sub login {
	my $self = shift;
	my $string = "Login " . $self->User . " " . $self->Password ;
	$self->_imap_command($string) 
		and $self->State(Authenticated);
	return ( ($self->State eq Authenticated) ? $self : undef);
}


sub list {
	my $self = shift;
	my ($reference, $target) = (shift, shift);
	$reference 	||= "";	
	$target 	||= '*';	
	my $string 	=  qq(LIST "$reference" "$target");
	$self->_imap_command($string)  or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;
}


sub select {
	my $self = shift;
	my $target = shift or return undef;
	my $string 	=  qq/SELECT $target/;

	my $old = $self->Folder;

	if ($self->_imap_command($string) and $self->State(Selected)) {
		$self->Folder($target);
		return $old||$self;
	} else { 
		return undef;
	}
}



sub examine {
	my $self = shift;
	my $target = shift or return undef;
	my $string 	=  qq/EXAMINE $target/;

	my $old = $self->Folder;

	if ($self->_imap_command($string) and $self->State(Selected)) {
		$self->Folder($target);
		return $old||$self;
	} else { 
		return undef;
	}
}

# _{name} methods are undocumented and meant to be private.

# _imap_command runs a command, inserting the correct tag
# and <CR><LF> and whatnot.
#
sub _imap_command {
	my $self 	= shift;
	my $string 	= shift 	or return undef;
	my $good 	= shift 	|| 'GOOD';

	$good = quotemeta($good);

	my $clear = $self->Clear;

	$self->Clear($clear) 
		if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);

	$string 	= "$count $string";

	$self->_record($count,"$string\r\n");

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError( "Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	
	until ( ($code) = $output =~ /^$count (NO|BAD|$good|OK)/) {
		$output = $self->_read_line;	
		$self->_record($count,$output);
		if ($output =~ /^\*\s+BYE/) {
			$self->State(Unconnected);
			return undef ;
		}
	}	
	
	return $code =~ /^OK|$good/ ? $self : undef ;

}
# _record saves the conversation into the History structure:
sub _record {

	my ($self,$count,$line) = ( shift, shift, shift);

	push @{$self->{"History"}{$count}}, $line;

	$self->LastError("$line") if /^\S+\s+(BAD|NO)/;

	return $self;
}

#_send_line writes to the socket:
sub _send_line {
	my($self,$string) = (shift, shift);

	unless ($string =~ /\r\n$/) {

		chomp $string;
		$string .= "\r" unless $string =~ /\r$/;	
		$string .= "\n" ;
	}

	print "Sending: $string\n" if $self->Debug;
	return	syswrite($self->Socket, $string, length($string) ) ;
}

#_read_line reads from the socket:
sub _read_line {
	
	my $self 	= shift;	
	my $sh		= $self->Socket;
	
	my $buffer	; 
	my $count	= 0;

	sysread($sh,$buffer,1,$count++) until $buffer =~ /\r\n$/;
	print "Read: $buffer\n" if $self->Debug;
	return $buffer;
}


sub Report {
	my $self = shift;
	return 	map 	{ @{$self->{"History"}{$_}} } 
			sort { $a <=> $b } keys %{$self->{"History"}}
	;
}


sub Results {
	my $self 	= shift	;
	my $transaction = shift||$self->Count;
	
	return wantarray 				? 
		@{$self->{"History"}{$transaction}} 	: 
		$self->{"History"}{$transaction}		;
}


sub History {
	my @a = @{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}};
	shift @a;
	return wantarray ? @a : \@a ;
}


sub logout {
	my $self = shift;
	my $string = "LOGOUT";
	$self->_imap_command($string) ; 
	$self->State(Unconnected);
	$self->Socket->close ; $self->Socket(undef);
	return $self;
}


sub folders {
	my $self = shift;
	return wantarray ? @{$self->{Folders}} : $self->{Folders} if ref($self->{Folders});

	my @folders = map { 
		my @split = split(/\s+/,$_); 
		my $pop = pop @split;
		if ($pop =~ /"$/) {
			$pop = pop(@split) . " $pop" until $pop =~ /^"/;
		}		
		$pop;
	} @{$self->list};
	pop @folders and shift @folders;
	my @fixed;

	$self->{Folders} = \@folders;
	
	return wantarray ? @folders : \@folders ;
}


sub exists {
	my ($self,$what) = (shift,shift);
	map { return 1 if $_ eq $what } @{$self->folders} ;
	return undef;
}
	
	
sub fetch {

	my $self = shift;
	$self->_imap_command( "FETCH " . (join(" ",@_)||'ALL')) 	 or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;

}
	
sub AUTOLOAD {

	my $self = shift;
	return undef if $AUTOLOAD =~ /DESTROY$/;
	delete $self->{Folders}  if lc($AUTOLOAD) =~ /::create$/i;
	delete $self->{Folders}  if lc($AUTOLOAD) =~ /::rename$/i;
	delete $self->{Folders}  if lc($AUTOLOAD) =~ /::delete$/i;
	my $cmd = $AUTOLOAD =~ s/.*:://;
	if (scalar(@_)) {
		$self->_imap_command(qq/$AUTOLOAD / . join(" ",@_) )  or return undef;
	} else {
		$self->_imap_command(qq/$AUTOLOAD/) or return undef;
	}
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;

}
	


sub status {

	my $self = shift;
	my $box = shift or return undef;
	my @pieces = @_;
	$self->_imap_command("STATUS $box (". (join(" ",@_)||'MESSAGES'). ")") or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;

}


sub message_count {
	
	my ($self, $folder) = (shift, shift);
	
	$self->status($folder, 'MESSAGES') or return undef;

	chomp(my $m =	   (	grep {s/\*\s+STATUS\s+.*\(MESSAGES\s+(\d+)\).*$/$1/ }
			$self->History($self->Transaction) 
	)[0]);

	return $m;

	
}

sub Strip_cr {
	my $self = shift;

	my $in = $_[0]||$self ;

	$in =~ s/\r//g  ;

	return $in;
}


sub disconnect { $_[0]->logout }

sub DESTROY {
	my $self = shift;
	eval {
		$self->logout if $self->State > Connected; 
		$self->Socket->close if ref($self->Socket);
	}
}


sub search {

	my $self = shift;
	$self->_imap_command( "SEARCH ". join(' ',@_)) 
		 or return undef;
	my @results =  $self->History($self->Count) 	;

	my @hits;

	for my $r (@results) {
			
		push @hits, split(/\s+/,$r)  if $r =~ s/^\*\s+SEARCH\s+//;
	}
	
	return wantarray ? @hits : \@hits;
		
}




sub delete_message {

	my $self = shift;
	my $count = 0;
	my @msgs;
	for my $arg (@_) {
		if (ref($arg) eq 'ARRAY') {
			push @msgs , @{$arg};
		} else {
			push @msgs , split(/,/,$arg);
		}
	}
	

	$self->store(join(',',@msgs),'+FLAGS.SILENT (\Deleted)') and $count++;

	return $count;
}

sub capability {

	my $self = shift;

	$self->_imap_command('CAPABILITY') or return undef;

	my @caps = map { split } grep (s/^\*\s+CAPABILITY\s+//, $self->History($self->Count));

	for (@caps) { $self->{CAPABILITY}{uc($_)}++}

	return wantarray ? @caps : \@caps;
}

sub has_capability {
	my $self = shift;
	$self->capability;
	return $self->{CAPABILITY}{uc($_[0])};
}


sub append {

        my $self = shift;
        my $folder = shift;
	my $text = join("\n",@_);
	$text =~ s/\n/\r\n/g;
        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);


        my $string = "$count APPEND $folder {" . length($text) . "}\r\n" ;

        $self->_record($count,"$string\r\n");

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	
	until ( ($code) = $output =~ /go ahead/) {
		$output = $self->_read_line;	
		$self->_record($count,$output);
		if ($output =~ /^\*\s+BYE/) {
			$self->State(Unconnected);
			return undef ;
		}
	}	
	
        return undef if $code =~ /^BAD|NO/ ;

        $feedback = $self->_send_line("$text\r\n");

        unless ($feedback) {
                $self->LastError("Error sending append msg text to IMAP: $!\n");
                return undef;
        }

        until (($code) = $output =~ /^$count (OK|NO|BAD)/) {
                $output = $self->_read_line;
                $self->_record($count,$output);
                if ($output =~ /^\*\s+BYE/) {
                        $self->State(Unconnected);
                        return undef ;
                }
        }

        return $code =~ /^OK/ ? $self : undef ;

}


sub move {

	my($self, $target, @msgs) = @_;

	$self->create($target) and $self->subscribe($target) 
		unless $self->exists($target);

	#for my $msg (@msgs) { 
	#	$self->copy("$msg","$target") or return undef;
	#}

	$self->copy(join(',',@msgs),"$target") or return undef;

	$self->delete_message(@msgs);
}


sub size {

	my ($self,$msg) = @_;

	my($size) = grep(/RFC822\.SIZE/,$self->fetch($msg,"(RFC822.SIZE)"));

	$size =~ /RFC822\.SIZE\s+(\d+)/;
	
	return $1;
}


# Status Routines:

sub Status 		{ return $_[0]->State 				;	}
sub IsUnconnected 	{ return ($_[0]->State == Unconnected)	? 1 : 0 ; 	}
sub IsConnected 	{ return ($_[0]->State >= Connected) 	? 1 : 0 ; 	}
sub IsAuthenticated 	{ return ($_[0]->State >= Authenticated)? 1 : 0 ; 	}
sub IsSelected 		{ return ($_[0]->State == Selected) 	? 1 : 0 ; 	}		

=head1 EXAMPLE

    use Mail::IMAPClient;
    my $imap = Mail::IMAPClient->new( Server => 'imaphost', 
			  User   => 'memememe',
			  Password => 'secret',
    );

    $imap->Debug($opt_d);

    my @folders = $imap->folders;

    foreach my $f (@folders) { 

	print 	"$f is a folder with ",
		$imap->message_count($f),
		" messages.\n";
   }
    

=cut

=head1 DESCRIPTION 

This module provides methods implementing the IMAP protocol. It allows perl scripts 
to interact with IMAP message stores.

The module is used by constructing or instantiating a new IMAPClient object via the
B<new> constructor method. Once the object has been instantiated, the B<connect> method is 
either implicitly or explicitly called. At that point methods are available that implement the 
IMAP client commands as specified in I<RFC2060>. When processing is complete, the I<logoff> object 
method is called, either explicitly by the program or implicitly when the object goes out of scope 
(or at program termination). 

This documentation is not meant to be a replacement for RFC2060, and the wily programmer will
have a copy of that document handy when coding IMAP clients. 

Note that this documentation uses the term I<folder> in place of RFC2060's use of I<mailbox>. 
This documentation reserves the use the term I<mailbox> to refer to the set of folders owned by a 
specific IMAP id.

RFC2060 defines four possible states for an IMAP connection: not authenticated, authenticated,
selected, and logged out. These correspond to the B<IMAPClient> constants B<Connected>, 
B<Authenticated>, B<Selected>, and B<Unconnected>, respectively. These constants are 
implemented as class methods, and can be used in conjunction with the B<Status> method to 
determine the status of an B<IMAPClient> object and its underlying IMAP session. Note that an 
B<IMAPClient> object can be in the B<Unconnected> state both before a server connection is made
and after it has ended. This differs slightly from RFC2060, which does not define a 
pre-connection status. For a discussion of the methods available for examining the B<IMAPClient>
object's status, see the section labeled "Status Methods", below.

=head2 Transactions

RFC2060 requires that each line in an IMAP conversation be prefixed with a tag. A typical
conversation consists of the client issuing a tag-prefixed command string, and the server
replying with one of more lines of output. Those lines of output will include a 
command completion status code prefixed by the same tag as the original command string.

The B<IMAPClient> module uses a simple counter to ensure that each client command is issued with 
a unique tag value. This tag value is referred to by the B<IMAPClient> module as the transaction 
number. A history is maintained by the B<IMAPClient> object documenting each transaction.
The B<Transaction> method returns the number of the last transaction, and can be used to
retrieve lines of text from the object's history. 

The B<Clear> parameter is used to control the size of the session history so that long-running
sessions do not eat up unreasonable amounts of memory. See the discussion of B<Clear> under the
I<Parameters> section for more information.

The B<Report> transaction returns the history of the entire IMAP session since the 
initial connection or the last time the  B<Clear> object method was invoked. 
This provides a record of the entire conversation, including
client command strings and server responses, and is a wonderful debugging tool as well as
a useful source of raw data for custom parsing.


=head2 Parameters

There are several parameters that influence the behavior of an B<IMAPClient> object. Each is 
set by specifying a named value pair during new method invocation as follows:

	my $imap = Mail::IMAPClient->new ( parameter  => "value",
			       parameter2 => "value",
				...
		);

Parameters can also be set after an object has been instantiated by using the parameter's
eponymous accessor method like this:

	my $imap = Mail::IMAPClient->new;
	   $imap->parameter( "value");
	   $imap->parameter2("value");

The eponymous accessor methods can also be used without arguments to obtain the current value 
of the parameter as follows:

	my $imap = Mail::IMAPClient->new;
           $imap->parameter( "value");
           $imap->parameter2("value");

		... 	# A whole bunch of awesome perl code, 
			# omitted for brevity
	   

	   my $forgot  = $imap->parameter;
	   my $forgot2 = $imap->parameter2;

Note that in these examples I'm using 'parameter' and 'parameter2' as generic parameter names. 
The B<IMAPClient> object doesn't actually have parameters named 'parameter' and 'parameter2'. 
On the contrary, the available parameters are:

=over 4

=item Debug

Sets the debugging flag to either a true or false value. Can be supplied with the B<new>
method call or separately by calling the B<Debug> object method.

=item Port

Specifies the port on which the IMAP server is listening. The default is 143, which is the 
standard IMAP port. Can be supplied with the B<new> method call or separately by calling 
the B<Port> object method.

=item Server

Specifies the hostname or IP address of the host running the IMAP server. If provided as part
of the B<new> method call, then the new IMAP object will automatically be connected at the 
time of instantiation. (See the B<new> method, below.) Can be supplied with the B<new> 
method call or separately by calling the B<Server> object method.

=cut

=item User

Specifies the userid to use when logging into the IMAP service. Can be supplied with the 
B<new> method call or separately by calling the B<User> object method.

=cut

=item Password

Specifies the password to use when logging into the IMAP service on the host specified in the
I<Server> parameter as the user specified in the I<User> parameter. Can be supplied with 
the B<new> method call or separately by calling the B<Password> object method.

If I<Server>, I<User>, and I<Password> are all provided to the B<new> method, then the newly
instantiated object will be connected to the host specified in I<Server> (at either the port
specified in I<Port> or the default port 143) and then logged on as the user specified in 
the I<User> parameter (using the password provided in the I<Password> parameter). See the 
discussion of the B<new> method, below.

=item Clear

Specifies that the object's history buffer should be cleared and the transaction count reset 
to 0 after every I<n> transactions, where I<n> is the value specified for the I<Clear> 
parameter.  Resetting I<Clear> to a true value during a session (even if it is reset to the 
current value) by calling the eponymous B<Clear> method with an argument will clear the history 
buffer of its contents immediately. Calling the eponymous B<Clear> method without an argument will 
return the current value of the I<Clear> parameter but will not immediately clear the history 
buffer. 

Setting I<Clear> to 0 turns off automatic history buffer clearing, and setting it to 1 
turns off the history buffer facility (except for the last transaction, which cannot be 
disabled without breaking the B<IMAPClient> module). Setting I<Clear> to 0 will not cause an
immediate clearing of the history buffer; setting it to 1 (or any other number) will. 

The default I<Clear> value is set to five in order to conserve memory.

=cut

=back

Parameters can be set during B<new> method invocation by passing named parameter/value pairs
to the method, or later by calling the parameter's eponymous object method.

=cut

=head2 Object Methods

There are basically two types of object methods--those that participate in the IMAP session's
conversation (i.e. they issue IMAP client commands) and those that do not. Methods that do
not result in new IMAP client commands being issued (such as the B<Transaction, Status>, and
B<History> methods) all begin with an uppercase letter, to distinguish themselves from methods
that do correspond to IMAP client commands. (Class methods and eponymous parameter methods 
likewise begin with an uppercase letter because they also do not correspond to an IMAP client
command.) 

The B<IMAPClient> object methods are:

=over 4

=item append

The B<append> method adds a message to the specified folder. It takes two arguments, the
name of the folder to append the message to, and the text of the message (including headers).
Additional arguments are added to the message text, separated with a newline.

The B<append> method returns a true value if successful or undef if not.

=cut

=item capability

The B<capability> method returns an array of capabilities as returned by the CAPABILITY IMAP 
Client command, or a reference to an array of capabilities if called in scalar context. 
If the CAPABILITY IMAP Client command fails for any reason then the B<capability> method will
return undef.

=item connect

The B<connect> method connects an imap object to the server. It returns C<undef> if it
fails to connect for any reason. If values are available for the I<User> and I<Password>
parameters at the time that B<connect> is invoked, then B<connect> will call the B<login>
method after connecting and return the result of the B<login> method to B<connect>'s caller.
If either or both of the I<User> and I<Password> parameters are unavailable but the connection
to the server succeeds then B<connect> returns a pointer to the B<IMAPClient> object.

The I<Server> parameter must be set (either during B<new> method invocation or 
via the B<Server> object method) before invoking B<connect>. If the I<Server> parameter is
supplied to the B<new> method then B<connect> is implicitly called during object construction.

The B<connect> method sets the state of the object to I<connected> if it successfully connects 
to the server.

=cut

=item delete_message

The B<delete_message> method accepts a list of arguments. Each item in the list should be either: 

=item a message sequence number,

=item a comma-separated list of message sequence numbers, or

=item a reference to an array of message sequence numbers.

The messages identified by the sequence numbers will be deleted. B<delete_message> returns the number
of messages it was told to delete. However, since the delete is done by issuing the 
I<+FLAGS.SILENT> option of the STORE IMAP client command, there is no guarantee that the 
delete was successful. In this manner the B<delete_message> method sacrifices accuracy for speed. 
If you must have guaranteed results then use the IMAP STORE client command (via the default
method) and use the +FLAGS (\Deleted) option, and then parse your results manually. Eg:


	
	$imap->store($msg_id,'+FLAGS (\Deleted)');
	my @results = $imap->History($imap->Transaction);
	...			# code to parse output goes here



The B<IMAPClient> object must be in I<Selected> status to use the B<delete_message> method. 

B<NOTE:> All the messages identified in the input argument(s) must be in the currently 
selected folder. Failure to comply to this requirement will almost certainly result in the
wrong message(s) being deleted. This would be a crying shame.

B<See also:> The B<delete> method, to delete a folder.

=cut

=item disconnect

Disconnects the B<IMAPClient> object from the server. Functionally equivalent to the B<logout> 
method.

=cut

=item examine

The B<examine> method selects a folder in read-only mode and changes the object's state to 
"Selected".  The folder selected via the B<examine> method can be examined but no changes can
be made unless it is first selected via the B<select> method. 

The B<examine> method accepts one argument, which is the name of the folder to select. 

=cut

=item exists

Accepts one argument, a folder name. Returns true if the folder exists or false if it does
not exist.

=cut

=item fetch

The B<fetch> method implements the FETCH IMAP client command. It accepts a list of
arguments, which will be converted into a space-delimited list of arguments to the
FETCH IMAP client command. If no arguments are supplied then B<fetch> does a FETCH ALL.
(It would really be a good idea at this point to review RFC2060.)

=cut

=item folders

The B<folders> method returns an array listing the available folders. It will only be 
successful if the object is in the B<Authenticated> or B<Selected> states.

=cut

=item has_capability

Returns true if the IMAP server to which the B<IMAPClient> object is connected has the capability
specified as an argument to B<has_capability>.

=item list

The B<list> method implements the IMAP LIST client command. Arguments are passed to the 
IMAP server as received, separated from each other by spaces. If no arguments are supplied
then the default list command C<tag LIST "" '*'> is issued.

The B<list> method returns an array (or an array reference, if called in a scalar context).
The array is the unaltered output of the LIST command.

=cut

=item login

The B<login> method uses the IMAP LOGIN client command (as defined in RFC2060) to log into
the server.  The I<User> and I<Password> parameters must be set before the B<login> method
can be invoked. If successful, the B<login> method returns a pointer to the B<IMAPClient> object 
and sets the object status to I<Authenticated>. If unsuccessful, it returns undef.

=cut

=item logout

The B<logout> method issues the LOGOUT IMAP client commmand. Since the LOGOUT IMAP client 
command causes the server to end the connection, this also results in the B<IMAPClient> client 
entering the B<Unconnected> state. This method does not, however, destroy the B<IMAPClient> object,
so a program can re-invoke the B<connect> and B<login> methods if it wishes to reestablish
a session later in the program.

=cut

=item message_count

The message_count method accepts the name of a folder as an argument and returns the number
of messages in that folder. Internally, it invokes the B<status> method (see above) and 
parses out the results to obtain the number of messages. 

=cut

=item move

The B<move> method moves messages from the currently selected folder to the folder specified
in the first argument to B<move>.  The rest of the arguments should be either:

=over 8

=item a message sequence number,

=item a comma-separated list of message sequence numbers, or

=item a reference to an array of message sequence numbers.

=back

If the target folder does not exist then it will be created.

=cut

=item new

The B<new> method creates a new instance of an B<IMAPClient> object. If the I<Server> parameter
is passed as an argument to B<new>, then B<new> will implicitly call the B<connect> method. If
the I<Server> parameter is not supplied then the B<IMAPClient> object is created in the 
I<Unconnected> state.

=cut

=item search

The B<search> method implements the SEARCH IMAP client command. Any argument supplied to
B<search> is prefixed with a space and appended  to the SEARCH IMAP client command. This
method is another one of those situations where it will really help to have your copy of
RFC2060 handy, since the SEARCH IMAP client command contains a plethora of options and 
possible arguments. 

Remember that if your argument needs quotes around it then you must make sure that the
quotes will be preserved when passing the argument. I.e. use C<qq/"$arg"/> instead of 
C<"$arg">.

The B<search> method returns an array containing sequence numbers of messages that passed the
SEARCH IMAP client command's search criteria. 

=cut

=item select

The B<select> method selects a folder and changes the object's state to "Selected".
It accepts one argument, which is the name of the folder to select.

=cut

=item size

The B<size> method accepts one input argument, a sequence number. It returns the size of the message
in the currently selected folder with the supplied sequence number.  The B<IMAPClient> object must be 
in a I<Selected> state in order to use this method.

=cut

=item status

The B<status> method accepts one argument, the name of a folder (or mailbox, to use
RFC2060's terminology), and returns an array containing the results of running the  IMAP 
STATUS client command against that folder. If additional arguments are supplied then they
are appended to the IMAP STATUS client command string, separated from the rest of the string 
and each other with spaces.

If B<status> is not called in an array context then it returns a reference to an array 
rather than the array itself.

The B<status> method should not be confused with the B<Status> method (with an uppercase 'S'),
which returns information about the B<IMAPClient> object. (See the section labeled 
B<Status Methods>, below).

=cut

=item Other IMAP Client Commands

IMAP Client Commands not otherwise documented have been implemented via an AUTOLOAD hack and use 
a default method.

If a program calls a method that is not defined (or inherited) by the B<IMAPClient> module then
the B<IMAPClient> module will assume that it is an IMAP client command. It will prefix the command
with the next available transaction number (or tag value), and append to it the 
space-delimited list of arguments supplied to the unimplemented method. It will then read lines
of output from the imap session until it finds a line containing  the strings "OK" and 
"Completed", and return an array containing all of the lines of output (or, if called in scalar
context, an array reference).

Eg:		

	$imap->FOO("bar","an example","of the default");


results in:


	"99 FOO bar an example of the default\r\n" 

being sent to the IMAP server (assuming that 99 is the current transaction number).

B<CAUTION:> Once again, remember to quote your quotes if you want quotes to be part of the 
IMAP command string.     

=cut

=item Status Methods

There are several methods that return the status of the object. They can be
used at any time to check the status of an B<IMAPClient> object, but are particularly useful 
for determining the cause of failure when a connection and login are attempted as part of
a single B<new> method invocationl. The status methods are:

=over 8

=item 1. Status 

returns a numerical value that indicates the current status of the B<IMAPClient> object. (Not to
be confused with the B<status> method, all lower-case, which is the implementation of the
STATUS IMAP client command.)

=item 2. IsUnconnected

returns a true value if the object is currently in an B<Unconnected> state.

=item 3. IsConnected

returns a true value if the object is currently in either a B<Connected, Authenticated>, or
B<Selected> state.

=item 4. IsAuthenticated

returns a true value if the object is currently in either an B<Authenticated> or 
B<Selected> state.

=item 5. IsSelected

returns a true value if the object is currently in a B<Selected> state.

=item 6. Transaction 

returns the tag value (or transaction number) of the last IMAP client command.

=item 7. Report

The B<Report> method returns an array containing a history of the IMAP session up to the
point that B<Report> was called. It is primarily meant to assist in debugging but can also
be used to retrieve raw output for manual parsing. The value of the B<Clear> parameter controls
how often the contents of B<Report> are cleared. (See the discussion of B<Clear> in the 
I<Parameters> section, above.)

=cut

=item 8. Results

The B<Results> method returns an array containing the results of one IMAP client command. 
It accepts one argument, the transaction number of the command whose results are to be 
returned. If transaction number is unspecified then B<Results> returns the results of the 
last IMAP client command issued. If called in a scalar context, B<Results> returns an 
array reference rather than an array.

=cut

=item 9. History

The B<History> method is almost identical to the B<Results> method. Unlike the B<Results>
method, however, the IMAP command that was issued to create the results being returned is
not included in the returned results.  If called in a scalar context, B<History> returns an 
array reference rather than an array.

=cut

=back

=back

=cut

=head2 Class Methods

The following class methods can also be called as object methods:

=over 4

=item Unconnected

returns a value equal to the numerical value associated with an object in the B<Unconnected> 
state.


=item Connected

returns a value equal to the numerical value associated with an object in the B<Connected> 
state.


=item Authenticated

returns a value equal to the numerical value associated with an object in the B<Authenticated> 
state.

=item Selected

returns a value equal to the numerical value associated with an object in the B<Selected> 
state.

=item Strip_cr

The <Strip_cr> method strips carriage returns from IMAP client command output. Although 
RFC2060 specifies that lines in an IMAP conversation end with <CR><LF>, it is often cumbersome
to have the carriage returns in the returned data. This method accepts a line of text as 
and argument, and returns that line with all carriage returns removed. If the input argument
had no carriage returns then it is returned unchanged. 

B<Strip_cr> does not remove new line characters.

=cut

=back


=head1 AUTHOR

	David J. Kernen
	The Kernen Consulting Group, Inc
	david.kernen@erols.com

=cut

=head1 COPYRIGHT

                       Copyright 1999, The Kernen Group, Inc.
                            All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of either:

=over 4

=item

a) the "Artistic License" which comes with this Kit, or

=item

b) the GNU General Public License as published by the Free Software Foundation; either versio
n 1, or (at your option) any later version.

=back

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either
the GNU General Public License or the Artistic License for more details.

=cut

