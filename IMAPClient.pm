package Mail::IMAPClient;

# $Id: IMAPClient.pm,v 20001010.7 2000/12/20 19:36:52 dkernen Exp $

$Mail::IMAPClient::VERSION = '2.0.5';
$Mail::IMAPClient::VERSION = '2.0.5';  	# do it twice to make sure it takes

use Fcntl qw(:DEFAULT);
use Socket;
use IO::Socket;
use IO::Select;
use IO::File;
use Carp;
use Data::Dumper;


#print "Found Fcntl in $INC{'Fcntl.pm'}\n";
#Fcntl->import;

=head1 NAME 

Mail::IMAPClient - An IMAP Client API

=cut

sub Unconnected   ()	{ return 0 ; }		# Object not connected
sub Connected 	  ()	{ return 1 ; } 		# connected; not logged in
sub Authenticated ()	{ return 2 ; }		# logged in; no mailbox selected
sub Selected 	  ()	{ return 3 ; }		# mailbox selected
sub _INDEX	  ()    { return 0 ; }		# Array index for output line number
sub _TYPE	  ()    { return 1 ; }		# Array index for output line type (OUTPUT,INPUT, or LITERAL)
sub _DATA	  ()    { return 2 ; }		# Array index for output line data

sub _debug {
	my $self = shift;
	return unless $self->Debug;
	my $fh = $self->{Debug_fh} || \*STDERR; 
	print $fh @_;
}

# the following for loop sets up eponymous accessor methods for 
# the object's parameters:

{
 for my $datum (
		qw( 	State Port Server Folder Fast_io Peek
			User Password Socket Timeout Buffer
			Debug LastError Count Uid Debug_fh
		)
 ) {
	no strict 'refs';
        *$datum = sub {
		if ( defined($_[1]) and $datum eq 'Fast_io' and ref($_[0]->{Socket})) {
			if ($_[1]) {			# Passed the "True" flag
				my $fcntl = 0;
				eval { $fcntl=fcntl($_[0]->{Socket}, F_GETFL, 0) } ;
				if ($@) {
					$_[0]->{Fast_io} = 0;
					warn ref($_[0]) . " not using Fast_IO; not available on this platform.\n"
						if ( $^W or $_[0]->Debug);
				} else {
					$_[0]->{Fast_io} = 1;
					$_[0]->{_fcntl} = $fcntl;
					my $newflags = $fcntl;
					$newflags |= O_NONBLOCK;
					fcntl($_[0]->{Socket}, F_SETFL, $newflags) ;
					
				}
			} else {
					fcntl($_[0]->{Socket}, F_SETFL, $_[0]->{_fcntl}) ;
					$_[0]->{Fast_io} = 0;
					delete $_[0]->{_fcntl};
			}
		} elsif ( defined($_[1]) and $datum eq 'Socket' ) {

			# Get rid of fcntl settings for obsolete socket handles:
			delete $_[0]->{_fcntl} ;
			# Register this handle in a select vector:
			$_[0]->{_select} = IO::Select->new($_[1]) ;
		}

                if (scalar(@_) > 1) {
			$@ = $_[1] if $datum eq 'LastError';
                        return $_[0]->{$datum} = $_[1] ;
                } else {
                        return $_[0]->{$datum};
                }
        };
 }
}

sub Wrap { 	shift->Clear(@_); 	}

# The following class method is for creating valid dates in appended msgs:

sub Rfc822_date {
my $class=      shift;
#Date: Fri, 09 Jul 1999 13:10:55 -0000#
my $date =      $class =~ /^\d+$/ ? $class : shift ;
my @date =      gmtime($date);
my @dow  =      qw{ Sun Mon Tue Wed Thu Fri Sat };
my @mnt  =      qw{ Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec};
#
return          sprintf(
                        "%s, %2.2d %s %4.4s %2.2d:%2.2d:%2.2d -%4.4d",
                        $dow[$date[6]],
                        $date[3],
                        $mnt[$date[4]],
                        $date[5]+=1900,
                        $date[2],
                        $date[1],
                        $date[0],
                        $date[8]) ;
}

# The following class method is for creating valid dates for use in IMAP search strings:

sub Rfc2060_date {
my $class=      shift;
# 11-Jan-2000
my $date =      $class =~ /^\d+$/ ? $class : shift ;
my @date =      gmtime($date);
my @mnt  =      qw{ Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec};
#
return          sprintf(
                        "%2.2d-%s-%4.4s",
                        $date[3],
                        $mnt[$date[4]],
                        $date[5]+=1900
		) ;
}

# The following class method strips out <CR>'s so lines end with <LF> instead of <CR><LF>:

sub Strip_cr {
	my $class = shift;
	unless ( ref($_[0]) or scalar(@_) > 1 ) {
		(my $string = $_[0]) =~ s/\r\n/\n/gm;
		return $string;
	}
	return wantarray ?     	map { s/\r\n/\n/gm ; $_ }  (ref($_[0]) ? @{$_[0]}  : @_)  : 
				[ map { s/\r\n/\n/gm ; $_ } ref($_[0]) ? @{$_[0]} : @_ ] ;
}

# The following defines a special method to deal with the Clear parameter:

sub Clear {
	my $self = shift;
	defined(my $clear = shift) or return $self->{Clear}; 
	
	my $oldclear   = $self->{Clear};
	$self->{Clear} = $clear;

	my (@keys) = sort { $b <=> $a } keys %{$self->{"History"}}  ;

	for ( my $i = $clear; $i < @keys ; $i++ ) { delete $self->{'History'}{$keys[$i]} }


	return $oldclear;
}

# read-only access to the transaction number:
sub Transaction { shift->Count };

# the constructor:
sub new {
	my $class 	= shift;
	my $self  	= 	{
		LastError	=> "", 
		Uid 		=> 1, 
		Count 		=> 0,
		Fast_io 	=> 1,
		"Clear"		=> 5, 
	};
	while (scalar(@_)) {
		$self->{ucfirst(lc($_[0]))} = $_[1]; shift, shift;
	}

	bless $self, ref($class)||$class;

	$self->State(Unconnected);

	$self->{Debug_fh} ||= \*STDERR;
 	$self->_debug("Using Mail::IMAPClient version $Mail::IMAPClient::VERSION " .
		"and perl version " . ( defined $^V ? join(".",unpack("CCC",$^V)) : "") . 
		" ($])\n") if $self->Debug;
	$self->LastError(0);
	return $self->connect if $self->Server and !$self->Socket;
	return $self;
}


sub connect {
	my $self = shift;
	
	$self->Port(143) 
		if 	defined ($IO::Socket::INET::VERSION) 
		and 	$IO::Socket::INET::VERSION eq '1.25' 
		and 	!$self->Port;
	%$self = (%$self, @_);
	my $sock = IO::Socket::INET->new(
		PeerAddr => $self->Server		,
                PeerPort => $self->Port||'imap(143)'	,
                Proto    => 'tcp' 			,
                Timeout  => $self->Timeout||0		,
		Debug	=> $self->Debug 		,
	)						;

	unless ( defined($sock) ) {
		
		$self->LastError( "Unable to connect to $self->{Server}: $!\n");	
		$@ 		= "Unable to connect to $self->{Server}: $!";	
		carp 		  "Unable to connect to $self->{Server}: $!" unless defined wantarray;	
		return undef;
	}
	$self->Socket($sock);
	$self->State(Connected);

	$sock->autoflush(1)				;
	
	my ($code, $output);
        $output = "";

        until ( $code ) {

                $output = $self->_read_line or return undef;
                for my $o (@$output) {
			$self->_debug("Connect: Received this from readline: " . join("/",@$o) . "\n");
                        $self->_record($self->Count,$o);	# $o is a ref
			next unless $o->[_TYPE] eq "OUTPUT";
        		($code) = $o->[_DATA] =~ /^\*\s+(OK|BAD|NO)/i  ;
                }

        }

	if ($code =~ /BYE|NO /) {
		$self->State(Unconnected);
		return undef ;
	}

	if ($self->User and $self->Password) {
		return $self->login ;
	} else {
		return $self;	
	}
}
	

sub login {
	my $self = shift;
	my $id   = $self->User;
	my $has_quotes = $id =~ /^".*"$/ ? 1 : 0;
	my $string = 	"Login " . ( $has_quotes ? $id : qq("$id") ) . " " . 
			$self->Massage($self->Password) ;
	$self->_imap_command($string) 
		and $self->State(Authenticated);
	# $self->folders and $self->separator unless $self->NoAutoList;
	unless ( $self->IsAuthenticated) {
		my($carp) 	=  $self->LastError;
		$carp 		=~ s/^[\S]+ ([^\r\n]*)\r?\n/$1/;
 		carp $carp unless defined wantarray;
		return undef;
	}
	return $self;
}

sub separator {
	my $self = shift;
	my $target = shift ; 

	unless ( defined($target) ) {
		my $sep ;
		# 	separator is namespace's 1st thing's 1st thing's 2nd thing:
		eval { 	$sep = $self->namespace->[0][0][1] } 	;
		return $sep if $sep;
	}	
		
	defined($target) or $target = "INBOX";
	$target ||= '""' ;
	
	

	# The fact that the response might end with {123} doesn't really matter here:

	unless (exists $self->{"$target${;}SEPARATOR"}) {
		my $list = (grep(/^\*\s+LIST\s+/, $self->list(undef,$target) ))[0] || qq("/");
		my $s = (split(/\s+/,$list))[3];
		$self->{"$target${;}SEPARATOR"} = ( $s eq 'NIL' ? 'NIL' : substr($s, 1,length($s)-2) );
	}

	return $self->{$target,'SEPARATOR'};
}

sub list {
	my $self = shift;
	my ($reference, $target) = (shift, shift);
	$reference = "" unless defined($reference);
	$target = '*' unless defined($target);
	$target 	  = $self->Massage($target);
	my $string 	=  qq(LIST "$reference" $target);
	$self->_imap_command($string)  or return undef;
	return wantarray ? 	$self->History($self->Count) 		: 
				[ map { $_->[_DATA] } @{$self->{'History'}{$self->Count}} ]	;
}

sub lsub {
	my $self = shift;
	my ($reference, $target) = (shift, shift);
	$reference = "" unless defined($reference);
	$target = '*' unless defined($target);
	$target           = $self->Massage($target);
	my $string      =  qq(LSUB "$reference" $target);
	$self->_imap_command($string)  or return undef;
	return wantarray ?      $self->History($self->Count)            : 
				[ map { $_->[_DATA] } @{$self->{'History'}{$self->Count}}	] ;
}

sub subscribed {
        my $self = shift;
	my $what = shift ;

        my @folders ;  

	my @list = $self->lsub(undef,( $what? "$what" . $self->separator($what) . "*" : undef ) );
	push @list, $self->lsub(undef, $what) if $what and $self->exists($what) ;

	# my @list = map { $self->_debug("Pushing $_->[${\(_DATA)}] \n"); $_->[_DATA] } @$output;

	my $m;

	for ($m = 0; $m < scalar(@list); $m++ ) {
		if ($list[$m] && $list[$m]  !~ /\r\n$/ ) {
			$list[$m] .= $list[$m+1] ;
			$list[$m+1] = "";	
		}
			
		
		# $self->_debug("Subscribed: examining $list[$m]\n");

		push @folders, $1||$2 
			if $list[$m] =~
                        /       ^\*\s+LSUB              # * LSUB
                                \s+\([^\)]*\)\s+            # (Flags)
                                "[^"]*"\s+              # "delimiter"
                                (?:"([^"]*)"|(.*))\r\n$  # Name or "Folder name"
                        /ix;

        } 

        # for my $f (@folders) { $f =~ s/^\\FOLDER LITERAL:://;}

        return wantarray ? @folders : \@folders ;
}


sub deleteacl {
	my $self = shift;
	my ($target, $user ) = @_;
	$target 	  = $self->Massage($target);
	$user		  =~ s/^"(.*)"$/$1/;
	$user 	  	  =~ s/"/\\"/g;
	my $string 	=  qq(DELETEACL $target "$user");
	$self->_imap_command($string)  or return undef;

	return wantarray ? 	$self->History($self->Count) 				: 
				[ map {$_->[_DATA] } @{$self->{'History'}{$self->Count}}] ;
}

sub setacl {
        my $self = shift;
        my ($target, $user, $acl) = @_;
        $user = $self->User unless length($user);
        $target = $self->Folder unless length($target);
        $target           = $self->Massage($target);
        $user             =~ s/^"(.*)"$/$1/;
        $user             =~ s/"/\\"/g;
        $acl              =~ s/^"(.*)"$/$1/;
        $acl              =~ s/"/\\"/g;
        my $string      =  qq(SETACL $target "$user" "$acl");
        $self->_imap_command($string)  or return undef;
        return wantarray?$self->History($self->Count):[map{$_->[_DATA]}@{$self->{'History'}{$self->Count}}];
}

sub getacl {
        my $self = shift;
        my ($target) = @_;
        $target = $self->Folder unless defined($target);
        $target           = $self->Massage($target);
        my $string      =  qq(GETACL $target);
        $self->_imap_command($string)  or return undef;
        my $perm = (grep(/^\* ACL/,     @{$self->History($self->Count)}))[0]
                        or return undef;
        $perm = (split(/\s+/,$perm))[-1];
        return $perm;
}

sub listrights {
	my $self = shift;
	my ($target, $user) = @_;
	$user = $self->User unless defined($user);
	$target = $self->Folder unless defined($target);
	$target 	  = $self->Massage($target);
	$user		  =~ s/^"(.*)"$/$1/;
	$user 	  	  =~ s/"/\\"/g;
	my $string 	=  qq(GETACL $target "$user");
	$self->_imap_command($string)  or return undef;
	my $rights = ( grep(s/.*$target"?\s+"?$user"?\s+//, $self->History($self->Count) ) );
	$rights =~ s/\s+//g;	
	return wantarray ? split(//,$rights) : $rights ;
}

sub select {
	my $self = shift;
	my $target = shift ;  
	return undef unless defined($target);

	my $qqtarget = $self->Massage($target);

	my $string 	=  qq/SELECT $qqtarget/;

	my $old = $self->Folder;

	if ($self->_imap_command($string) and $self->State(Selected)) {
		$self->Folder($target);
		return $old||$self;
	} else { 
		return undef;
	}
}

sub message_string {
	my $self = shift;
	my $msg  = shift;
	my $expected_size = $self->size($msg);
	
	my $cmd  = $self->Peek ? 'BODY.PEEK[]' : 'BODY[]' ;

	$self->fetch($msg,$cmd) or return undef;
	
	my $string = "";

	foreach my $result  (@{$self->{"History"}{$self->Transaction}}) { 
		$string .= $result->[_DATA] if $self->_is_literal($result) ;
	}      
	if ( length($string) > $expected_size ) { $string = substr($string,0,$expected_size) }
	if ( length($string) < $expected_size and $self->Debug ) {
		$self->LastError("${self}::message_string: expected $expected_size bytes but received " . 
			length($string)."\n");
		warn "${self}::message_string: expected $expected_size bytes but received " . length($string);
	}
	return $string||undef;
}

sub message_to_file {
	my $self = shift;
	my $fh   = shift;
	my @msgs = @_;
	my $handle;

	if ( ref($fh) ) {
		$handle = $fh;
	} else { 
		$handle = IO::File->new(">>$fh");
		unless ( defined($handle)) {
			$@ = "Unable to open $fh: $!";
			$self->LastError("Unable to open $fh: $!\n");
			carp $@ if $^W;
			return undef;
		}
	} 

        my $clear = "";
        $clear = $self->Clear;
	my $cmd = $self->Peek ? 'BODY.PEEK[]' : 'BODY[]';

	my $string = ( $self->Uid ? "UID " : "" ) . "FETCH " . join(",",@msgs) . " $cmd";

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

        my $trans       = $self->Count($self->Count+1);

        $string         = "$trans $string" ;

        $self->_record($trans,[ 0, "INPUT", "$string\r\n"] );

        my $feedback = $self->_send_line("$string");

        unless ($feedback) {
                $self->LastError( "Error sending '$string' to IMAP: $!\n");
                $@ = "Error sending '$string' to IMAP: $!";
                return undef;
        }

        my ($code, $output);
        $output = "";

        READ: until ( $code)  {
                $output = $self->_read_line($handle) or return undef; # avoid possible infinite loop
                for my $o (@$output) {
                        $self->_record($trans,$o);	# $o is a ref
                        # $self->_debug("Received from readline: ${\($o->[_DATA])}<<END OF RESULT>>\n");
                        next unless $self->_is_output($o);
                        ($code) = $o->[_DATA] =~ /^$trans (OK|BAD|NO)/mi ;
                        if ($o->[_DATA] =~ /^\*\s+BYE/im) {
                                $self->State(Unconnected);
                                return undef ;
                        }
                }
        }

        # $self->_debug("Command $string: returned $code\n");
	close $handle unless ref($fh);
        return $code =~ /^OK/im ? $self : undef ;

}

sub message_uid {
	my $self = shift;
	my $msg  = shift;
	my @uid = $self->fetch($msg,"UID");
	my $uid;
	while ( my $u = shift @uid and !$uid) {
		($uid) = $u =~ /\(UID\s+(\d+)\s*\)\r?$/;
	}
	return $uid;
}

sub old_body_string {
	my $self = shift;
	my $msg  = shift;
	my @torso = $self->fetch($msg,"RFC822.TEXT");
	$torso[0] =~ s/.*FETCH \(.*RFC822\.TEXT //i;
	pop @torso and pop @torso;
	return join("",@torso);
}


sub body_string {
	my $self = shift;
	my $msg  = shift;
	my $ref = $self->fetch($msg,"BODY" . ( $self->Peek ? ".PEEK" : "" ) . "[TEXT]");

        my $string = "";
    	foreach my $result  (@{$ref}) 	{ 
                $string .= $result->[_DATA] if $self->_is_literal($result) ;
        }
	return $string if $string;

        my $head = shift @$ref;
        $self->_debug("body_string: first shift = '$head'\n");

        until ( (! $head)  or $head =~ /(?:.*FETCH .*\(.*BODY\[TEXT\])|(?:^\d+ BAD )|(?:^\d NO )/i ) {
                $self->_debug("body_string: shifted '$head'\n");
                $head = shift(@$ref) ;
        }
	unless ( scalar(@$ref) ) {
			$self->LastError("Unable to parse server response from " . $self->LastIMAPCommand );
			return undef ;
	}
	my $popped ; $popped = pop @$ref until $popped =~ /\)\r\n$/;

        if      ($head =~ /BODY\[TEXT\]\s*$/i )     {       # Next line is a literal
                        $string .= shift @$ref while scalar(@$ref);
                        $self->_debug("String is now $string\n") if $self->Debug;
        }

        return $string||undef;
}


sub examine {
	my $self = shift;
	my $target = shift ; return undef unless defined($target);
	$target = $self->Massage($target);
	my $string 	=  qq/EXAMINE $target/;

	my $old = $self->Folder;

	if ($self->_imap_command($string) and $self->State(Selected)) {
		$self->Folder($target);
		return $old||$self;
	} else { 
		return undef;
	}
}

sub tag_and_run {
	my $self = shift;
	my $string = shift;
	my $good = shift;
	$self->_imap_command($string,$good);
	return @{$self->Results};
}
# _{name} methods are undocumented and meant to be private.

# _imap_command runs a command, inserting the correct tag
# and <CR><LF> and whatnot.
# When updating _imap_command, remember to examine the run method, too, since it is very similar.
#

sub _imap_command {
	
	my $self 	= shift;
	my $string 	= shift 	or return undef;
	my $good 	= shift 	|| 'GOOD';

	$good = quotemeta($good);

	my $clear = "";
	$clear = $self->Clear;

	$self->Clear($clear) 
		if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);

	$string 	= "$count $string" ;

	$self->_record($count,[ 0, "INPUT", "$string\r\n"] );

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError( "Error sending '$string' to IMAP: $!\n");
		$@ = "Error sending '$string' to IMAP: $!";
		carp "Error sending '$string' to IMAP: $!" if $^W;
		return undef;
	}

	my ($code, $output);	
	$output = "";

	READ: until ( $code)  {
              $output = $self->_read_line or return undef; # escape infinite loop if read_line never returns any data
		for my $o (@$output) { 
			$self->_record($count,$o);	# $o is a ref
			# $self->_debug("Received from readline: ${\($o->[_DATA])}<<END OF RESULT>>\n");
			next unless $self->_is_output($o);
			($code) = $o->[_DATA] =~ /^$count (OK|BAD|NO|$good)/mi ;
			if ($o->[_DATA] =~ /^\*\s+BYE/im) {
				$self->State(Unconnected);
				return undef ;
			}
		}
	}	
	
	# $self->_debug("Command $string: returned $code\n");
	return $code =~ /^OK|$good/im ? $self : undef ;

}

sub run {
	my $self 	= shift;
	my $string 	= shift 	or return undef;
	my $good 	= shift 	|| 'GOOD';
	my $count 	= $self->Count($self->Count+1);
	my($tag)	= $string =~ /^(\S+) /  ;

	unless ($tag) {
		$self->LastError("Invalid string passed to run method; no tag found.\n");
		return undef;
	}

	$good = quotemeta($good);

	my $clear = "";
	$clear = $self->Clear;

	$self->Clear($clear) 
		if $self->Count >= $clear and $clear > 0;

	$self->_record($count,[ $self->_next_index($count), "INPUT", "$string\r\n"] );

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError( "Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	$output = "";

	until ( ($code) = $output =~ /^$tag (OK|BAD|NO|$good)/m ) {

		$output = $self->_read_line or return undef;	
		for my $o (@$output) { 
			$self->_record($count,$o);	# $o is a ref
			next unless $self->_is_output($o);
			($code) = $o->[_DATA] =~ /^(?:$tag|\*) (OK|BAD|NO|$good)/m  ;
			if ($o->[_DATA] =~ /^\*\s+BYE/) {
				$self->State(Unconnected);
			}
		}
	}	
	$self->{'History'}{$tag} = $self->{"History"}{$count} unless $tag eq $count;
	return $code =~ /^OK|$good/ ? @{$self->Results} : undef ;

}

# _record saves the conversation into the History structure:
sub _record {

	my ($self,$count,$array) = ( shift, shift, shift);
	local($^W)= undef;

	#$self->_debug(sprintf("in _record: count is $count, values are %s/%s/%s and caller is " . 
	#	join(":",caller()) . "\n",@$array));
	
	if ( 	# 	$array->[_DATA] and 
		$array->[_DATA] =~ /^\d+ LOGIN/i ) { 

		$array->[_DATA] =~ s/LOGIN.*/LOGIN XXXXXXXX XXXXXXXX/i ;
	}

	push @{$self->{"History"}{$count}}, $array;

	if ( $array->[_DATA] =~ /^\d+\s+(BAD|NO)\s/im ) {
		$self->LastError("$array->[_DATA]") ;
		$@ = $array->[_DATA];
		carp "$array->[_DATA]" if $^W ;
	}
	return $self;
}

#_send_line writes to the socket:
sub _send_line {
	my($self,$string,$suppress) = (shift, shift, shift);

	unless ($self->IsConnected and $self->Socket) {
		$self->LastError("NO Not connected.\n");
		carp "Not connected" if $^W;
		return undef;
	}

	unless ($string =~ /\r\n$/ or $suppress ) {

		chomp $string;
		$string .= "\r" unless $string =~ /\r$/;	
		$string .= "\n" ;
	}

	if ($self->Debug) {
		my $dstring = $string;
		if ( $dstring =~ m[\d+\s+Login\s+]i) {
			$dstring =~ 	s	(\b(?:\Q$self->{Password}\E|\Q$self->{User}\E)\b)
						('X' x length($self->{Password}))eg;
		}
		_debug $self, "Sending: $dstring\n" if $self->Debug;
	}
	my $total = 0;
	$total +=	syswrite(	
				$self->Socket, 
				$string, 
				length($string)-$total, 
				$total
	) until $total >= length($string);
	return $total;
}

#_read_line reads from the socket:
sub _old_read_line {
	
	my $self 	= shift;	
	my $sh		= $self->Socket;
	
	my $buffer	= ""; 
	my $count	= ""; $count = 0;
	my $rvec 	= my $ready = my $errors = 0; 
	my $timeout	= $self->Timeout;

	my $readlen 	= 1;
	my $fcntl 	= '';
	my($flags,$in_literal) 	= ('0',0);

	if ( $self->Fast_io ) {
		eval { $fcntl=fcntl($sh, F_GETFL, $flags) } ;
		# _debug $self, STDERR 
		# "Setfl = ",F_SETFL," and GETFL = ",F_GETFL," and NONBLOCK = ",O_NONBLOCK,"\n";
		# _debug $self, STDERR "Fcntl flag is now $fcntl\n";
		if ($@) {
			$self->Fast_io(0);
			carp ref($self) . " not using Fast_IO; not available on this platform.\n" 
				if ( $^W or $self->Debug);
		} else {
 
			my $newflags = $fcntl;
			$newflags |= O_NONBLOCK;
			fcntl($sh, F_SETFL, $newflags) and $readlen = ($self->{Buffer}||4096);
		}
	}
	my $offset = 0;

	until ($buffer =~ /\r?\n$/ ) {
		# _debug $self,"Entering read engine.\n" if $self->Debug;
		if ($timeout) {
			vec($rvec, fileno($self->Socket), 1) = 1;
			CORE::select( $ready = $rvec, undef, $errors = $rvec, $timeout) ;
			unless ( vec ( $ready, fileno($self->Socket), 1 ) ) {
				$self->LastError("Tag " . $self->Transaction . 
					": Timeout waiting for data from server\n");	
				fcntl($sh, F_SETFL, $fcntl) 
					if $self->Fast_io and defined($fcntl);
				$self->_record($self->Transaction,
					[ 	$self->_next_index($self->Transaction),
						"ERROR",
						$self->Transaction . "* NO Timeout during read from server\r\n"
					]
				);
				$@ = "Timeout during read from server\r\n";
				return undef;
			}
		}
		# _debug($self,"count is $count and length of buffer is " . length($buffer) . "\n");
		local($^W) = undef;
		$count += sysread(
					$sh,
					$buffer,
					$readlen,
					$offset
		) ;
		$offset = $count ;
		pos $buffer = 1;
		LITERAL: while ( $buffer =~ /\{(\d+)\}\r\n/g and ! $in_literal ) {
			my $len = $1 ;
			$in_literal++;
			_debug $self, 	"Buffer:\n$buffer" . ('-' x 30) . "\n" if $self->Debug;
			$offset = $count - length($buffer) ;
			$count -= length("{" . "$len" . "}\r\n" ) ;

			# _debug($self, "Count = $count and offset = $offset\n") if $self->Debug;

			# If I used anything from the buffer for my literal then it needs to come
			# out of the buffer now:		(use substr to avoid regexp overhead)

			substr($buffer , index($buffer, "{" . $len . "}\r\n"), length("{}\n\r" . $len)) = "";
				

			if ($timeout) {
				vec($rvec, fileno($self->Socket), 1) = 1;
				unless ( CORE::select( $ready = $rvec, 
							undef, 
							$errors = $rvec, 
							$timeout) 
				) {
					$self->LastError("Tag " . $self->Transaction . 
						": Timeout waiting for literal data from server\n");	
					return undef;
				}	
			}
			until ( $offset >= $len ) {
				# _debug $self, "Reading literal data\n";
				$offset += sysread($sh,$buffer,$len-$offset, $count+$offset) ;
			}
			$count += $offset;
			pos $buffer = 1;
		}
		$offset = length($buffer);

		pos $buffer = 1;
	}
	fcntl($sh, F_SETFL, $fcntl) if $self->Fast_io and defined($fcntl);
	#	_debug $self, "Buffer is now $buffer\n";
	_debug $self, "Read: $buffer\n" if $self->Debug;
	return defined($buffer) ? $buffer : undef ;
}

# _read_line reads from the socket. It is called by:
# 	append	append_file	authenticate	connect		_imap_command
#
# It is also re-implemented in:
#	message_to_file
#
# syntax: $output = $self->_readline( ( $literal_callback|undef ) , ( $output_callback|undef ) ) ;
# 	  Both input argument are optional, but if supplied must either be a filehandle, coderef, or undef.
#
#	Returned argument is a reference to an array of arrays, ie: 
#	$output = [ 
#			[ $index, 'OUTPUT'|'LITERAL', $output_line ] ,
#			[ $index, 'OUTPUT'|'LITERAL', $output_line ] ,
#			... 	# etc,
#	];

sub _read_line {
	
	my $self 	= shift;	
	my $sh		= $self->Socket;
	my $literal_callback    = shift;
	my $output_callback = shift;
	
	unless ($self->IsConnected and $self->Socket) {
		$self->LastError("NO Not connected.\n");
		carp "Not connected" if $^W;
		return undef;
	}

	my $iBuffer	= ""; 
	my $oBuffer	= [];
	my $count	= 0;
	my $index	= $self->_next_index($self->Transaction);
	my $rvec 	= my $ready = my $errors = 0; 
	my $timeout	= $self->Timeout;

	my $readlen 	= 1;
	my $fast_io	= $self->Fast_io;	# Remember setting to reduce future method calls

	if ( $fast_io ) {
		
		# set fcntl if necessary:
		exists $self->{_fcntl} or $self->Fast_io($fast_io);
		$readlen = $self->{Buffer}||4096;
	}
	until (		scalar(@$oBuffer) 			and 	# stuff in output buffer
			$oBuffer->[-1][_DATA] 	=~ /\r\n$/ 	and 	# the last thing there has cr-lf
			$oBuffer->[-1][_TYPE] 	eq "OUTPUT" 	and	# that thing is an output line
			$iBuffer		eq "" 		# and	# and the input buffer has been MT'ed
	) {
              my $transno = $self->Transaction;                       # used below in several places
		if ($timeout) {
			vec($rvec, fileno($self->Socket), 1) = 1;
			my @ready = $self->{_select}->can_read($timeout) ;
			unless ( @ready ) {
				$self->LastError("Tag $transno: " .
					"Timeout after $timeout seconds waiting for data from server\n");	
				$self->_record($transno,
					[	$self->_next_index($transno),
						"ERROR",
						"$transno * NO Timeout after $timeout seconds " .
						"during read from server\r\n"
					]
				);
				$@ = "Timeout after $timeout seconds during read from server\r\n";
				carp "Timeout after $timeout seconds during read from server: $!" 
					if $self->Debug or $^W;
				return undef;
			}
		}
		
		local($^W) = undef;	# Now quiet down warnings

		# read "$readlen" bytes (or less):
              # need to check return code from sysread in case other end has shut down!!!
              my $ret = sysread( $sh, $iBuffer, $readlen, length($iBuffer)) ;
		# $self->_debug("Read so far: $iBuffer<<END>>\n");
              if($timeout and ! defined($ret)) { # Blocking read error...
                  my $msg = "Error while reading data from server: $!\r\n";
                  $self->_record($transno,
                                 [ $self->_next_index($transno),
                                   "ERROR", "$transno * NO $msg "
                                   ]);
                  $@ = "$msg";
                  return undef;
              }
              elsif(defined($ret) and $ret == 0) {    # Caught EOF...
                  my $msg="Socket closed while reading data from server.\r\n";
                  $self->_record($transno,
                                 [ $self->_next_index($transno),
                                   "ERROR", "$transno * NO $msg "
                                   ]);
                  $@ = "$msg";
                  return undef;
              }
              # successfully wrote to other end, keep going...
              $count += $ret;
		LINES: while ( $iBuffer =~ s/^(.*?\r?\n)// ) {
		   my $current_line = $1;

		   # $self->_debug("BUFFER: pulled from buffer: <BEGIN>${current_line}<END>\n" .
		   # 	"and left with buffer contents of: <BEGIN>${iBuffer}<END>\n");

		   LITERAL: if ($current_line =~ s/\{(\d+)\}\r\n$//) {
			# This part handles IMAP "Literals", which according to rfc2060 look something like this:
			# [tag]|* BLAH BLAH {nnn}\r\n
			# [nnn bytes of literally transmitted stuff]
			# [part of line that follows literal data]\r\n

			# Set $len to be length of impending literal:
			my $len = $1 ;
			
			$self->_debug("LITERAL: received literal in line $current_line of length $len; ".
			"attempting to ".
			"retrieve from the " . length($iBuffer) . " bytes in: $iBuffer<END_OF_iBuffer>\n");

			# Transfer up to $len bytes from front of $iBuffer to $litstring: 
			my $litstring = substr($iBuffer, 0, $len);
			$iBuffer = substr($iBuffer, length($litstring), length($iBuffer) - length($litstring) ) ;

			# Figure out what's left to read (i.e. what part of literal wasn't in buffer):
			my $remainder_count = $len - length($litstring);

			if ( defined($literal_callback) ) 	{	# let's assume this is a fh for now
				print $literal_callback $litstring ;
				$litstring = "";
		
			}
			if ($remainder_count > 0 and $timeout) {
				# If we're doing timeouts then here we set up select and wait for data from the
				# the IMAP socket.
				vec($rvec, fileno($self->Socket), 1) = 1;
				unless ( CORE::select( $ready = $rvec, 
							undef, 
							$errors = $rvec, 
							$timeout) 
				) {	# Select failed; that means bad news. Better tell someone.
					$self->LastError("Tag " . $transno . 
						": Timeout waiting for literal data from server\n");	
					carp "Timeout waiting for literal data from server" 
						if $self->Debug or $^W;	
					return undef;
				}	
			} 
			
			fcntl($sh, F_SETFL, $self->{_fcntl}) if $fast_io and defined($self->{_fcntl});
			while ( $remainder_count > 0 ) {	   # As long literal not done,
				my $ret	= sysread(	   	   # bytes read
						$sh, 		   # IMAP handle 
						$litstring,	   # place to read into
						$remainder_count,  # bytes left to read
						length($litstring) # offset to read into
				) ;
				if ( $timeout and !defined($ret)) { # possible timeout
					$self->_record($transno, [ 
							$self->_next_index($transno),
							"ERROR",
							"$transno * NO Error reading data from server: $!\r\n"	
						]
					);
					return undef;
				} elsif ( $ret == 0 and eof($sh) ) {
					$self->_record($transno, [ 
							$self->_next_index($transno),
							"ERROR",
							"$transno * ".
							"BYE Server unexpectedly closed connection: $!\r\n"	
						]
					);
					$self->State(Unconnected);
					return undef;
				}
				$remainder_count -= $ret;	   # decrement remaining bytes by amt read
				if ( defined($literal_callback) ) {
					print $literal_callback $litstring;
					$litstring = "";
				}

			}
			$self->Fast_io($fast_io) if $fast_io;
			# Now let's make sure there are no IMAP server output lines 
			# (i.e. [tag|*] BAD|NO|OK Text) embedded in the literal string
			# (There shouldn't be but I've seen it done!)

			my $embedded_output = 0;
			my $lastline = ( split(/\r?\n/,$litstring))[-1] if $litstring;
			if ( $lastline and $lastline =~ /^(?:\*|(\d+))\s(BAD|NO|OK)/i ) {
				$litstring =~ s/$lastline\r?\n//;
				$embedded_output++;
				$self->_debug("Got server output mixed in with literal: ",
						"$lastline\n") 
					if $self->Debug;
			}

		  	# Finally, we need to stuff the literal onto the end of the oBuffer:
			push 	@$oBuffer , 	[ $index++, "OUTPUT",  $current_line 	] , 
						[ $index++, "LITERAL", $litstring 	];

			push @$oBuffer,		[ $index++, "OUTPUT",  $lastline 	] 
				if $embedded_output;


		  } else { 
			push @$oBuffer, [ $index++, "OUTPUT" , $current_line ]; 
		  }
		
		}
		#$self->_debug("iBuffer is now: $iBuffer<<END OF BUFFER>>\n");
	}
	#	_debug $self, "Buffer is now $buffer\n";
	_debug $self, "Read: " . join("",map {$_->[_DATA]} @$oBuffer) ."\n" if $self->Debug;
	return scalar(@$oBuffer) ? $oBuffer : undef ;
}

sub Report {
	my $self = shift;
#	$self->_debug( "Dumper: " . Data::Dumper::Dumper($self) . 
#			"\nReporting on following keys: " . join(", ",keys %{$self->{'History'}}). "\n");
	return 	map { 
			map { $_->[_DATA] } @{$self->{"History"}{$_}} 
	}		sort { $a <=> $b } keys %{$self->{"History"}}
	;
}


sub Results {
	my $self 	= shift	;
	my $transaction = shift||$self->Count;
	
	return wantarray 							? 
		map {$_->[_DATA] } 	@{$self->{"History"}{$transaction}}	: 
		[ map {$_->[_DATA] }  	@{$self->{"History"}{$transaction}} ]	;
}


sub LastIMAPCommand {
	my @a = map { $_->[_DATA] } @{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}};
	return shift @a;
}


sub History {
	my @a = map { $_->[_DATA] } @{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}};
	shift @a;
	return wantarray ? @a : \@a ;

}


sub logout {
	my $self = shift;
	my $string = "LOGOUT";
	$self->_imap_command($string) ; 
	$self->State(Unconnected);
	$self->{Folders} = undef;
	$self->Socket->close ; $self->{Socket} = undef;
	return $self;
}

sub folders {
        my $self = shift;
	my $what = shift ;
        return wantarray ?      @{$self->{Folders}} :
                                $self->{Folders} 
                if ref($self->{Folders}) and !$what;
	
        my @folders ;  
	my @list = $self->list(undef,( $what? "$what" . $self->separator($what) . "*" : undef ) );
	push @list, $self->list(undef, $what) if $what and $self->exists($what) ;
	# my @list = map { $self->_debug("Pushing $_->[${\(_DATA)}] \n"); $_->[_DATA] } @$output;
	my $m;

	for ($m = 0; $m < scalar(@list); $m++ ) {
		if ($list[$m] && $list[$m]  !~ /\r\n$/ ) {
			$list[$m] .= $list[$m+1] ;
			$list[$m+1] = "";	
		}
			
		
		# $self->_debug("Folders: examining $list[$m]\n");

		push @folders, $1||$2 
			if $list[$m] =~
                        /       ^\*\s+LIST              # * LIST
                                \s+\([^\)]*\)\s+            # (Flags)
                                "[^"]*"\s+              # "delimiter"
                                (?:"([^"]*)"|(.*))\r\n$  # Name or "Folder name"
                        /ix;

        } 

        # for my $f (@folders) { $f =~ s/^\\FOLDER LITERAL:://;}

        $self->{Folders} = \@folders unless $what;

        return wantarray ? @folders : \@folders ;
}


sub exists {
	my ($self,$what) = (shift,shift);
	return $self if $self->STATUS($self->Massage($what),"(MESSAGES)");
	return undef;
}
	
	
sub fetch {

	my $self = shift;
	my $what = shift||"ALL";
	ref($what) and $what = join(",",@$what);	
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) .
				"FETCH $what" . ( @_ ? " " . join(" ",@_) : '' )
	) 	 					or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				[ map { $_->[_DATA] } @{$self->{'History'}{$self->Count}} ];

}
	
sub AUTOLOAD {

	my $self = shift;
	return undef if $Mail::IMAPClient::AUTOLOAD =~ /DESTROY$/;
	delete $self->{Folders}  ;
	$Mail::IMAPClient::AUTOLOAD =~ s/.*:://;
	if (	
			$Mail::IMAPClient::AUTOLOAD =~ /^[a-z]+$/
		and	$Mail::IMAPClient::AUTOLOAD !~ 
				/^	(?:
						store	 |
						copy	 |
						subscribe|
						create	 |
						delete	 |
						close	 |
						expunge
					)$
				/x 
		and 	$^W
	) {
		carp 	"$Mail::IMAPClient::AUTOLOAD is all lower-case. " .
			"May conflict with future methods. " .
			"Change method name to be mixed case or all upper case to ensure upward compatability"
	}
	if (scalar(@_)) {
		my @a = @_;
		if (	
			$Mail::IMAPClient::AUTOLOAD =~ 
				/^(?:subscribe|create|delete|myrights)$/i
		) {
			$a[-1] = $self->Massage($a[-1]) ;
		} elsif (
			$Mail::IMAPClient::AUTOLOAD =~ /^(?:store|copy)$/i
		) {
			$Mail::IMAPClient::AUTOLOAD = "UID $Mail::IMAPClient::AUTOLOAD"
				if $self->Uid;
		} elsif (
			$Mail::IMAPClient::AUTOLOAD =~ /^(?:expunge)$/i and defined($_[0])
		) {
			my $old;
			if ( $_[0] ne $self->Folder ) {
				$old = $self->Folder; $self->select($_[0]); 
			} 	
			my $succ = $self->_imap_command(qq/$Mail::IMAPClient::AUTOLOAD/) ;
			$self->select($old);
			return undef unless $succ;
			return wantarray ? 	$self->History($self->Count) 	: 
						map {$_->[_DATA]}@{$self->{'History'}{$self->Count}}	;
			
		}
		_debug $self, "Autoloading: $Mail::IMAPClient::AUTOLOAD " . join(" ",@a) ,"\n" if $self->Debug;
		return undef unless $self->_imap_command(qq/$Mail::IMAPClient::AUTOLOAD / . join(" ",@a) )  ;
	} else {
		$self->Folder(undef) if $Mail::IMAPClient::AUTOLOAD =~ /^(?:close)/i ; 
		$self->_imap_command(qq/$Mail::IMAPClient::AUTOLOAD/) or return undef;
	}
	return wantarray ? 	$self->History($self->Count) 	: 
				[map {$_->[_DATA] } @{$self->{'History'}{$self->Count}}]	;

}

sub rename {
    my $self = shift;
    my ($from, $to) = @_;
    local($_);
    if ($from =~ /^"(.*)"$/) {
	$from = $1 unless $self->exists($from);
        $from =~ s/"/\\"/g;
    }
    if ($to =~ /^"(.*)"$/) {
	$to = $1 unless $self->exists($from) and $from =~ /^".*"$/;
        $to =~ s/"/\\"/g;
    }
    $self->_imap_command(qq(RENAME "$from" "$to")) or return undef;
    return $self;
}

sub status {

	my $self = shift;
	my $box = shift ;  
	return undef unless defined($box);
	$box = $self->Massage($box);
	my @pieces = @_;
	$self->_imap_command("STATUS $box (". (join(" ",@_)||'MESSAGES'). ")") or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				[map{$_->[_DATA]}@{$self->{'History'}{$self->Count}}];

}


# Can take a list of messages now.
# If a single message, returns array or ref to array of flags
# If a ref to array of messages, returns a ref to hash of msgid => flag arr
# See parse_headers for more information
# 2000-03-22 Adrian Smith (adrian.smith@ucpag.com)

sub flags {
	my $self = shift;
	my $msgspec = shift;
	my $flagset = {};
	my $msg;
	my $u_f = $self->Uid;

	# Determine if set of messages or just one
	if (ref($msgspec) eq 'ARRAY') {
		$msg = join(',', @$msgspec);
	} else {
		$msg = $msgspec;
		if ( scalar(@_) ) {
			$msg .= join(",",@_) ;
			$msgspec = [ $msgspec, @_ ] ;
		}
	}

	# Send command
	unless ( $self->fetch($msg,"FLAGS") ) {
		return undef;
	}

	# Parse results, setting entry in result hash for each line
 	foreach my $resultline ($self->Results) {
		if (	$resultline =~ 
			/	\*\s+(\d+)\s+FETCH\s+	# * nnn FETCH 
				\(			# ( 
				(?:\s?UID\s(\d+)\s?)?	# optional: UID nnn <space>
				FLAGS\s?\((.*)\)\s?	# FLAGS (\Flag1 \Flag2) <space>
				(?:\s?UID\s(\d+))?	# optional: UID nnn
				\) 			# )
			/x
		) {
			$self->_debug("flags: line = '$resultline' and 1,2,3,4 = $1,$2,$3,$4\n") 
					if $self->Debug;
			my $mailid = $u_f ? ( $2||$4) : $1;
			my $flagsString = $3 ;
			my @flags = map { s/\s+$//; $_ } split(/\s+/, $flagsString);
			$flagset->{$mailid} = \@flags;
		}
	}

	# Did the guy want just one response? Return it if so
	if (ref($msgspec) ne 'ARRAY') {
		my $flagsref = $flagset->{$msgspec};
		return wantarray ? @$flagsref : $flagsref;
	}

	# Or did he want a hash from msgid to flag array?
	return $flagset;
}

# parse_headers modified to allow second param to also be a
# reference to a list of numbers. If this is a case, the headers
# are read from all the specified messages, and a reference to
# an hash of mail numbers to references to hashes, are returned.
# I found, with a mailbox of 300 messages, this was
# *significantly* faster against our mailserver (< 1 second
# vs. 20 seconds)
#
# 2000-03-22 Adrian Smith (adrian.smith@ucpag.com)

sub parse_headers {
	my($self,$msgspec,@fields) = @_;
	my(%fieldmap) = map { ( lc($_),$_ )  } @fields;
	my $msg; my $string; my $field;

	# Make $msg a comma separated list, of messages we want
        if (ref($msgspec) eq 'ARRAY') {
		$msg = join(',', @$msgspec);
	} else {
		$msg = $msgspec;
	}

	if ($fields[0] 	=~ 	/^[Aa][Ll]{2}$/ 	) { 

		$string = 	"$msg body" . 
		# use ".peek" if Peek parameter is a) defined and true, 
		# 	or b) undefined, but not if it's defined and untrue:

		( 	defined($self->Peek) 		? 
			( $self->Peek ? ".peek" : "" ) 	: 
			".peek" 
		) .  "[header]" 			; 

	} else {
		$string	= 	"$msg body" .
		# use ".peek" if Peek parameter is a) defined and true, or 
		# b) undefined, but not if it's defined and untrue:

		( defined($self->Peek) 			? 
			( $self->Peek ? ".peek" : "" ) 	: 
			".peek" 
		) .  "[header.fields ("	. join(" ",@fields) 	. ')]' ;
	}

	my @raw=$self->fetch(	$string	) or return undef;

	my $headers = {};	# hash from message ids to header hash
	my $h = 0;		# reference to hash of current msgid, or 0 between msgs
	
        for my $header (map { split(/(?:\r\n)/,$_) } @raw) {
                local($^W) = undef;
                if ( $header =~ /^\*\s+\d+\s+FETCH\s+\(.*BODY\[HEADER(?:\]|\.FIELDS)/i) {
                        if ($self->Uid) {
                                if ( my($msgid) = $header =~ /UID\s+(\d+)/ ) {
                                        $h = {};
                                        $headers->{$msgid} = $h;
                                } else {
                                        $h = {};
                                }
                        } else {
                                if ( my($msgid) = $header =~ /^\*\s+(\d+)/ ) {
                                        #start of new message header:
                                        $h = {};
                                        $headers->{$msgid} = $h;
                                }
                        }
                }
#               my $pattern = $self->Uid ?      'UID\\s+(\\d+)' : '^\\*\\s(\\d+)' ;
#                       if (    my($msgid) = $header =~ /$pattern/
#                       and $header =~ /BODY\[HEADER(?:\]|\.FIELDS)/i
#               ) {
#                       # start of new message header:
#                       $h = {};                  # new hash for headers for this mail
#                       $headers->{$msgid} = $h;  # store in results, against this message
#               }

                next if $header =~ /^\s+$/;

                # ( for vi
                if ($header =~ /^\)/) {           # end of this message
                        $h = 0;                   # set to be between messages
                        next;
                }
                # check for '<optional_white_space>UID<white_space><UID_number><optional_white_space>)'
                # when parsing headers by UID.
                if ($self->Uid and my($msgid) = $header =~ /^\s*UID\s+(\d+)\s*\)/) {
                        $headers->{$msgid} = $h;        # store in results against this message
                        $h = 0;                 	# set to be between messages
                        next;
                }

		if ($h != 0) {			  # do we expect this to be a header?
               		my $hdr = $header;
               		chomp $hdr;
               		$hdr =~ s/\r$//;   
               		if ($hdr =~ s/^(\S+):\s*//) { 
                       		$field = exists $fieldmap{lc($1)} ? $fieldmap{lc($1)} : $1 ;
                       		push @{$h->{$field}} , $hdr ;
               		} elsif ($hdr =~ s/^.*FETCH\s\(.*BODY\[HEADER\.FIELDS.*\)\]\s(\S+):\s*//) { 
                       		$field = exists $fieldmap{lc($1)} ? $fieldmap{lc($1)} : $1 ;
                       		push @{$h->{$field}} , $hdr ;
               		} elsif ( ref($h->{$field}) eq 'ARRAY') {
			        
					$hdr =~ s/^\s+/ /;
                       			$h->{$field}[-1] .= $hdr ;
               		}
		}
	}
	my $candump = 0;
	if ($self->Debug) {
		eval {
			require Data::Dumper;
			Data::Dumper->import;
		};
		$candump++ unless $@;
	}
	# if we asked for one message, just return its hash,
	# otherwise, return hash of numbers => header hash
	if (ref($msgspec) eq 'ARRAY') {
		#_debug $self,"Structure from parse_headers:\n", 
		#	Dumper($headers) 
		#	if $self->Debug;
		return $headers;
	} else {
		#_debug $self, "Structure from parse_headers:\n", 
		#	Dumper($headers->{$msgspec}) 
		#	if $self->Debug;
		return $headers->{$msgspec};
	}
}

sub parse_headers2 {
	my($self,$msgspec,@fields) = @_;
	my(%fieldmap) = map { ( lc($_),$_ )  } @fields;
	my $msg; my $string; my $field;

	# Make $msg a comma separated list, of messages we want
        if (ref($msgspec) eq 'ARRAY') {
		$msg = join(',', @$msgspec);
	} else {
		$msg = $msgspec;
	}

	if ($fields[0] 	=~ 	/^[Aa][Ll]{2}$/ 	) { 

		$string = 	"$msg body" . 
		# use ".peek" if Peek parameter is a) defined and true, 
		# 	or b) undefined, but not if it's defined and untrue:

		( 	defined($self->Peek) 		? 
			( $self->Peek ? ".peek" : "" ) 	: 
			".peek" 
		) .  "[header]" 			; 

	} else {
		$string	= 	"$msg body" .
		# use ".peek" if Peek parameter is a) defined and true, or 
		# b) undefined, but not if it's defined and untrue:

		( defined($self->Peek) 			? 
			( $self->Peek ? ".peek" : "" ) 	: 
			".peek" 
		) .  "[header.fields ("	. join(" ",@fields) 	. ')]' ;
	}

	my @raw=$self->fetch(	$string	) or return undef;

	my $headers = {};	# hash from message ids to header hash
	my $h = 0;		# reference to hash of current msgid, or 0 between msgs
	
       	for my $header (map { split(/(?:\r\n)/,$_) } @raw) {
		local($^W) = undef;
		my $pattern = $self->Uid ? 	'UID\\s+(\\d+)' : '^\\*\\s(\\d+)' ;
               	if (	my($msgid) = $header =~ /$pattern/ 
			and $header =~ /BODY\[HEADER(?:\]|\.FIELDS)/i
		) {	  
			# start of new message header:
			$h = {};		  # new hash for headers for this mail
			$headers->{$msgid} = $h;  # store in results, against this message
		}

                next if $header =~ /^\s+$/;

                # ( for vi
		if ($header =~ /^\)/) {		  # end of this message
			$h = 0;			  # set to be between messages
			next;
		}

		if ($h != 0) {			  # do we expect this to be a header?
               		my $hdr = $header;
               		chomp $hdr;
               		$hdr =~ s/\r$//;   
               		if ($hdr =~ s/^(\S+): //) { 
                       		$field = exists $fieldmap{lc($1)} ? $fieldmap{lc($1)} : $1 ;
                       		push @{$h->{$field}} , $hdr ;
               		} elsif ($hdr =~ s/^.*FETCH\s\(.*BODY\[HEADER\.FIELDS.*\)\]\s(\S+): //) { 
                       		$field = exists $fieldmap{lc($1)} ? $fieldmap{lc($1)} : $1 ;
                       		push @{$h->{$field}} , $hdr ;
               		} elsif ( ref($h->{$field}) eq 'ARRAY') {
			        
					$hdr =~ s/^\s+/ /;
                       			$h->{$field}[-1] .= $hdr ;
               		}
		}
	}
	my $candump = 0;
	if ($self->Debug) {
		eval {
			require Data::Dumper;
			Data::Dumper->import;
		};
		$candump++ unless $@;
	}
	# if we asked for one message, just return its hash,
	# otherwise, return hash of numbers => header hash
	if (ref($msgspec) eq 'ARRAY') {
		#_debug $self,"Structure from parse_headers:\n", 
		#	Dumper($headers) 
		#	if $self->Debug;
		return $headers;
	} else {
		#_debug $self, "Structure from parse_headers:\n", 
		#	Dumper($headers->{$msgspec}) 
		#	if $self->Debug;
		return $headers->{$msgspec};
	}
}



sub recent_count {
	my ($self, $folder) = (shift, shift);

	$self->status($folder, 'RECENT') or return undef;

	chomp(my $r = ( grep { s/\*\s+STATUS\s+.*\(RECENT\s+(\d+)\s*\)/$1/ }
			$self->History($self->Transaction)
	)[0]);

	$r =~ s/\D//g;

	return $r;
}

sub message_count {
	
	my ($self, $folder) = (shift, shift);
	$folder ||= $self->Folder;
	
	$self->status($folder, 'MESSAGES') or return undef;

	chomp(my $m =	   (	grep {s/\*\s+STATUS\s+.*\(MESSAGES\s+(\d+)\s*\).*$/$1/ }
			$self->History($self->Transaction) 
	)[0]);

	$m =~ s/\D//g;

	return $m;

}

{
for my $datum (
                qw(     recent seen
                        unseen
                 )
) {
        no strict 'refs';
        *$datum = sub {
		my $self = shift;
		my @hits;

		my $hits = $self->search("$datum")
			 or return undef;
		return wantarray ? @$hits : $hits;


        };
}
}
{
for my $datum (
                qw(     sentbefore 	sentsince 	senton
			since 		before 		on
                 )
) {
	no strict 'refs';
	*$datum = sub {

		my($self,$time) = (shift,shift);

		my @hits; my $imapdate;
		my @mnt  =      qw{ Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec};

		if ( $time =~ /\d\d-\D\D\D-\d\d\d\d/ ) {
			$imapdate = $time;
		} elsif ( $time =~ /^\d+$/ ) {
			my @ltime = localtime($time);
			$imapdate = sprintf(	"%2.2d-%s-%4.4d", 
						$ltime[3], $mnt[$ltime[4]], $ltime[5] + 1900);
		} else {
			$self->LastError("Invalid date format supplied to '$datum' method.");
			return undef;
		}
		$self->_imap_command( ($self->Uid ? "UID " : "") . "SEARCH $datum $imapdate")
			or return undef;
		my @results =  $self->History($self->Count)     ;

		for my $r (@results) {

		       chomp $r;
		       $r =~ s/\r$//;
		       $r =~ s/^\*\s+SEARCH\s+//i or next;
		       push @hits, grep(/\d/,(split(/\s+/,$r)));
			_debug $self, "Hits are now: ",join(',',@hits),"\n" if $self->Debug;
		}

		return wantarray ? @hits : \@hits;
	}
}
}

#sub Strip_cr {
#	my $self = shift;

#	my $in = $_[0]||$self ;

#	$in =~ s/\r//g  ;

#	return $in;
#}


sub disconnect { $_[0]->logout }


sub search {

	my $self = shift;
	my @hits;
	my @a = @_;
	$a[-1] = $self->Massage($a[-1]) if scalar(@a) > 1; # massage
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) . "SEARCH ". join(' ',@a)) 
		 or return wantarray ? @hits : \@hits ;
	my @results =  $self->History($self->Count) 	;


	for my $r (@results) {
			
               chomp $r;
               $r =~ s/\r$//;
               $r =~ s/^\*\s+SEARCH\s+// or next;
               push @hits, grep(/\d/,(split(/\s+/,$r)));

	}
	
	return wantarray ? @hits : \@hits;
		
}




sub delete_message {

	my $self = shift;
	my $count = 0;
	my @msgs = ();
	for my $arg (@_) {
		if (ref($arg) eq 'ARRAY') {
			push @msgs, @{$arg};
		} else {
			push @msgs, split(/\,/,$arg);
		}
	}
	

	$self->store(join(',',@msgs),'+FLAGS.SILENT','(\Deleted)') and $count = scalar(@msgs);

	return $count;
}

sub restore_message {

	my $self = shift;
	my @msgs = ();
	for my $arg (@_) {
		if (ref($arg) eq 'ARRAY') {
			push @msgs, @{$arg};
		} else {
			push @msgs, split(/\,/,$arg);
		}
	}
	

	$self->store(join(',',@msgs),'-FLAGS','(\Deleted)') ;
	my $count = grep(
			/
				^\*			# Start with an asterisk
				\s\d+			# then a space then a number
				\sFETCH			# then a space then the string 'FETCH'
				\s\(			# then a space then an open paren :-) 
				.*			# plus optional anything
				FLAGS			# then the string "FLAGS"
				.*			# plus anything else
				(?!\\Deleted)		# but never "\Deleted"
			/x,
			$self->Results
	);
	

	return $count;
}


sub uidvalidity {

	my $self = shift; my $folder = shift;

	my $vline = (grep(/UIDVALIDITY/i, $self->status($folder, "UIDVALIDITY")))[0];

	my($validity) = $vline =~ /\(UIDVALIDITY\s+([^\)]+)/;

	return $validity;
}

# 3 status folder (uidnext)
# * STATUS folder (UIDNEXT 290)

sub uidnext {

	my $self = shift; my $folder = $self->Massage(shift);

	my $line = (grep(/UIDNEXT/i, $self->status($folder, "UIDNEXT")))[0];

	my($uidnext) = $line =~ /\(UIDNEXT\s+([^\)]+)/;

	return $uidnext;
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

sub namespace {
	# Returns a (reference to a?) nested list as follows:
	# [ 
	#	[
	#		[ $user_prefix, $user_delim ] ( , [ $user_prefix2, $user_delim] , [etc, etc] ) ,
	#	],
	#	[
	#		[ $shared_prefix, $shared_delim ] ( , [ $shared_prefix2, $shared_delim] , [etc, etc] ) ,
	#	],
	#	[
	#		[ $public_prefix, $public_delim ] ( , [ $public_prefix2, $public_delim] , [etc, etc] ) ,
	#	],
	# ] ;
		
	my $self = shift;
	unless ( $self->has_capability("NAMESPACE") ) {
			my $error = $self->Count . " NO NAMESPACE not supported by " . $self->Server ;
			$self->LastError("$error\n") ;
			$@ = $error;
			carp "$@" if $^W;
	}
	my $namespace = (map({ /^\* NAMESPACE (.*)/ ? $1 : () } @{$self->_imap_command("NAMESPACE")->Results}))[0] ;
	$namespace =~ s/\r?\n$//;
	my($personal,$shared,$public) = $namespace =~ m#
		(NIL|\((?:\([^\)]+\)\s*)+\))\s
		(NIL|\((?:\([^\)]+\)\s*)+\))\s
		(NIL|\((?:\([^\)]+\)\s*)+\))
	#xi;
	
	my @ns = ();
	$self->_debug("NAMESPACE: pers=$personal, shared=$shared, pub=$public\n");
	push @ns, map {
		$_ =~ s/^\((.*)\)$/$1/;
		my @pieces = m#\(([^\)]*)\)#g;
		$self->_debug("NAMESPACE pieces: " . join(", ",@pieces) . "\n");
		my $ref = [];
		foreach my $atom (@pieces) {
			push @$ref, [ $atom =~ m#"([^"]*)"\s*#g ] ;
		}
		$_ =~ /^NIL$/i ? undef : $ref;
	} ( $personal, $shared, $public) ;
	return wantarray ? @ns : \@ns;
}

sub is_parent {
	my ($self, $folder) = (shift, shift);
	# $self->_debug("Checking parentage " . ( $folder ? "for folder $folder" : "" ) . "\n");
        my $list = $self->list(undef, $folder);
	my $line;

        for (my $m = 0; $m < scalar(@$list); $m++ ) {
		#$self->_debug("Judging whether or not $list->[$m] is fit for parenthood\n");
		return undef if $list->[$m] =~ /NoInferior/i; # let's not beat around the bush
                if ($list->[$m]  =~ s/(\{\d+\})\r\n$// ) {
                        $list->[$m] .= $list->[$m+1];
                        $list->[$m+1] = "";
                }

	    	$line = $list->[$m]
                        if $list->[$m] =~
                        /       ^\*\s+LIST              # * LIST
                                \s+\([^\)]*\)\s+            # (Flags)
                                "[^"]*"\s+              # "delimiter"
                                (?:"([^"]*)"|(.*))\r\n$  # Name or "Folder name"
                        /x;
	}	
	my($f) = $line =~ /^\*\s+LIST\s+\(([^\)]*)\s*\)/ if $line;
	unless ( $f =~ /\\/) {		# no flags at all unless there's a backslash
		my $sep = $self->separator($folder);
		return 1 if scalar(grep /^$folder$sep/, $self->folders);
		return 0;
	}
	return  $f =~ /HasChildren/i ? 1 : ( $f =~ /HasNoChildren/i ? 0 : undef ) ;
}


sub append {

        my $self = shift;
        my $folder = $self->Massage(shift);

	my $text = join("\r\n",@_);
	$text =~ s/\r?\n/\r\n/g;

        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);

        my $string = "$count APPEND $folder {" . length($text)  . "}\r\n" ;

        $self->_record($count,[ $self->_next_index($count), "INPUT", "$string\r\n" ] );

	# Step 1: Send the append command.

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output) = ("","");	
	
	# Step 2: Get the "+ go ahead" response
	until ( $code ) {
		$output = $self->_read_line or return undef;	
		foreach my $o (@$output) { 

			$self->_record($count,$o);	# $o is already an array ref
			next unless $self->_is_output($o);

			($code) = $o->[_DATA] =~ /(^\+|^\d*\s*NO|^\d*\s*BAD)/i ;

			if ($o->[_DATA] =~ /^\*\s+BYE/i) {
                		$self->LastError("Error trying to append: " . $o->[_DATA]. "; Disconnected.\n");
                		$self->_debug("Error trying to append: " . $o->[_DATA]. "; Disconnected.\n");
				carp("Error trying to append: " . $o->[_DATA] ."; Disconnected") if $^W;
				$self->State(Unconnected);

                      } elsif ( $o->[_DATA] =~ /^\d*\s*(NO|BAD)/i ) { # i and / transposed!!!
                		$self->LastError("Error trying to append: " . $o->[_DATA]  . "\n");
				$self->_debug("Error trying to append: " . $o->[_DATA] . "\n");
				carp("Error trying to append: " . $o->[_DATA]) if $^W;
				return undef;
			}
		}
	}	
	
	$self->_record($count,[ $self->_next_index($count), "INPUT", "$text\r\n" ] );

	# Step 3: Send the actual text of the message:
        $feedback = $self->_send_line("$text\r\n");

        unless ($feedback) {
                $self->LastError("Error sending append msg text to IMAP: $!\n");
                return undef;
        }
	$code = undef;			# clear out code

	# Step 4: Figure out the results:
        until ($code) {
                $output = $self->_read_line or return undef;
		$self->_debug("Append results: " . map({ $_->[_DATA] } @$output) . "\n" )
			if $self->Debug;
                foreach my $o (@$output) {
			$self->_record($count,$o); # $o is already an array ref

        		($code) = $o->[_DATA] =~ /^(?:$count|\*) (OK|NO|BAD)/im 	;
			
			if ($o->[_DATA] =~ /^\*\s+BYE/im) {
				$self->State(Unconnected);
                		$self->LastError("Error trying to append: " . $o->[_DATA] . "\n");
				$self->_debug("Error trying to append: " . $o->[_DATA] . "\n");
				carp("Error trying to append: " . $o->[_DATA] ) if $^W;
			}
			if ($code and $code !~ /^OK/im) {
				$self->LastError("Error trying to append: " . $o->[_DATA] . "\n");
				$self->_debug("Error trying to append: " . $o->[_DATA] . "\n");
				carp("Error trying to append: " . $o->[_DATA] ) if $^W;
				return undef;
			}
        	}
	}

	my($uid) = join("",map { $_->[_TYPE] eq "OUTPUT" ? $_->[_DATA] : () } @$output ) =~ m#\s+(\d+)\]#;

        return defined($uid) ? $uid : $self;
}

sub append_file {

        my $self 	= shift;
        my $folder 	= $self->Massage(shift);
	my $file 	= shift; 
	my $control 	= shift || undef;
	my $count 	= $self->Count($self->Count+1);


	unless ( -f $file ) {
		$self->LastError("File $file not found.\n");
		return undef;
	}

	my $fh = IO::File->new($file) ;

	unless ($fh) {
		$self->LastError("Unable to open $file: $!\n");
		$@ = "Unable to open $file: $!" ;
		carp "unable to open $file: $!" if $^W;
		return undef;
	}

	my $bare_nl_count = scalar grep { /^\n$|[^\r]\n$/} <$fh>;

	seek($fh,0,0);
	
        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $length = ( -s $file ) + $bare_nl_count;

        my $string = "$count APPEND $folder {" . $length  . "}\r\n" ;

        $self->_record($count,[ $self->_next_index($count), "INPUT", "$string" ] );

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		close $fh;
		return undef;
	}

	my ($code, $output) = ("","");	
	
	until ( $code ) {
		$output = $self->_read_line or close $fh, return undef;	
		foreach my $o (@$output) {
			$self->_record($count,$o);		# $o is already an array ref
			($code) = $o->[_DATA] =~ /(^\+|^\d+\sNO|^\d+\sBAD)/i; 
			if ($o->[_DATA] =~ /^\*\s+BYE/) {
				carp $o->[_DATA] if $^W;
				$self->State(Unconnected);
				close $fh;
				return undef ;
			} elsif ( $o->[_DATA]=~ /^\d+\s+(NO|BAD)/i ) {
				carp $o->[_DATA] if $^W;
				close $fh;
				return undef;
			}
		}
	}	
	
	{ 	# Narrow scope
		# Slurp up headers: later we'll make this more efficient I guess
		local $/ = "\r\n\r\n"; 
		my $text = <$fh>;
		$text =~ s/\r?\n/\r\n/g;
		$self->_record($count,[ $self->_next_index($count), "INPUT", "{From file $file}" ] ) ;
		$feedback = $self->_send_line($text);

		unless ($feedback) {
			$self->LastError("Error sending append msg text to IMAP: $!\n");
			close $fh;
			return undef;
		}
		_debug $self, "control points to $$control\n" if ref($control) and $self->Debug;
		$/ = 	$control ? 	$control : 	"\n";	
		while (defined($text = <$fh>)) {
			$text =~ s/\r?\n/\r\n/g;
			$self->_record(	$count,
					[ $self->_next_index($count), "INPUT", "{from $file}\r\n" ] 
			);
			$feedback = $self->_send_line($text,1);

			unless ($feedback) {
				$self->LastError("Error sending append msg text to IMAP: $!\n");
				close $fh;
				return undef;
			}
		}
		$feedback = $self->_send_line("\r\n");

		unless ($feedback) {
			$self->LastError("Error sending append msg text to IMAP: $!\n");
			close $fh;
			return undef;
		}
	} 

	# Now for the crucial test: Did the append work or not?
	($code, $output) = ("","");	

	my $uid = undef;	
	until ( $code ) {
		$output = $self->_read_line or return undef;	
		foreach my $o (@$output) {
			$self->_record($count,$o);		# $o is already an array ref
			$self->_debug("append_file: Deciding if " . $o->[_DATA] . " has the code.\n") 
				if $self->Debug;
			($code) = $o->[_DATA]  =~ /^\d+\s(NO|BAD|OK)/i; 
			# try to grab new msg's uid from o/p
			$o->[_DATA]  =~ m#UID\s+\d+\s+(\d+)\]# and $uid = $1; 
			if ($o->[_DATA] =~ /^\*\s+BYE/) {
				carp $o->[_DATA] if $^W;
				$self->State(Unconnected);
				close $fh;
				return undef ;
			} elsif ( $o->[_DATA]=~ /^\d+\s+(NO|BAD)/i ) {
				carp $o->[_DATA] if $^W;
				close $fh;
				return undef;
			}
		}
	}	
	close $fh;

	if ($code !~ /^OK/i) {
		return undef;
	}


        return defined($uid) ? $uid : $self;
}


sub authenticate {

        my $self 	= shift;
        my $scheme 	= shift;
        my $response 	= shift;

        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);


        my $string = "$count AUTHENTICATE $scheme";

        $self->_record($count,[ $self->_next_index($self->Transaction), "INPUT", "$string\r\n"] );

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	
	until ($code) {
		$output = $self->_read_line or return undef;	
		foreach my $o (@$output) {
			$self->_record($count,$o);	# $o is a ref
			($code) = $o->[_DATA] =~ /^\+ (.*)\+$/ ;
			if ($o->[_DATA] =~ /^\*\s+BYE/) {
				$self->State(Unconnected);
				return undef ;
			}
		}
	}	
	
        return undef if $code =~ /^BAD|^NO/ ;

        $feedback = $self->_send_line($response->($code));

        unless ($feedback) {
                $self->LastError("Error sending append msg text to IMAP: $!\n");
                return undef;
        }

        until ($code) {
                $output = $self->_read_line or return undef;
		foreach my $o (@$output) {
                	$self->_record($count,$o);	# $o is a ref
        		$o->[_DATA] =~ /^$count (OK|NO|BAD)/ and $code = $1;
			if ($o->[_DATA] =~ /^\*\s+BYE/) {
				$self->State(Unconnected);
				return undef ;
			}
		}
        }

        return $code =~ /^OK/ ? $self : undef ;

}

# UIDPLUS response from a copy: [COPYUID (uidvalidity) (origuid) (newuid)]
sub copy {

	my($self, $target, @msgs) = @_;

	$target = $self->Massage($target);
	@msgs   = sort { $a <=> $b } map { ref($_)? @$_ : split(',',$_) } @msgs;

	$self->_imap_command( 
	  ( 	$self->Uid ? "UID " : "" ) . 
		"COPY " . 
		join(',',map { ref($_)? @$_ : $_ } @msgs) . 
		" $target"
	) 			or return undef		;
	my @results =  $self->History($self->Count) 	;
	
	my @uids;

	for my $r (@results) {
			
               chomp $r;
               $r =~ s/\r$//;
               $r =~ s/^.*\[COPYUID\s+\d+\s+[\d:,]+\s+([\d:,]+)\].*/$1/ or next;
               push @uids, ( $r =~ /(\d+):(\d+)/ ? $1 ... $2 : split(/,/,$r) ) ;

	}

	return scalar(@uids) ? join(",",@uids) : $self;
}

sub move {

	my($self, $target, @msgs) = @_;

	$self->create($target) and $self->subscribe($target) 
		unless $self->exists($target);

	my $uids = $self->copy($target, map { ref($_) ? @{$_} : $_ } @msgs) or return undef;

	$self->delete_message(@msgs);
	
	return $uids;
}

sub set_flag {
	my($self, $flag, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$flag =~ /^\\/ or $flag = "\\" . $flag ;
	$self->store( join(",",@msgs), "+FLAGS.SILENT (" . $flag . ")" );
}

sub see {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->set_flag('\\Seen', @msgs);
}

sub unset_flag {
	my($self, $flag, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$flag =~ /^\\/ or $flag = "\\" . $flag ;
	$self->store( join(",",@msgs), "-FLAGS.SILENT (" . $flag . ")" );
}

sub deny_seeing {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->unset_flag('\\Seen', @msgs);
}

sub size {

	my ($self,$msg) = @_;

	my($size) = grep(/RFC822\.SIZE/,$self->fetch($msg,"(RFC822.SIZE)"));

	$size =~ /RFC822\.SIZE\s+(\d+)/;
	
	return $1;
}

sub getquotaroot {
	my $self = shift;
	my $what = shift;
	$what = ( $what ? $self->Massage($what) : "INBOX" ) ;
	$self->_imap_command("getquotaroot $what") or return undef;
	return $self->Results;
}

sub getquota {
	my $self = shift;
	my $what = shift;
	$what = ( $what ? $self->Massage($what) : "user/$self->{User}" ) ;
	$self->_imap_command("getquota $what") or return undef;
	return $self->Results;
}

sub quota 	{
	my $self = shift;
	my ($what) = shift||"INBOX";
	$self->_imap_command("getquota $what")||$self->getquotaroot("$what");
	return (	map { s/.*STORAGE\s+\d+\s+(\d+).*\n$/$1/ ? $_ : () } $self->Results
	)[0] ;
}

sub quota_usage 	{
	my $self = shift;
	my ($what) = shift||"INBOX";
	$self->_imap_command("getquota $what")||$self->getquotaroot("$what");
	return (	map { s/.*STORAGE\s+(\d+)\s+\d+.*\n$/$1/ ? $_ : () } $self->Results
	)[0] ;
}

sub Massage {
	my $self= shift;
	my $arg = shift;

	my $escaped_arg = $arg; $escaped_arg =~ s/"/\\"/g;
	$arg 	= substr($arg,1,length($arg)-2) if $arg =~ /^".*"$/
                and ! ( $self->STATUS(qq("$escaped_arg"),"(MESSAGES)"));


	if ($arg =~ /["'\\]/) {
		$arg = "{" . length($arg) . "}\r\n$arg" ;
	} else {
		$arg = qq("${arg}") unless $arg =~ /^"/;
	}

	return $arg;
}

sub unseen_count {

	my ($self, $folder) = (shift, shift);
	$folder ||= $self->Folder;
	$self->status($folder, 'UNSEEN') or return undef;

	chomp(	my $r = ( grep 
			  { s/\*\s+STATUS\s+.*\(UNSEEN\s+(\d+)\s*\)/$1/ }
			  $self->History($self->Transaction)
			)[0]
	);

	$r =~ s/\D//g;
	return $r;
}



# Status Routines:

sub Status 		{ return $_[0]->State 				;	}
sub IsUnconnected 	{ return ($_[0]->State == Unconnected)	? 1 : 0 ; 	}
sub IsConnected 	{ return ($_[0]->State >= Connected) 	? 1 : 0 ; 	}
sub IsAuthenticated 	{ return ($_[0]->State >= Authenticated)? 1 : 0 ; 	}
sub IsSelected 		{ return ($_[0]->State == Selected) 	? 1 : 0 ; 	}		

# The following private methods all work on an output line array.
# _data returns the data portion of an output array:
sub _data {  return $_[1]->[_DATA]; }

# _index returns the index portion of an output array:
sub _index { return $_[1]->[_INDEX]; }

# _type returns the type portion of an output array:
sub _type {  return $_[1]->[_TYPE]; }

# _is_literal returns true if this is a literal:
sub _is_literal { return $_[1]->[_TYPE] eq "LITERAL" };

# _is_output_or_literal returns true if this is an 
#  	output line (or the literal part of one):
sub _is_output_or_literal { 
		return 	$_[1]->[_TYPE] eq "OUTPUT" || $_[1]->[_TYPE] eq "LITERAL" 
};

# _is_output returns true if this is an output line:
sub _is_output { return $_[1]->[_TYPE] eq "OUTPUT" };

# _is_input returns true if this is an input line:
sub _is_input { return $_[1]->[_TYPE] eq "INPUT" };

# _next_index returns next_index for a transaction; may legitimately return 0 when successful.
sub _next_index { 
	return defined(scalar(@{$_[0]->{'History'}{$_[1]||$_[0]->Transaction}}))	? 
		scalar(@{$_[0]->{'History'}{$_[1]||$_[0]->Transaction}}) 		: 0 
};


=head1 SYNOPSIS

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
This documentation reserves the use of the term I<mailbox> to refer to the set of folders owned 
by a specific IMAP id.

RFC2060 defines four possible states for an IMAP connection: not authenticated, authenticated,
selected, and logged out. These correspond to the B<IMAPClient> constants B<Connected>, 
B<Authenticated>, B<Selected>, and B<Unconnected>, respectively. These constants are 
implemented as class methods, and can be used in conjunction with the B<Status> method to 
determine the status of an B<IMAPClient> object and its underlying IMAP session. Note that an 
B<IMAPClient> object can be in the B<Unconnected> state both before a server connection is made
and after it has ended. This differs slightly from RFC2060, which does not define a 
pre-connection status. For a discussion of the methods available for examining the B<IMAPClient>
object's status, see the section labeled "Status Methods", below.

=head2 Errors

If you attempt an operation that results in an error, then you can retrieve the text of the error
message by using the B<LastError> method. However, since the B<LastError> method is an object method
(and not a class method) you will only be able to use this method if you've successfully created your 
object. Errors in the B<new> method can prevent your object from ever being created. Additionally, if
you supply the I<Server>, I<User>, and I<Password> parameters to B<new>, it will attempt to call 
B<connect> and B<login>, either of which could fail and cause your B<new> method call to return undef
(in which case your object will have been created but its reference will have been discarded before
ever being returned to you).

If this happens to you, you can always check C<$@>. B<Mail::IMAPClient> will populate that variable with 
something useful if either of the B<new>, B<connect>, or B<login> methods fail. In fact, as of version
2, the C<$@> variable will always contain error info from the last error, so you can print that instead
of calling B<LastError> if you wish.

If you run your script with warnings turned on (which I'm sure you'll do at some point because it's 
such a good idea) then any error message that gets placed into the I<LastError> slot (and/or in C<$@>)
will automatically generate a warning. 

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
sessions do not eat up unreasonable amounts of memory. See the discussion of B<Clear> under 
L<"parameters"> for more information.

The B<Report> transaction returns the history of the entire IMAP session since the 
initial connection or for the last I<Clear> transactions.  This provides a record of the entire 
conversation, including client command strings and server responses, and is a wonderful debugging tool 
as well as a useful source of raw data for custom parsing.

=head2 Class Methods

There are a couple of methods that can be invoked as class methods. Generally they can be 
invoked as an object method as well, as a convenience to the programmer. (That is, as a 
convenience to the programmer who wrote this module, as well as the programmers using it. 
It's easier I<not> to enforce a class method's classiness.) Note that if the B<new> method 
is called as an object method, the object returned is identical to what have would been 
returned if B<new> had been called as a class method. It doesn't give you a copy of the original 
object or anything like that.

=over 4

=item new()

The B<new> method creates a new instance of an B<IMAPClient> object. If the I<Server> parameter
is passed as an argument to B<new>, then B<new> will implicitly call the B<connect> method, placing
the new object in the I<Connected> state. If I<User> and I<Password> values are also provided, then 
B<connect> will in turn call B<login>, and the resulting object will be returned from B<new> in the 
I<Authenticated> state.

If the I<Server> parameter is not supplied then the B<IMAPClient> object is created in the 
I<Unconnected> state.

If the B<new> method is passed arguments then those arguments will be treated as a list of key=>value
pairs. The key should be one of the parameters as documented under L<"parameters">, below. 

Here are some examples:

	use Mail::IMAPClient;
	
	my $imap = Mail::IMAPClient->new;	# returns an unconnected Mail::IMAPClient object
	#	...				# intervening code using the 1st object, then:
	$imap = Mail::IMAPClient->new(		# returns a new, authenticated Mail::IMAPClient object
			Server => $host,
			User 	=> $id,
			Password=> $pass,
			Clear	=> 5,		# Unnecessary since '5' is the default
	#		...			# Other key=>value pairs go here
	)	or die "Cannot connect to $host as $id: $@";
	
	

See also L<"parameters">, below, and L<"connect"> and L<"login"> for information on how to manually 
connect and login after B<new>.

=cut

=item Unconnected()

returns a value equal to the numerical value associated with an object in the B<Unconnected> 
state.

=item Connected()

returns a value equal to the numerical value associated with an object in the B<Connected> 
state.

=item Authenticated()

returns a value equal to the numerical value associated with an object in the B<Authenticated> 
state.

=item Selected()

returns a value equal to the numerical value associated with an object in the B<Selected> 
state.

=item Strip_cr()

The B<Strip_cr> method strips carriage returns from IMAP client command output. Although 
RFC2060 specifies that lines in an IMAP conversation end with <CR><LF>, it is often cumbersome
to have the carriage returns in the returned data. This method accepts one or more lines of text as 
arguments, and returns those lines with all <CR><LF> sequences changed to <LF>. Any input argument
with no carriage returns is returned unchanged. If the first argument (not counting the class name or
object reference) is an array reference, then members of that array are processed as above and subsequent
arguments are ignored. If the method is called in scalar context then an array reference is returned instead
of an array of results.

Taken together, these last two lines mean that you can do something like:
 
	my @list = $imap->some_imap_method ;
        @list = $imap->Strip_cr(@list) ; 
	# or: 
	my $list = [ $imap->some_imap_method ] ; # returns an array ref
	$list = $imap->Strip_cr($list);

B<NOTE: Strip_cr> does not remove new line characters.

=cut

=item Rfc2060_date()

The B<Rfc2060_date> method accepts one input argument, a number of seconds since
the epoch date. It returns an RFC2060 compliant date string for that date
(as required in date-related arguments to SEARCH, such as "since", "before", etc.). 

=item Rfc822_date()

The B<Rfc822_date> method accepts one input argument, a number of seconds since
the epoch date. It returns an RFC822 compliant date string for that date
(without the 'Date:' prefix). Useful for putting dates in message strings before
calling B<append>.

=back

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

=item Clear()

The name of this parameter, for historical reasons, is somewhat misleading. It should be named
I<Wrap>, because it specifies how many transactions are stored in the wrapped history buffer. But
it didn't always work that way; the buffer used to actually get cleared. The name though remains 
the same in the interests of backwards compatibility. 

I<Clear> specifies that the object's history buffer should be wrapped after every I<n> transactions, 
where I<n> is the value specified for the I<Clear> parameter.  Calling the eponymous B<Clear> 
method without an argument will return the current value of the I<Clear> parameter but will not 
cause clear the history buffer to wrap. 

Setting I<Clear> to 0 turns off automatic history buffer wrapping, and setting it to 1 
turns off the history buffer facility (except for the last transaction, which cannot be 
disabled without breaking the B<IMAPClient> module). Setting I<Clear> to 0 will not cause an
immediate clearing of the history buffer; setting it to 1 (or any other number) will (except of
course for that inevitable last transaction). 

The default I<Clear> value is set to five in order to conserve memory.

=item Debug()

Sets the debugging flag to either a true or false value. Can be supplied with the B<new>
method call or separately by calling the B<Debug> object method. Use of this parameter is
strongly recommended when debugging scripts and required when reporting bugs.

=item Debug_fh()

Specifies the filehandle to which debugging information should be printed. It can either a
filehandle object reference or a filehandle glob. The default is to print debugging info to
STDERR.

For example, you can:

	use Mail::IMAPClient;
	use IO::File;
	# set $user, $pass, and $server here
	my $dh = IO::File->new(">debugging.output") 
		or die "Can't open debugging.output: $!\n";
	my $imap = Mail::IMAPClient->new(	User=>$user, Password=>$pass, 
						Server=>$server, Debug=> "yes, please",
						Debug_fh => $dh
	);
	
Or you can:


	use Mail::IMAPClient;
	# set $user, $pass, and $server here
	open(DBG,">debugging.output") 
		or die "Can't open debugging.output: $!\n";
	my $imap = Mail::IMAPClient->new(	User=>$user, Password=>$pass, 
						Server=>$server, Debug=> 1,
						Debug_fh => *DBG
	);


=item Folder()

The I<Folder> parameter returns the name of the currently-selected folder (in case you forgot).
It can also be used to set the name of the currently selected folder, which is completely unnecessary if
you used the B<select> method (or B<select>'s read-only equivalent, the B<examine> method) to select it. 
Note that setting the I<Folder> parameter does not automatically select a new folder; you use the 
B<select> or B<examine> object methods for that. Generally, the I<Folder> parameter should only be
queried (by using the no-argument form of the B<Folder> method). You will only need to set the I<Folder>
parameter if you use some mysterious technique of your own for selecting a folder, which you probably
won't do.

=cut

=item Password()

Specifies the password to use when logging into the IMAP service on the host specified in the
I<Server> parameter as the user specified in the I<User> parameter. Can be supplied with 
the B<new> method call or separately by calling the B<Password> object method.

If I<Server>, I<User>, and I<Password> are all provided to the B<new> method, then the newly
instantiated object will be connected to the host specified in I<Server> (at either the port
specified in I<Port> or the default port 143) and then logged on as the user specified in 
the I<User> parameter (using the password provided in the I<Password> parameter). See the 
discussion of the B<new> method, below.

=item Peek()

Setting I<Peek> to a true value will prevent the B<body_string>, B<message_string> and B<message_to_file> 
methods from automatically setting the I<\Seen> flag. Setting I<Peek> to 0 (zero) will force B<body_string>,
B<message_string>, B<message_to_file>, and B<parse_headers> to always set the I<\Seen> flag.

The default is to set the seen flag whenever you fetch the body of a message but not when you just fetch the
headers. Passing I<undef> to the eponymous B<Peek> method will reset the I<Peek> parameter to its pristine,
default state. 

=cut

=item Port()

Specifies the port on which the IMAP server is listening. The default is 143, which is the 
standard IMAP port. Can be supplied with the B<new> method call or separately by calling 
the B<Port> object method.

=item Server()

Specifies the hostname or IP address of the host running the IMAP server. If provided as part
of the B<new> method call, then the new IMAP object will automatically be connected at the 
time of instantiation. (See the B<new> method, below.) Can be supplied with the B<new> 
method call or separately by calling the B<Server> object method.

=cut

=item Socket()

The I<Socket> method can be used to obtain the socket handle of the current connection (say, to
do I/O on the connection that is not otherwise supported by B<Mail::IMAPClient>) or to replace
the current socket with a new handle (perhaps an SSL handle, for example). 

If you supply a socket handle yourself, either by doing something like:

	 $imap=Mail::IMAPClient->new(Socket=>$sock, User => ... );

or by doing something like:

	 $imap=Mail::IMAPClient->new(User => $user, Password => $pass, Server => $host);
	 # blah blah blah
	 $imap->Socket($ssl);

then it will be up to you to establish the connection AND to authenticate, either via the B<login>
method, or the fancier B<authenticate>, or, since you know so much anyway, by just doing raw I/O 
against the socket until you're logged in. If you do any of this then you should also set the 
I<State> parameter yourself to reflect the current state of the object (i.e. Connected, 
Authenticated, etc).

=cut

=item Timeout()

Specifies the timeout value in seconds for reads. Specifying a true value for I<Timeout> 
will prevent B<Mail::IMAPClient> from blocking in a read.

Since timeouts are implemented via the perl B<select> operator, the I<Timeout> parameter
may be set to a fractional number of seconds. Not supplying a I<Timeout>, or (re)setting it to 
zero, disables the timeout feature.

=cut

=item Uid()

If I<Uid> is set to a true value (i.e. 1) then the behavior of the B<fetch>, B<search>, B<copy>,
and B<store> methods (and their derivatives) is changed so that arguments that would otherwise be 
message sequence numbers are treated as message UID's and so that return values (in the case of the 
B<search> method and its derivatives) that would normally be message sequence numbers are 
instead message UID's.

Internally this is implemented as a switch that, if turned on, causes methods that would otherwise
issue an IMAP FETCH, STORE, SEARCH, or COPY client command to instead issue UID FETCH, UID STORE,
UID SEARCH, or UID COPY, respectively. The main difference between message sequence numbers and 
message UID's is that, according to RFC2060, UID's must not change during a session and should not
change between sessions, and must never be reused. Sequence numbers do not have that same guarantee and
in fact may be reused right away. 

Since foldernames also have a unique identifier (UIDVALIDITY), which is provided when the folder 
is B<select>ed or B<examine>d or by doing something like "$imap->status($folder,"UIDVALIDITY"), 
it is possible to uniquely identify every message on the server, although normally you won't need 
to bother.

The methods currently affected by turning on the I<Uid> flag are:

	copy 		fetch
	search 		store 
	message_string 	message_uid
	body_string 	flags
	move 		size
	parse_headers

Note that if for some reason you only want the I<Uid> parameter turned on for one command, then
you can choose between the following two snippets, which are equivalent:

Example 1:

	$imap->Uid(1);
	my @uids = $imap->search('SUBJECT',"Just a silly test"); # 
	$imap->Uid(0);

Example 2:

	my @uids; 
       	foreach $r ($imap->UID("SEARCH","SUBJECT","Just a silly test") {
	       chomp $r;
	       $r =~ s/\r$//;
	       $r =~ s/^\*\s+SEARCH\s+// or next;
	       push @uids, grep(/\d/,(split(/\s+/,$r)));
	}

In the second example, we used the default method to issue the UID IMAP Client command, being 
careful to use an all-uppercase method name so as not to inadvertently call the I<Uid> accessor method. 
Then we parsed out the message UIDs manually, since we don't have the benefit of the built-in 
B<search> method doing it for us.
	
Please be very careful when turning the I<Uid> parameter on and off throughout a script. If you loose
track of whether you've got the I<Uid> parameter turned on you might do something sad, like deleting
the wrong message. Remember, like all eponymous accessor methods, the B<Uid> method without arguments
will return the current value for the I<Uid> parameter, so do yourself a favor and check. The safest
approach is probably to turn it on at the beginning and then leave it on. (Remember that leaving it
turned off can lead to problems if changes to a folder's contents cause resequencing.) 

By default, the I<Uid> parameter is turned on.

=item User()

Specifies the userid to use when logging into the IMAP service. Can be supplied with the 
B<new> method call or separately by calling the B<User> object method.

Parameters can be set during B<new> method invocation by passing named parameter/value pairs
to the method, or later by calling the parameter's eponymous object method.

=back

=cut

=head2 Object Methods

Object methods must be invoked against objects created via the B<new> method. They cannot be
invoked as class methods, which is why they are "object methods" and not "class methods". 

There are basically two types of object methods--those that participate in the IMAP session's
conversation (i.e. they issue IMAP client commands) and those that do not. Methods that do
not result in new IMAP client commands being issued (such as the B<Transaction, Status>, and
B<History> methods) all begin with an uppercase letter, to distinguish themselves from methods
that do correspond to IMAP client commands. (Class methods and eponymous parameter methods 
likewise begin with an uppercase letter because they also do not correspond to an IMAP client
command.) 

The B<IMAPClient> object methods are:

=over 4

=item append()

The B<append> method adds a message to the specified folder. It takes two arguments, the
name of the folder to append the message to, and the text of the message (including headers).
Additional arguments are added to the message text, separated with <CR><LF>.

The B<append> method returns the UID of the new message (a true value) if successful, 
or undef if not, if the IMAP server has the UIDPLUS capability.  If it doesn't then you just 
get true on success and undef on failure.

Note that many servers will get really ticked off if you try to append a message that contains
"bare newlines", which is the titillating term given to newlines that are not preceded by a 
carrage return. To protect against this, B<append> will insert a carrage return before any newline
that is "bare". If you don't like this behavior then you can avoid it by not passing naked newlines 
to B<append>.

=cut

=item append_file()

The B<append_file> method adds a message to the specified folder. It takes two arguments, the
name of the folder to append the message to, and the file name of an RFC822-formatted message.

An optional third argument is the value to use for C<input_record_separator>. The default is
to use "" for the first read (to get the headers) and "\n" for the rest. Any valid value for
C<$/> is acceptable, even the funky stuff, like C<\1024>. (See L<perlvar> for more information
on C<$/>).

The B<append_file> method returns the UID of the new message (a true value) if successful, 
or undef if not, if the IMAP server has the UIDPLUS capability. If it doesn't then you just 
get true on success and undef on failure. If you supply a filename that doesn't exist then you
get an automatic undef. The B<LastError> method will remind you of this if you forget that your
file doesn't exist but somehow manage to remember to check B<LastError>.

In case you're wondering, B<append_file> is provided mostly as a way to allow large messages
to be appended without having to have the whole file in memory. It uses the C<-s> operator to
obtain the size of the file and then reads and sends the contents line by line (or not, depending
on whether you supplied that optional third argument).

=cut

=item authenticate()

The B<authenticate> method accepts two arguments, an authentication type to be used (ie CRAM-MD5)
and a code or subroutine reference to execute to obtain a response. The B<authenticate> assumes that
the authentication type specified in the first argument follows a challenge-response flow. The 
B<authenticate> method issues the IMAP Client AUTHENTICATE command and receives a challenge from the
server. That challenge (minus any tag prefix or enclosing '+' characters but still in the original
base64 encoding) is passed as the only argument to the code or subroutine referenced in the second 
argument. The return value from the 2nd argument's code is written to the server as is, except that 
a <CR><NL> sequence is appended if neccessary.

=cut

=item before()

The B<before> method works just like the L<"since"> method, below, 
except it returns a list of messages whose internal system dates 
are before the date supplied as the argument to the B<before> method.

=cut

=item body_string()

The B<body_string> method accepts a message sequence number (or a message UID, if the I<Uid> parameter 
is set to true) as an argument and returns the message body as a string. The returned value contains 
the entire message in one scalar variable, without the message headers.

=cut

=item capability()

The B<capability> method returns an array of capabilities as returned by the CAPABILITY IMAP 
Client command, or a reference to an array of capabilities if called in scalar context. 
If the CAPABILITY IMAP Client command fails for any reason then the B<capability> method will
return undef.

=item close()

The B<close> method is implemented via the default method and is used to close the currently selected folder
via the CLOSE IMAP client command.  According to RFC2060, the CLOSE command performs an implicit EXPUNGE,
which means that any messages that you've flagged as I<\Deleted> (say, with the B<delete_message> method) will
now be deleted. If you haven't deleted any messages then B<close> can be thought of as an "unselect".

See also B<delete_message>, B<expunge>, and your tattered copy of RFC2060.

=item connect()

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

=item copy()

The B<copy> method requires a folder name as the first argument, and a list of one or more messages
sequence numbers (or messages UID's, if the I<UID> parameter is set to a true value). The message 
sequence numbers or UID's should refer to messages in the currenly selected folder. Those messages 
will be copied into the folder named in the first argument.

The B<copy> method returns undef on failure and a true value if successful. If the server to which 
the current Mail::IMAPClient object is connected supports the UIDPLUS capability then the true value
returned by B<copy> will be a comma separated list of UID's, which are the UID's of the newly copied 
messages in the target folder. 

=cut

=item delete_message()

The B<delete_message> method accepts a list of arguments. If the I<Uid> parameter is not set to 
a true value, then each item in the list should be either: 

=over 8

=item >
a message sequence number,

=item >
a comma-separated list of message sequence numbers, 

=item >
a reference to an array of message sequence numbers, or

=back

If the I<Uid> parameter is set to a true value, then each item in the list should be either: 

=over 8

=item >
a message UID, 

=item >
a comma-separated list of UID's, or 

=item >
a reference to an array of message UID's.

=back

The messages identified by the sequence numbers or UID's will be deleted. B<delete_message> returns 
the number of messages it was told to delete. However, since the delete is done by issuing the 
I<+FLAGS.SILENT> option of the STORE IMAP client command, there is no guarantee that the 
delete was successful for every message. In this manner the B<delete_message> method sacrifices 
accuracy for speed. 

If you must have guaranteed results then use the IMAP STORE client command (via the default
method) and use the +FLAGS (\Deleted) option, and then parse your results manually. Eg:


	
	$imap->store($msg_id,'+FLAGS (\Deleted)');
	my @results = $imap->History($imap->Transaction);
	...			# code to parse output goes here



The B<IMAPClient> object must be in I<Selected> status to use the B<delete_message> method. 

B<NOTE:> All the messages identified in the input argument(s) must be in the currently 
selected folder. Failure to comply to this requirement will almost certainly result in the
wrong message(s) being deleted. This would be a crying shame. 

B<NOTE SOME MORE:> In the grand tradition of the IMAP protocol, deleting a message doesn't actually
delete the message. Really. If you want to make sure the message has been deleted, you need to expunge
the folder (via the B<expunge> method, which is implemented via the default method). Or at least B<close> it.
This is generally considered a feature, since after deleting a message, you can change your mind and undelete
it at any time before your B<expunge> or B<close>.

I<See also:> The B<delete> method, to delete a folder, the B<expunge> method, to expunge a folder, the 
B<restore_message> method to undelete a message, and the B<close> method (implemented here via the 
default method) to close a folder.  Oh, and don't forget about RFC2060. 

=cut

=item deny_seeing()

The B<deny_seeing> method accepts a list of one or more message sequence numbers, or a single 
reference to an array of one or more message sequence numbers, as its argument(s). It then unsets 
the "\Seen" flag for those messages. Of course, if the I<Uid> parameter is set to a true value then 
those message sequence numbers had better be unique message id's, but then you already knew that, 
didn't you?

Note that specifying C<$imap->deny_seeing(@msgs)> is just a shortcut for specifying 
C<$imap->unset_flag("Seen",@msgs)>. 

=cut

=item disconnect()

Disconnects the B<IMAPClient> object from the server. Functionally equivalent to the B<logout> 
method.

=cut

=item examine()

The B<examine> method selects a folder in read-only mode and changes the object's state to 
"Selected".  The folder selected via the B<examine> method can be examined but no changes can
be made unless it is first selected via the B<select> method. 

The B<examine> method accepts one argument, which is the name of the folder to select. 

=cut

=item exists()

Accepts one argument, a folder name. Returns true if the folder exists or false if it does
not exist.

=cut

=item expunge()

The B<expunge> method accepts one optional argument, a folder name. It expunges the folder specified as 
the argument, or the currently selected folder if no argument is supplied. 

Although RFC2060 does not permit optional arguments (like a folder name) to the EXPUNGE client command, 
the B<expunge> method does, which is especially interesting given that the B<expunge> method doesn't 
technically exist.  In case you're curious, expunging a folder deletes the messages that you thought 
were already deleted via B<delete_message> but really weren't, which means you have to use a method that 
doesn't exist to delete messages that you thought didn't exist. (Seriously, I'm not making any of 
this stuff up.)

Or you could use the B<close> method, which de-selects as well as expunges and which likewise 
doesn't technically exist. As with any IMAP client command, that fact that these methods don't 
exist will not stop them from working anyway. This is a feature of the B<Mail::IMAPClient> module.
(See L<"Other IMAP Client Commands and the Default Method"> if you still don't believe me.)

=cut

=item fetch()

The B<fetch> method implements the FETCH IMAP client command. It accepts a list of
arguments, which will be converted into a space-delimited list of arguments to the
FETCH IMAP client command. If no arguments are supplied then B<fetch> does a FETCH ALL.
If the I<Uid> parameter is set to a true value then the first argument will be treated as
a UID or list of UID's, which means that the UID FETCH IMAP client command will be run instead
of FETCH. (It would really be a good idea at this point to review RFC2060.) 

If called in array context, B<fetch> will return an array of output lines. The output lines 
will be returned just as they were received from the server, so your script will have to be 
prepared to parse out the bits you want. The only exception to this is literal strings, which 
will be inserted into the output line at the point at which they were encountered (without the 
{nnn} literal field indicator). See RFC2060 for a description of literal fields, and don't blame 
me if you're still confused by them. (I read that rfc just about every day and didn't really 
understand those literal fields for about a year.)

If B<fetch> is called in a scalar context, then a reference to an array (as described above) 
is returned instead of the entire array. 


=cut

=item flags()

The B<flags> method implements the FETCH IMAP client command to list a single message's flags.
It accepts one argument, a message sequence number (or a message UID, if the I<Uid> parameter is
true), and returns an array (or a reference to an array, if called in scalar context) listing the 
flags that have been set. Flag names are provided with leading backslashes.

As of version 1.11, you can supply either a list of message id's or a reference to an array of 
of message id's (which means either sequence number, if the Uid parameter is false, or message
UID's, if the Uid parameter is true) instead of supplying a single message sequence number or UID.
If you do, then the return value will not be an array or array reference; instead, it will be a 
hash reference, with each key being a message sequence number (or UID) and each value being a 
reference to an array of flags set for that message.

For example, if you want to display the flags for every message in the folder where you store 
e-mail related to your plans for world domination, you could do something like this:

	use Mail::IMAPClient;
	my $imap = Mail::IMAPClient->new( Server => $imaphost,
					  User   => $login,
					  Password=> $pass,
					  Uid => 1,		# optional
	);

	$imap->select("World Domination");
	# get the flags for every message in my 'World Domination' folder 
	$flaghash = $imap->flags( scalar($imap->search("ALL"))) ;

	# pump through sorted hash keys to print results:
	for my $k (sort { $flaghash->{$a} <=> $flaghash->{$b} } keys %$flaghash) {
		# print: Message 1: \Flag1, \Flag2, \Flag3
		print "Message $k:\t",join(", ",@{$flaghash->{$k}}),"\n";
	}


=cut

=item folders()

The B<folders> method returns an array listing the available folders. It will only be 
successful if the object is in the B<Authenticated> or B<Selected> states.

=cut

=item has_capability()

Returns true if the IMAP server to which the B<IMAPClient> object is connected has the capability
specified as an argument to B<has_capability>.

=item is_parent()

The B<is_parent> method accepts one argument, the name of a folder. It returns a value
that indicates whether or not the folder has children. The value it returns is either
1) a true value (indicating that the folder has children), 2) 0 if the folder has no
children at this time, or 3) undef if the folder is not permitted to have children.

Eg:

	my $parenthood = $imap->is_parent($folder);
	if (defined($parenthood)) { 
		if ($parenthood) {
			print "$folder has children.\n" ;
		} else {
			print "$folder is permitted children, but has none.\n";
		}
	} else {
		print "$folder is not permitted to have children.\n";
	}


=cut

=item list()

The B<list> method implements the IMAP LIST client command. Arguments are passed to the 
IMAP server as received, separated from each other by spaces. If no arguments are supplied
then the default list command C<tag LIST "" '*'> is issued.

The B<list> method returns an array (or an array reference, if called in a scalar context).
The array is the unaltered output of the LIST command. (If you want your output altered then
see the B<folders> method, above.)

=cut

=item login()

The B<login> method uses the IMAP LOGIN client command (as defined in RFC2060) to log into
the server.  The I<User> and I<Password> parameters must be set before the B<login> method
can be invoked. If successful, the B<login> method returns a pointer to the B<IMAPClient> object 
and sets the object status to I<Authenticated>. If unsuccessful, it returns undef.

=cut

=item logout()

The B<logout> method issues the LOGOUT IMAP client commmand. Since the LOGOUT IMAP client 
command causes the server to end the connection, this also results in the B<IMAPClient> client 
entering the B<Unconnected> state. This method does not, however, destroy the B<IMAPClient> object,
so a program can re-invoke the B<connect> and B<login> methods if it wishes to reestablish
a session later in the program.

=cut

=item lsub()

The B<lsub> method implements the IMAP LSUB client command. Arguments are passed to the 
IMAP server as received, separated from each other by spaces. If no arguments are supplied
then the default lsub command C<tag LSUB "" '*'> is issued.

The B<lsub> method returns an array (or an array reference, if called in a scalar context).
The array is the unaltered output of the LSUB command. If you want an array of subscribed folders
then see the B<subscribed> method, below.

=cut

=item message_count()

The B<message_count> method accepts the name of a folder as an argument and returns the number
of messages in that folder. Internally, it invokes the B<status> method (see above) and 
parses out the results to obtain the number of messages. If you don't supply an argument to 
B<message_count> then it will return the number of messages in the currently selected folder
(assuming of course that you've used the B<select> or B<examine> method to select it instead of
trying something funky).

=cut

=item message_string()

The B<message_string> method accepts a message sequence number (or message UID if I<Uid> is true)
as an argument and returns the message as a string. The returned value contains the entire message 
in one scalar variable, including the message headers. Note that using this method will set the
message's "\Seen" flag as a side effect, unless I<Peek> is set to a true value.

=cut

=item message_to_file()

The B<message_to_file> method accepts a filename or file handle and one or more message sequence numbers 
(or message UIDs if I<Uid> is true) as arguments and places the message string(s) (including RFC822 headers) 
into the file named in the first argument (or prints them to the filehandle, if a filehandle is passed). 
The returned value is true on succes and undef on failure. 

If the first argument is a reference, it is assumed to be an open filehandle and will not be 
closed when the method completes, If it is a file, it is opened in append mode, written to, then closed.

Note that using this method will set the message's "\Seen" flag as a side effect. But you can use 
the B<deny_seeing> method to set it back, or set the I<Peek> parameter to a true value to prevent 
setting the "\Seen" flag at all.

This method currently works by making some basic assumptions about the server's behavior, notably 
that the message text will be returned as a literal string but that nothing else will be. If you have
a better idea then I'd like to hear it.

=cut

=item message_uid()

The B<message_uid> method accepts a message sequence number (or message UID if I<Uid> is true)
as an argument and returns the message's UID. Yes, if I<Uid> is true then it will use the IMAP 
UID FETCH UID client command to obtain and return the very same argument you supplied.  This is an 
IMAP feature so don't complain to me about it.

=cut

=item move()

The B<move> method moves messages from the currently selected folder to the folder specified
in the first argument to B<move>.  If the I<Uid> parameter is not true, then the rest of the arguments 
should be either:

=over 8

=item >
a message sequence number,

=item >
a comma-separated list of message sequence numbers, or

=item >
a reference to an array of message sequence numbers.

=back

If the I<Uid> parameter is true, then the arguments should be:

=over 8

=item >
a message UID,

=item >
a comma-separated list of message UID's, or

=item >
a reference to an array of message UID's.

=back

If the target folder does not exist then it will be created.

If move is sucessful, then it returns a true value. Furthermore, if the B<Mail::IMAPClient> 
object is connected to a server that has the UIDPLUS capability, then the true value will 
be the comma-separated list of UID's for the newly copied messages. The list will be in the 
order in which the messages were moved. (Since B<move> uses the copy method, the messages will 
be moved in numerical order.)

If the move is not successful then B<move> returns undef.

Note that a move really just involves copying the message to the new folder and then setting the I<\Deleted>
flag. To actually delete the original message you will need to run B<expunge> (or B<close>).

=cut

=item namespace()

The namespace method runs the NAMESPACE IMAP command (as defined in RFC 2342). When called in a 
list context, it returns a list of three references. Each reference looks like this:

	[ [ $prefix_1, $separator_1 ] , [ $prefix_2, $separator_2], [ $prefix_n , $separator_n] ]

The first reference provides a list of prefices and separator charactors for the available 
personal namespaces.  The second reference provides a list of prefices and separator charactors 
for the available shared namespaces.  The third reference provides a list of prefices and separator 
charactors for the available public namespaces.

If any of the three namespaces are unavailable on the current server then an 'undef' is returned 
instead of a reference. So for example if shared folders were not supported on the server but 
personal and public namespaces were both available (with one namespace each), the returned value 
might resemble this:  

	( [ "", "/" ] , undef, [ "#news", "." ] ) ;

If the B<namespace> method is called in scalar context, it returns a reference to the 
above-mentioned list of three references, thus creating a single structure that would 
pretty-print something like this:

	$VAR1 = [
			[ 
				[ $user_prefix_1, $user_separator_1 ] , 
				[ $user_prefix_2, $user_separator_2], 
				[ $user_prefix_n , $user_separator_n] 
			] 	,					# or undef
			[ 
				[ $shared_prefix_1, $shared_separator_1 ] , 
				[ $shared_prefix_2, $shared_separator_2], 
				[ $shared_prefix_n , $shared_separator_n] 
			] 	,					# or undef
			[ 
				[ $public_prefix_1, $public_separator_1 ] , 
				[ $public_prefix_2, $public_separator_2], 
				[ $public_prefix_n , $public_separator_n] 
			] 	,					# or undef
	];

Or, to look at our previous example (where shared folders are unsupported) called in scalar context:

	$VAR1 = [
			[
				[
					"" ,
					"/",
				],
			],

			undef, 

			[
				[
					"#news", 		
					"." 
				],
			],
	];

=cut

=item on()

The B<on> method works just like the B<since> method, below, except it returns a list of messages 
whose internal system dates are the same as the date supplied as the argument.
 
=item parse_headers ()

The B<parse_headers> method accepts as arguments a message sequence number and a list of header
fields. It returns a hash reference in which the keys are the header field names (without the colon) 
and the values are references to arrays of values. A picture would look something like this:

   $hashref = $imap->parse_headers(1,"Date","Received","Subject","To");
   $hashref = {
	"Date" 	 	=> [ "Thu, 09 Sep 1999 09:49:04 -0400" ]  ,
        "Received"	=> [ q/
	   	from mailhub ([111.11.111.111]) by mailhost.bigco.com 
	   	(Netscape Messaging Server 3.6)  with ESMTP id AAA527D for 
	   	<bigshot@bigco.com>; Fri, 18 Jun 1999 16:29:07 +0000
	   	/, q/
	   	from directory-daemon by mailhub.bigco.com (PMDF V5.2-31 #38473)
 	   	id <0FDJ0010174HF7@mailhub.bigco.com> for bigshot@bigco.com
 	   	(ORCPT rfc822;big.shot@bigco.com); Fri, 18 Jun 1999 16:29:05 +0000 (GMT)
	   	/, q/
	   	from someplace ([999.9.99.99]) by smtp-relay.bigco.com (PMDF V5.2-31 #38473) 
	        with ESMTP id <0FDJ0000P74H0W@smtp-relay.bigco.com> for big.shot@bigco.com; Fri,
	   	18 Jun 1999 16:29:05 +0000 (GMT)
	   	/] ,
	"Subject" 	=> [ qw/ Help! I've fallen and I can't get up!/ ] ,
	"To"		=> [ "Big Shot <big.shot@bigco.com> ] ,
	} ;

The text in the example for the "Received" array has been formated to make reading the
example easier. The actual values returned are just strings of words separated by spaces and 
with newlines and carriage returns stripped off. The I<Received> header is probably the main reason 
that the B<parse_headers> method creates a hash of lists rather than a hash of values. 

If the second argument to B<parse_headers> is 'ALL' or if it is unspecified then all available headers
are included in the returned hash of lists.

If you're not emotionally prepared to deal with a hash of lists then you can always call the 
B<fetch> method yourself with the appropriate parameters and parse the data out any way you want to.

If the I<Uid> parameter is true then the first argument will be treated as a message UID. If the first
argument is a reference to an array of message sequence numbers (or UID's if Uid is true), then 
B<parse_headers> will be run against each message in the array. In this case the return value is
a hash, in which the key is the message sequence number (or UID) and the value is a reference to
a hash as described above.

An example of using B<parse_headers> to print the date and subject of every message in your smut
folder could look like this:

	use Mail::IMAPClient;
	my $imap = Mail::IMAPClient->new( Server => $imaphost,
					  User   => $login,
					  Password=> $pass,
					  Uid => 1,		# optional
	);

	$imap->select("smut");

	for my $h (	

	 # grab the Subject and Date from every message in my (fictional!) smut folder;
	 # the first argument is a reference to an array listing all messages in the folder
	 # (which is what gets returned by the $imap->search("ALL") method when called in
	 # scalar context) and the remaining arguments are the fields to parse out

	 # The key is the message number, which in this case we don't care about:
	 values %{$imap->parse_headers( scalar($imap->search("ALL")) , "Subject", "Date")}
	) {
		# $h is the value of each element in the hash ref returned from parse_headers,
		# and $h is also a reference to a hash.
		# We'll only print the first occurance of each field because we don't expect more
		# than one Date: or Subject: line per message.
		 print map { "$_:\t$h->{$_}[0]\n"} keys %$h ; 
	}


=cut

=item recent()

The B<recent> method performs an IMAP SEARCH RECENT search against the selected folder and returns
an array of sequence numbers (or UID's, if the I<Uid> parameter is true) of messages that are recent.

=cut

=item recent_count()

The B<recent_count> method accepts as an argument a folder name. It returns the number of recent 
messages in the folder (as returned by the IMAP client command "STATUS folder RECENT"), or undef 
in the case of an error. The B<recent_count> method was contributed by Rob Deker (deker@ikimbo.com).

=cut

=item rename()

The B<rename> method accepts two arguments: the name of an existing folder, and a new name for
the folder. The existing folder will be renamed to the new name using the RENAME IMAP client 
command. B<rename> will return a true value if successful, or undef if unsuccessful.

=cut

=item restore_message()

The B<restore_message> method is used to undo a previous B<delete_message> operation (but not if there
has been an intervening B<expunge> or B<close>).  The B<IMAPClient> object must be in I<Selected> status 
to use the B<restore_message> method.  

The B<restore_message> method accepts a list of arguments. If the I<Uid> parameter is not set to a 
true value, then each item in the list should be either:

=over 8

=item >
a message sequence number,

=item >
a comma-separated list of message sequence numbers,

=item >
a reference to an array of message sequence numbers, or

=back

If the I<Uid> parameter is set to a true value, then each item in the list should be either:

=over 8

=item >
a message UID,

=item >
a comma-separated list of UID's, or

=item >
a reference to an array of message UID's.

=back

The messages identified by the sequence numbers or UID's will have their I<\Deleted> flags cleared,
effectively "undeleting" the messages.  B<restore_message> returns the number of messages it was able 
to restore. 

Note that B<restore_messages> is similar to calling B<unset_flag("\Deleted",@msgs)>, except that 
B<restore_messages> returns a more meaningful value. 

=cut

=item run()

Like Perl itself, the B<Mail::IMAPClient> module is designed to make common things easy and 
uncommon things possible. The B<run> method is provided to make those uncommon things possible.

The B<run> method excepts one or two arguments. The first argument is a string containing 
an IMAP Client command, including a tag and all required arguments. The optional second 
argument is a string to look for that will indicate success. (The default is C</OK.*/>). 
The B<run> method returns an array of output lines from the command, which you are free to parse 
as you see fit.

The B<run> method does not do any syntax checking, other than rudimentary checking for a tag.

When B<run> processes the command, it increments the transaction count and saves the command and 
responses in the History buffer in the same way other commands do. However, it also creates a 
special entry in the History buffer named after the tag supplied in the string passed as the 
first argument. If you supply a numeric value as the tag then you may risk overwriting a previous
transaction's entry in the History buffer.

If you want the control of B<run> but you don't want to worry about the damn tags then see 
L<"tag_and_run">, below.

=cut

=item search()

The B<search> method implements the SEARCH IMAP client command. Any argument supplied to
B<search> is prefixed with a space and appended  to the SEARCH IMAP client command. This
method is another one of those situations where it will really help to have your copy of
RFC2060 handy, since the SEARCH IMAP client command contains a plethora of options and 
possible arguments. 

Remember that if your argument needs quotes around it then you must make sure that the
quotes will be preserved when passing the argument. I.e. use C<qq/"$arg"/> instead of 
C<"$arg">.

The B<search> method returns an array containing sequence numbers of messages that passed the
SEARCH IMAP client command's search criteria. If the I<Uid> parameter is true then the array
will contain message UID's. If B<search> is called in scalar context then a pointer to the array
will be passed, instead of the array itself.

=cut

=item see()

The B<see> method accepts a list of one or more messages sequence numbers, or a single reference to an 
array of one or more message sequence numbers, as its argument(s). It then sets the "\Seen" flag for 
those message(s). Of course, if the I<Uid> parameter is set to a true value then those message 
sequence numbers had better be unique message id's, but then you already knew that, didn't you?

Note that specifying C<$imap->see(@msgs)> is just a shortcut for specifying 
C<$imap->set_flag("Seen",@msgs)>. 

=cut

=item seen()

The B<seen> method performs an IMAP SEARCH SEEN search against the selected folder and returns
an array of sequence numbers of messages that have already been seen (ie their SEEN flag is set).
If the I<Uid> parameter is true then an array of message UID's will be returned instead. If called
in scalar context than a pointer to the array (rather than the array itself) will be returned.

=cut

=item select()

The B<select> method selects a folder and changes the object's state to "Selected".
It accepts one argument, which is the name of the folder to select.

=cut

=item sentbefore()

The B<sentbefore> works just like L<"sentsince">, below, except it searches for messages that were 
sent before the date supplied as an argument to the method.

=cut

=item senton()

The B<senton> works just like L<"sentsince">, below, except it searches for messages that were 
sent on the exact date supplied as an argument to the method.

=cut

=item sentsince()

The B<sentsince> method accepts one argument, a date in either standard perl format (seconds since 
1/1/1970, or as output by L<time|perlfunc/time> and as accepted by L<localtime|perlfunc/localtime>) 
or in the I<date_text> format as defined in RFC2060 (dd-Mon-yyyy, where Mon is the English-language 
three-letter abbreviation for the month). 

It searches for items in the currently selected folder for messages sent since the day whose date 
is provided as the argument. It uses the RFC822 I<Date:> header to determine the I<sentsince> date.

In the case of arguments supplied as a number of seconds, the returned result list will include 
items sent on or after that day, regardless of whether they arrived before the specified time on 
that day. The IMAP protocol does not support searches at a granularity finer than a day, so 
neither do I. On the other hand, the only thing I check for in a I<date_text> argument is that it
matches the pattern C</\d\d-\D\D\D-\d\d\d\d/> (notice the lack of anchors),  so if your server lets you 
add something extra to a I<date_text> string then so will B<Mail::IMAPClient>.

=cut

=item separator()

The B<separator> method returns the character used as a separator character in 
folder hierarchies. On unix-based servers, this is often a forward slash (/). It accepts one
argument, the name of a folder whose hierarchy's separator should be returned. If no folder
name is supplied then the separator for the INBOX is returned, which probably is good enough.

=cut

=item set_flag()

The B<set_flag> method accepts the name of a flag as its first argument and a list of one or more messages
 sequence numbers, or a single reference to an array of one or more message sequence numbers, as its 
next argument(s). It then sets the flag specified for those message(s). Of course, if the I<Uid> 
parameter is set to a true value then those message sequence numbers had better be unique message id's, 
just as you'd expect.

Note that when specifying the flag in question, the preceding backslash (\) is entirely optional. (For you,
that is. B<Mail::IMAPClient> still has remember to stick it in there before passing the command to the 
server. This is in fact so important that the method checks its argument and adds the backslash when necessary, 
which is why you don't have to worry about it overly much.)

=cut

=item setacl()

The B<setacl> method accepts three input arguments, a folder name, a user id (or authentication
identifier, to use the terminology of RFC2086), and an access rights modification string. See
RFC2086 for more information. (This is somewhat experimental and its implementation may change.)

=cut

=item since()

The B<since> method accepts a date in either standard perl format (seconds since 1/1/1970, or
as output by L<perlfunc/time> and as accepted by L<perlfunc/localtime>) or in the I<date_text> 
format as defined in RFC2060 (dd-Mon-yyyy, where Mon is the English-language three-letter 
abbreviation for the month). 
It searches for items in the currently selected folder for messages whose internal dates are on or
after the day whose date is provided as the argument. It uses the internal system date for a 
message to determine if that message was sent since the given date.

In the case of arguments supplied as a number of seconds, the returned result list will include 
items whose internal date is on or after that day, regardless of whether they arrived before the 
specified time on that day. 

=cut

=item size()

The B<size> method accepts one input argument, a sequence number (or message UID if the I<Uid> parameter
is true). It returns the size of the message in the currently selected folder with the supplied 
sequence number (or UID).  The B<IMAPClient> object must be in a I<Selected> state in order to use 
this method.

=cut

=item status()

The B<status> method accepts one argument, the name of a folder (or mailbox, to use
RFC2060's terminology), and returns an array containing the results of running the  IMAP 
STATUS client command against that folder. If additional arguments are supplied then they
are appended to the IMAP STATUS client command string, separated from the rest of the string 
and each other with spaces.

If B<status> is not called in an array context then it returns a reference to an array 
rather than the array itself.

The B<status> method should not be confused with the B<Status> method (with an uppercase 'S'),
which returns information about the B<IMAPClient> object. (See the section labeled 
L<"Status Methods">, below).

=cut

=item subscribed()

The B<subscribed> method works like the B<folders> method, above, except that the returned list (or
array reference, if called in scalar context) contains only the subscribed folders. 

=item tag_and_run()

The B<tag_and_run> method accepts one or two arguments. The first argument is a string containing 
an IMAP Client command, without a tag but with all required arguments. The optional second 
argument is a string to look for that will indicate success (without pattern delimiters). 
The default is C<OK.*>. 

The B<tag_and_run> method will prefix your string (from the first argument) with the next 
transaction number and run the command. It returns an array of output lines from the command, 
which you are free to parse as you see fit. Using this method instead of B<run> (above) will 
free you from having to worry about handling the tags (and from worrying about the side affects of 
naming your own tags).

=cut

=item uidnext()

The B<uidnext> method accepts one argument, the name of a folder, and returns the numeric string
that is the next available message UID for that folder.

=item uidvalidity()

The B<uidvalidity> method accepts one argument, the name of a folder, and returns the numeric string
that is the unique identifier validity value for the folder.

=item unseen()

The B<unseen> method performs an IMAP SEARCH UNSEEN search against the selected folder and returns
an array of sequence numbers of messages that have not yet been seen (ie their SEEN flag is not set).
If the I<Uid> parameter is true then an array of message UID's will be returned instead. If called
in scalar context than a pointer to the array (rather than the array itself) will be returned.

Note that when specifying the flag in question, the preceding backslash (\) is entirely optional.

=cut

=item unseen_count()

The B<unseen_count> method accepts the name of a folder as an argument and returns the number of unseen
messages in that folder. If no folder argument is provided then it returns the number of unseen messages 
in the currently selected Folder.

=item unset_flag()

The B<unset_flag> method accepts the name of a flag as its first argument and a list of one or more 
messages sequence numbers, or a single reference to an array of one or more message sequence numbers, 
as its next argument(s). It then unsets the flag specified for those message(s). Of course, if the I<Uid> 
parameter is set to a true value then those message sequence numbers had better be unique message id's, 
just as you'd expect.

=cut


=item Other IMAP Client Commands and the Default Method()

IMAP Client Commands not otherwise documented have been implemented via an AUTOLOAD hack and use 
a default method.

If a program calls a method that is not defined (or inherited) by the B<IMAPClient> module then
the B<IMAPClient> module will assume that it is an IMAP client command. It will prefix the command
with the next available transaction number (or tag value), and append to it the 
space-delimited list of arguments supplied to the unimplemented method (if any). It will then read lines
of output from the imap session until it finds a line containing  the strings "OK" and 
"Completed", and return an array containing all of the lines of output (or, if called in scalar
context, an array reference). If it finds "BAD" or "NO" instead of "OK" it returns undef.

Eg:		

	$imap->FOO("bar","an example","of the default");


results in:


	"99 FOO bar an example of the default\r\n" 

being sent to the IMAP server (assuming that 99 is the current transaction number).

Notice that we used an uppercase method name "FOO" so as not to conflict with future implementations of that
IMAP command. If you run your script with warnings turned on (always a good idea, at least during testing),
then you will receive warnings whenever you use a lowercase method name that has not been implemented. An 
exception to this is when you use certain common (yet unimplemented) methods that, if ever explicitly implemented,
are guaranteed to behave just like the default method. To date, these are:

=over 8

=item store("$msg[,$msg2,$msg_n]", $flags)

Invoke the STORE IMAP Client command to update a message's status on the server. 

=item copy($msg,$folder)

Copy a message into a folder.

=item subscribe($folder)

Subscribe to a folder

=item create($folder)

Create a new folder.

=item delete($folder)

Delete a folder (and its contents).

=item expunge

Remove messages previously marked as deleted. (This is also documented under L<"object methods">, above.)

=back

B<CAUTION:> Once again, remember to quote your quotes if you want quotes to be part of the 
IMAP command string.     

You can also use the default method to override the behavior of implemented IMAP methods by
changing the case of the method name, preferably to all-uppercase so as not to conflict with
the Class method and accessor method namespace. For example, if you don't want the B<search>
method's behavior (which returns a list of message numbers) but would rather have an array
of raw data returned from your B<search> operation, you can issue the following snippet:
	
	@raw = $imap->SEARCH("SUBJECT","Whatever...");

which is slightly more efficient than the equivalent:

	my @msgs = $imap->search("SUBJECT","Whatever...");

	my @raw = $imap->Results;


=cut

=item Status Methods

There are several object methods that return the status of the object. They can be
used at any time to check the status of an B<IMAPClient> object, but are particularly useful 
for determining the cause of failure when a connection and login are attempted as part of
a single B<new> method invocation. The status methods are:

=over 8

=item 1. History

The B<History> method is almost identical to the B<Results> method. Unlike the B<Results>
method, however, the IMAP command that was issued to create the results being returned is
not included in the returned results.  If called in a scalar context, B<History> returns an 
array reference rather than an array.

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

=item 6. LastError

Internally B<LastError> is implemented just like a parameter (as described in L<"Parameters">, above). 
There is a I<LastError> attribute and an eponymous accessor method which returns the I<LastError> text 
string describing the last error condition encountered by the server. 

Note that some errors are more serious than others, so I<LastError>'s value is only meaningful 
if you encounter an error condition that you don't like. For example, if you use the B<exists> 
method to see if a folder exists and the folder does not exist, then an error message will be 
recorded in LastError even though this is not a particularly serious error. On the other hand, 
if you didn't use B<exists> and just tried to B<select> a non-existing folder, then B<select> 
would return undef after setting I<LastError> to something like "NO SELECT failed: Can't open 
mailbox <mailbox>: no such mailbox".  At this point it would be useful to print out the contents 
of I<LastError> as you B<die>.

=item 7. LastIMAPCommand

New in version 2.0.4, B<LastIMAPCommand> returns the exact IMAP command string to be sent to the 
server. Useful mainly in constructing error messages when B<LastError> just isn't enough.

=item 8. Report

The B<Report> method returns an array containing a history of the IMAP session up to the
point that B<Report> was called. It is primarily meant to assist in debugging but can also
be used to retrieve raw output for manual parsing. The value of the B<Clear> parameter controls
how many transactions are in the report. (See the discussion of B<Clear> in 
L<"parameters">, above.)

=cut

=item 9. Results

The B<Results> method returns an array containing the results of one IMAP client command. 
It accepts one argument, the transaction number of the command whose results are to be 
returned. If transaction number is unspecified then B<Results> returns the results of the 
last IMAP client command issued. If called in a scalar context, B<Results> returns an 
array reference rather than an array.

=cut

=item 10. State 

The B<State> method returns a numerical value that indicates the current status of the B<IMAPClient> 
object. If invoked with an argument, it will set the object's state to that value. If invoked without 
an argument, it behaves just like B<Status>, below. 

Normally you will not have to invoke this function. An exception is if you are bypassing the
B<Mail::IMAPClient> module's B<connect> and/or B<login> modules to set up your own connection
(say, for example, over a secure socket), in which case you must manually do what the B<connect>
and B<login> methods would otherwise do for you.

=item 11. Status 

The B<Status> method returns a numerical value that indicates the current status of the B<IMAPClient> 
object. (Not to be confused with the B<status> method, all lower-case, which is the implementation of the
STATUS IMAP client command.)

=item 12. Transaction 

The B<Transaction> method returns the tag value (or transaction number) of the last IMAP client command.

=back

=back

=cut

=head1 REPORTING BUGS

Please feel free to e-mail the author at the below address if you encounter any strange behaviors. 
When reporting a bug, please be sure to include the following:

- As much information about your environment as possible. I especially need to know which version 
of Mail::IMAPClient you are running and the type/version of IMAP server to which you are connecting.

- As detailed a description of the problem as possible. (What are you doing? What happens?)

- An example script that demonstrates the problem (preferably with as few lines of code as possible!) 
and which calls the Mail::IMAPClient's B<new> method with the I<Debug> parameter set to "1".

- Output from the example script when it's running with the Debug parameter turned on. You can 
edit the output to remove (or preferably to "X" out) sensitive data, such as hostnames, user 
names, and passwords, but PLEASE do not remove the text that identifies the TYPE of IMAP server 
to which you are connecting. Note that in the latest versions of Mail::IMAPClient, debugging does
not print out the user or password from the login command line. However, if you use some other means
of authenticating then you may need to edit the debugging output with an eye to security.

- Don't be surprised if I come back asking for a trace of the problem. To provide this, you should create
a file called I<.perldb> in your current working directory and include the following line of text in 
that file:

C<&parse_options("NonStop=1 LineInfo=mail_imapclient_db.out");>

For your debugging convenience, a sample .perldb file, which was randomly assigned the name 
F<sample.perldb>, is provided in the distribution.

Next, without changing your working directory, debug the example script like this:
C<perl -d example_script.pl [ args ]>

Note that in these examples, the script that demonstrates your problem is named "example_script.pl"
and the trace output will be saved in "mail_imapclient_db.out". You should change these to suite 
your needs.

=head1 AUTHOR

	David J. Kernen
	The Kernen Consulting Group, Inc
	david.kernen@erols.com

=cut

=head1 COPYRIGHT

                       Copyright 1999, 2000 The Kernen Group, Inc.
                            All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the terms of either:

=over 4

=item a) the "Artistic License" which comes with this Kit, or

=item b) the GNU General Public License as published by the Free Software Foundation; either 
version 1, or (at your option) any later version.

=back

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either
the GNU General Public License or the Artistic License for more details.

=cut

my $not_void_context = '0 but true'; 		# return true value

# $Log: IMAPClient.pm,v $
# Revision 20001010.7  2000/12/20 19:36:52  dkernen
#
# ---------------------------------------------------------------------------------
# Modified Files: IMAPClient.pm -- added bug fix to I/O engine, also cleaned up doc
# 		Changes	      -- documented same
# ---------------------------------------------------------------------------------
#
# Revision 20001010.6  2000/11/10 22:08:06  dkernen
#
# Modified Files: Changes IMAPClient.pm Makefile t/basic.t -- to add Peek parm and to make several bug fixes
#
# Revision 20001010.5  2000/10/30 21:04:51  dkernen
#
# Modified Files: 	IMAPClient.pm -- updated version to 2.0.2.
#
# Revision 20001010.4  2000/10/30 21:03:50  dkernen
#
# Modified Files: Changes IMAPClient.pm  -- to update documentation
#
# Revision 20001010.3  2000/10/30 18:40:02  dkernen
#
# Modified Files: Changes IMAPClient.pm INSTALL MANIFEST Makefile README test.txt  -- for 2.0.1
#
# Revision 20001010.2  2000/10/27 14:43:25  dkernen
#
# Modified Files: Changes IMAPClient.pm Todo -- major rewrite of I/O et al.
#
# Revision 20001010.1  2000/10/10 17:32:15  dkernen
#
# ----------------------------------------------------------------------
# Tag: 	v1_19 rev 20001010.1
# Modified Files:
# 	.nstest .uwtest45 .uwtest47 Artistic Changes Copying
# 	IMAPClient.pm INSTALL MANIFEST Makefile Makefile.PL README
# 	Todo pm_to_blib test.txt test_template.txt
# See Changes for a list of changes in v1.19
# ----------------------------------------------------------------------
#
# Revision 19991216.17  2000/07/10 20:54:02  dkernen
#
# Modified Files: Changes IMAPClient.pm MANIFEST Makefile README
#
# Revision 19991216.16  2000/06/23 19:07:53  dkernen
#
# Modified Files:
# 	Changes IMAPClient.pm Makefile test.txt  -- for v1.16
#
# Revision 19991216.15  2000/06/21 21:07:37  dkernen
#
# Modified Files: Changes IMAPClient.pm Makefile
#
# Revision 19991216.14  2000/05/22 20:10:25  dkernen
#
# Modified Files: Changes IMAPClient.pm
#
# Revision 19991216.13  2000/04/27 17:59:45  dkernen
#
# Modified Files:
# 	Changes IMAPClient.pm INSTALL MANIFEST Makefile README
# 	test.txt
# Added Files:
# 	.uwtest45 .uwtest47
# Removed Files:
# 	.uwtest
#
# Revision 19991216.12  2000/03/16 14:55:23  dkernen
#
# Changes 	- to document changes
# IMAPClient.pm 	- to add lsub and subscribed methods
# Makefile	- to increment version to 1.10
#
# Revision 19991216.11  2000/03/09 20:32:28  dkernen
#
# Modified Files:
# 		IMAPClient.pm  	-- to fix bugs in recent, seen, and unseen
# 		Changes 	-- to document same
#
# Revision 19991216.10  2000/03/09 14:05:18  dkernen
#
# Modified Files:
# 	Changes -- to document changes
# 	IMAPClient.pm -- to add new methods since, sentsince, before, on, senton, sentbefore
# 			 and to fix bug in i/o engine
# 	INSTALL -- to enhance installation documentation
# 	README -- to add new platforms to list of tested platforms
# 	Todo -- to update entries with current status
#
# Revision 19991216.9  2000/03/02 19:56:06  dkernen
#
# Modified Files:
# Changes -- to document changes
# IMAPClient.pm -- to add run and run_and_tag methods and to add documentation, also to
# 	fix bug in folders method when optional arg is supplied
# INSTALL -- to improve install instructions
# README --  to add to list of IMAP servers against which this module has been used successfully
#
# Revision 19991216.8  2000/01/12 18:58:00  dkernen
# *** empty log message ***
#
# Revision 19991216.7  2000/01/12 17:49:40  dkernen
#
# Modified Files: 	Changes IMAPClient.pm  -- to fix problem in subscribe
# method when subscribing to folders with embedded spaces in name and updated
# change log to document same
#
# Revision 19991216.6  1999/12/28 13:56:35  dkernen
# Created v1.08. Added acl methods.
#
# Revision 19991216.5  1999/12/16 17:18:32  dkernen
# Bring up to same level
#
# Revision 19991216.2  1999/12/16 17:17:17  dkernen
# Bring up to same level
#
# Revision 19991216.1  1999/12/16 17:16:16  dkernen
# Bring up to same level
#
# Revision 19991129.6  1999/12/16 17:13:56  dkernen
# Incorporate changes for exists method performance enhancement
#
# Revision 19991129.5  1999/12/15 21:39:07  dkernen
# Made performance enhancements to login and exists methods.
#
# Revision 19991129.4  1999/12/10 21:51:05  dkernen
# Fix my brain-damaged select operator.
#
# Revision 19991129.3  1999/12/07 16:13:23  dkernen
# Added timeout feature to IMAPClient.pm and documented same in pod,
# fixed bug in Makefile.PL when specifying PREFIX directive,
# and updated change log accordingly.
#
# Revision 19991129.2  1999/12/01 22:11:03  dkernen
# Enhance support for UID and add tests to t/basic for same
#
# Revision 19991129.1  1999/11/30 20:36:24  dkernen
# Updated DESTROY to turn off spurious warnings
#
# Revision 19991129.0  1999/11/30 15:50:02  dkernen
# Incorporated bug fixes from Scott Wilson
#

