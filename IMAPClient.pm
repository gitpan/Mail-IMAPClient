package Mail::IMAPClient;

# $Id: IMAPClient.pm,v 20001010.12 2002/08/23 14:33:40 dkernen Exp $

$Mail::IMAPClient::VERSION = '2.2.1';
$Mail::IMAPClient::VERSION = '2.2.1';  	# do it twice to make sure it takes

use Fcntl qw(F_GETFL F_SETFL O_NONBLOCK);
use Socket();
use IO::Socket();
use IO::Select();
use IO::File();
use Carp qw(carp);
#use Data::Dumper;
use Errno qw/EAGAIN/;


#print "Found Fcntl in $INC{'Fcntl.pm'}\n";
#Fcntl->import;

=head1 NAME 

Mail::IMAPClient - An IMAP Client API

=cut

use constant Unconnected => 0;
use constant Connected         => 1;         	# connected; not logged in
use constant Authenticated => 2;      		# logged in; no mailbox selected
use constant Selected => 3;   		        # mailbox selected
use constant INDEX => 0;              		# Array index for output line number
use constant TYPE => 1;               		# Array index for output line type (OUTPUT,INPUT, or LITERAL)
use constant DATA => 2;                       	# Array index for output line data

my %SEARCH_KEYS = map { ( $_ => 1 ) } qw/
	ALL ANSWERED BCC BEFORE BODY CC DELETED DRAFT FLAGGED
	FROM HEADER KEYWORD LARGER NEW NOT OLD ON OR RECENT
	SEEN SENTBEFORE SENTON SENTSINCE SINCE SMALLER SUBJECT
	TEXT TO UID UNANSWERED UNDELETED UNDRAFT UNFLAGGED 
	UNKEYWORD UNSEEN
/;

sub _debug {
	my $self = shift;
	return unless $self->Debug;
	my $fh = $self->{Debug_fh} || \*STDERR; 
	print $fh @_;
}

sub MaxTempErrors {
	my $self = shift;
	$_[0]->{Maxtemperrors} = $_[1] if defined($_[1]);
	return $_[0]->{Maxtemperrors};
}

# This function is used by the accessor methods
#
sub _do_accessor {
  my $datum = shift;

  if ( defined($_[1]) and $datum eq 'Fast_io' and ref($_[0]->{Socket})) {
    if ($_[1]) {                      # Passed the "True" flag
      my $fcntl = 0;
      eval { $fcntl=fcntl($_[0]->{Socket}, F_GETFL, 0) } ;
      if ($@) {
      $_[0]->{Fast_io} = 0;
      carp ref($_[0]) . " not using Fast_IO; not available on this platform"
        if ( ( $^W or $_[0]->Debug) and not $_[0]->{_fastio_warning_}++);
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
    chomp $@ if $datum eq 'LastError';
    return $_[0]->{$datum} = $_[1] ;
  } else {
    return $_[0]->{$datum};
  }
}

# the following for loop sets up eponymous accessor methods for 
# the object's parameters:

BEGIN {
 for my $datum (
		qw( 	State Port Server Folder Fast_io Peek
			User Password Socket Timeout Buffer
			Debug LastError Count Uid Debug_fh Maxtemperrors
			EnableServerResponseInLiteral
		)
 ) {
        no strict 'refs';
        *$datum = sub { _do_accessor($datum, @_); };
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
		(my $string = $_[0]) =~ s/\x0d\x0a/\x0a/gm;
		return $string;
	}
	return wantarray ?     	map { s/\x0d\x0a/\0a/gm ; $_ }  (ref($_[0]) ? @{$_[0]}  : @_)  : 
				[ map { s/\x0d\x0a/\x0a/gm ; $_ } ref($_[0]) ? @{$_[0]} : @_ ] ;
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
	$self->Maxtemperrors or $self->Maxtemperrors("unlimited") ;
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
                      next unless $o->[TYPE] eq "OUTPUT";
                      ($code) = $o->[DATA] =~ /^\*\s+(OK|BAD|NO)/i  ;
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
			$self->Massage($self->Password,1) ;
	$self->_imap_command($string) 
		and $self->State(Authenticated);
	# $self->folders and $self->separator unless $self->NoAutoList;
	unless ( $self->IsAuthenticated) {
		my($carp) 	=  $self->LastError;
		$carp 		=~ s/^[\S]+ ([^\x0d\x0a]*)\x0d?\x0a/$1/;
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

sub sort {
    my $self = shift;
    my @hits;
    my @a = @_;
    $@ = "";
    $a[0] = "($a[0])" unless $a[0] =~ /^\(.*\)$/;      # wrap criteria in parens
    $self->_imap_command( ( $self->Uid ? "UID " : "" ) . "SORT ". join(' ',@a))
         or return wantarray ? @hits : \@hits ;
    my @results =  $self->History($self->Count);

    for my $r (@results) {
        chomp $r;
        $r =~ s/\r$//;
        $r =~ s/^\*\s+SORT\s+// or next;   
        push @hits, grep(/\d/,(split(/\s+/,$r)));
    }
    return wantarray ? @hits : \@hits;     
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
                              [ map { $_->[DATA] } @{$self->{'History'}{$self->Count}} ]      ;
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
                              [ map { $_->[DATA] } @{$self->{'History'}{$self->Count}}        ] ;
}

sub subscribed {
        my $self = shift;
	my $what = shift ;

        my @folders ;  

	my @list = $self->lsub(undef,( $what? "$what" . $self->separator($what) . "*" : undef ) );
	push @list, $self->lsub(undef, $what) if $what and $self->exists($what) ;

      # my @list = map { $self->_debug("Pushing $_->[${\(DATA)}] \n"); $_->[DATA] } @$output;

	my $m;

	for ($m = 0; $m < scalar(@list); $m++ ) {
		if ($list[$m] && $list[$m]  !~ /\x0d\x0a$/ ) {
			$list[$m] .= $list[$m+1] ;
			$list[$m+1] = "";	
		}
			
		
		# $self->_debug("Subscribed: examining $list[$m]\n");

		push @folders, $1||$2 
			if $list[$m] =~
                        /       ^\*\s+LSUB               # * LSUB
                                \s+\([^\)]*\)\s+         # (Flags)
                                (?:"[^"]*"|NIL)\s+	 # "delimiter" or NIL
                                (?:"([^"]*)"|(.*))\x0d\x0a$  # Name or "Folder name"
                        /ix;

        } 

        # for my $f (@folders) { $f =~ s/^\\FOLDER LITERAL:://;}
	my @clean = () ; my %memory = (); 
	foreach my $f (@folders) { push @clean, $f unless $memory{$f}++ }
        return wantarray ? @clean : \@clean ;
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
                              [ map {$_->[DATA] } @{$self->{'History'}{$self->Count}}] ;
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
        return wantarray?$self->History($self->Count):[map{$_->[DATA]}@{$self->{'History'}{$self->Count}}];
}


sub getacl {
        my $self = shift;
        my ($target) = @_;
        $target = $self->Folder unless defined($target);
        my $mtarget           = $self->Massage($target);
        my $string      =  qq(GETACL $mtarget);
        $self->_imap_command($string)  or return undef;
	my @history = $self->History($self->Count);
	#$self->_debug("Getacl history: " . join("|",@history) . ">>>End of History<<<" ) ;
	my $perm = ""; 
	my $hash = {};
	for ( my $x = 0; $x < scalar(@history) ; $x++ ) {
        	if ( $history[$x] =~ /^\* ACL/ ) {
			
			$perm = $history[$x] =~ /^\* ACL $/ ? $history[++$x].$history[++$x] : 
				$history[$x];		
			$perm =~ s/\s?\x0d\x0a$//;
			piece:  until ( $perm =~ /\Q$target\E"?$/ or !$perm) {
				#$self->_debug(qq(Piece: permline=$perm and pattern = /\Q$target\E"? \$/));
				$perm =~ s/\s([^\s]+)\s?$// or last piece;
				my($p) = $1;
				$perm =~ s/\s([^\s]+)\s?$// or last piece;
				my($u) = $1;
				$hash->{$u} = $p;
				$self->_debug("Permissions: $u => $p \n");
			}
		
		}
	}
        return $hash;
}

sub listrights {
	my $self = shift;
	my ($target, $user) = @_;
	$user = $self->User unless defined($user);
	$target = $self->Folder unless defined($target);
	$target 	  = $self->Massage($target);
	$user		  =~ s/^"(.*)"$/$1/;
	$user 	  	  =~ s/"/\\"/g;
	my $string 	=  qq(LISTRIGHTS $target "$user");
	$self->_imap_command($string)  or return undef;
	my $resp = ( grep(/^\* LISTRIGHTS/, $self->History($self->Count) ) )[0];
	my @rights = split(/\s/,$resp);	
	shift @rights, shift @rights, shift @rights, shift @rights;
	my $rights = join("",@rights);
	$rights =~ s/"//g;	
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
	return undef unless(defined $expected_size);	# unable to get size
	my $cmd  =  	$self->has_capability('IMAP4REV1') 				? 
				"BODY" . ( $self->Peek ? '.PEEK[]' : '[]' ) 		: 
				"RFC822" .  ( $self->Peek ? '.PEEK' : ''  )		;

	$self->fetch($msg,$cmd) or return undef;
	
	my $string = "";

	foreach my $result  (@{$self->{"History"}{$self->Transaction}}) { 
              $string .= $result->[DATA] if defined($result) and $self->_is_literal($result) ;
	}      
	# BUG? should probably return undef if length != expected
	if ( length($string) != $expected_size ) { 
		carp "${self}::message_string: expected $expected_size bytes but received " . 
		length($string) 
			if $self->Debug or $^W; 
	}
	if ( length($string) > $expected_size ) { $string = substr($string,0,$expected_size) }
	if ( length($string) < $expected_size ) {
		$self->LastError("${self}::message_string: expected $expected_size bytes but received " . 
			length($string)."\n");
		return undef;
	}
	return $string;
}

sub bodypart_string {
	my($self, $msg, $partno, $bytes, $offset) = @_;

	unless ( $self->has_capability('IMAP4REV1') ) {
		$self->LastError(
				"Unable to get body part; server " . 
				$self->Server . 
				" does not support IMAP4REV1"
		);
		return undef;
	}
	my $cmd = "BODY" . ( $self->Peek ? ".PEEK[$partno]" : "[$partno]" ) 	;
	$offset ||= 0 ;
	$cmd .= "<$offset.$bytes>" if $bytes;

	$self->fetch($msg,$cmd) or return undef;
	
	my $string = "";

	foreach my $result  (@{$self->{"History"}{$self->Transaction}}) { 
              $string .= $result->[DATA] if defined($result) and $self->_is_literal($result) ;
	}      
	return $string;
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
		binmode $handle;	# For those of you who need something like this...
	} 

        my $clear = $self->Clear;
	my $cmd = $self->Peek ? 'BODY.PEEK[]' : 'BODY[]';
	$cmd = $self->Peek ? 'RFC822.PEEK' : 'RFC822' unless $self->imap4rev1;
	
	my $string = ( $self->Uid ? "UID " : "" ) . "FETCH " . join(",",@msgs) . " $cmd";

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

        my $trans       = $self->Count($self->Count+1);

        $string         = "$trans $string" ;

        $self->_record($trans,[ 0, "INPUT", "$string\x0d\x0a"] );

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
                        # $self->_debug("Received from readline: ${\($o->[DATA])}<<END OF RESULT>>\n");
                        next unless $self->_is_output($o);
                        ($code) = $o->[DATA] =~ /^$trans (OK|BAD|NO)/mi ;
                        if ($o->[DATA] =~ /^\*\s+BYE/im) {
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

sub original_migrate {
	my($self,$peer,$msgs,$folder) = @_;
	unless ( eval { $peer->IsConnected } ) {
		$self->LastError("Invalid or unconnected " .  ref($self). 
				 " object used as target for migrate." );
		return undef;
	}
	unless ($folder) {
		$folder = $self->Folder;
		$peer->exists($folder) 		or 
			$peer->create($folder) 	or 
			(
				$self->LastError("Unable to created folder $folder on target mailbox: ".
					"$peer->LastError") and 
				return undef 
			) ;
	}			
	if ( $msgs =~ /^all$/i ) { $msgs = $self->search("ALL") }
	foreach my $mid ( ref($msgs) ? @$msgs : $msgs ) {
		my $uid = $peer->append($folder,$self->message_string($mid));
		$self->LastError("Trouble appending to peer: " . $peer->LastError . "\n");
	}
}


sub migrate {

	my($self,$peer,$msgs,$folder) 	= @_;
	my($toSock,$fromSock) 		= ( $peer->Socket, $self->Socket);
	my $bufferSize 			= $self->Buffer || 4096;
	my $fromBuffer 			= "";
	my $clear 			= $self->Clear;

	unless ( eval { $peer->IsConnected } ) {
		$self->LastError("Invalid or unconnected " . 
			ref($self) . " object used as target for migrate. $@");
		return undef;
	}

	unless ($folder) {
		$folder = $self->Folder 	or
			$self->LastError( "No folder selected on source mailbox.") 
			and return undef;

		$peer->exists($folder)		or 
			$peer->create($folder)	or 
			(
				$self->LastError(
				  "Unable to create folder $folder on target mailbox: ".
				  $peer->LastError . "\n"
				) and return undef 
			) ;
	}
	$msgs or $msgs eq "0" or $msgs = "all";	
	if ( $msgs =~ /^all$/i ) { $msgs = $self->search("ALL") }
	$self->_debug("Migrating the following msgs from $folder: " . 
		( ref($msgs) ? join(", ",@$msgs) : $msgs) );

	MIGMSG:	foreach my $mid ( ref($msgs) ? @$msgs : (split(/,\s*/,$msgs)) ) {
		# Set up counters for size of msg and portion of msg remaining to
		# process:
		$self->_debug("Migrating message $mid in folder $folder\n") 
			if $self->Debug;
		my $leftSoFar = my $size = $self->size($mid);

		# fetch internaldate and flags of original message:
		my $intDate = '"' . $self->internaldate($mid) . '"' ;
		my $flags   = "(" . join(" ",grep(!/\\Recent/i,$self->flags($mid)) ) . ")" ;

		# set up transaction numbers for from and to connections:
		my $trans       = $self->Count($self->Count+1);
		my $ptrans      = $peer->Count($peer->Count+1);

		# If msg size is less than buffersize then do whole msg in one 
		# transaction:
		if ( $size <= $bufferSize ) {
			my $new_mid = $peer->append($folder,$self->message_string($mid) ) ;
		        $self->_debug("Copied message $mid in folder $folder to " . $peer->User .
				    '@' . $peer->Server . ". New Message UID is $new_mid.\n" 
		        ) if $self->Debug;

		        $peer->_debug("Copied message $mid in folder $folder from " . $self->User .
				    '@' . $self->Server . ". New Message UID is $new_mid.\n" 
		        ) if $peer->Debug;


			next MIGMSG;
		}

		# otherwise break it up into digestible pieces:
		my ($cmd, $pattern);
		if ( $self->imap4rev1 ) {
			# imap4rev1 supports FETCH BODY 
			$cmd = $self->Peek ? 'BODY.PEEK[]' : 'BODY[]';
			$pattern = sub {
                                #$self->_debug("Data fed to pattern: $_[0]<END>\n");
                                $_[0] =~ /\(.*BODY\[\]<\d+> \{(\d+)\}/i 	; # ;-)
					# or $self->_debug("Didn't match pattern\n") ; 
                                #$self->_debug("Returning from pattern: $1\n") if defined($1);
				return $1 ;
                        } ;
		} else {
			# older imaps use (deprecated) FETCH RFC822:
			$cmd = $self->Peek ? 'RFC822.PEEK' : 'RFC822' ;
			$pattern = sub {
				shift =~ /\(RFC822\[\]<\d+> \{(\d+)\}/i; 
				return $1 ;
			};
		}


		# Now let's warn the peer that there's a message coming:

		my $pstring = 	"$ptrans APPEND $folder " .  
				"$flags $intDate {" . $size . "}"  ;

		$peer->_debug("About to issue APPEND command to peer for msg $mid\n")
			if $peer->Debug;

		my $feedback2 = $peer->_send_line( $pstring ) ;

		$peer->_record($ptrans,[ 
			0, 
			"INPUT", 
			"$pstring" ,
		] ) ;
		unless ($feedback2) {
		   $self->LastError("Error sending '$pstring' to target IMAP: $!\n");
		   return undef;
		}
		# Get the "+ Go ahead" response:
		my $code = 0;
		until ($code eq '+' or $code =~ /NO|BAD|OK/ ) {
	  	  my $readSoFar = 0 ;
		  $readSoFar += sysread($toSock,$fromBuffer,1,$readSoFar)||0
			until $fromBuffer =~ /\x0d\x0a/;

		  $peer->_debug("migrate: response from target server: $fromBuffer<END>\n")
			if $peer->Debug;

		  ($code)= $fromBuffer =~ /^(\+)/ ;
		  $code ||=0;

		  #$peer->_debug( "$folder: received $fromBuffer from server\n") 
		  #if $self->Debug;

	  	  # ... and log it in the history buffers
		  $self->_record($trans,[ 
			0, 
			"OUTPUT", 
			"Mail::IMAPClient migrating message $mid to $peer->User\@$peer->Server"
		  ] ) ;
		  $peer->_record($ptrans,[ 
			0, 
			"OUTPUT", 
			$fromBuffer
		  ] ) ;


		}
		unless ( $code eq '+'  ) {
			$^W and warn "$@\n";
			$self->Debug and $self->_debug("Error writing to target host: $@\n");
			next MIGMSG;	
		}
		# Here is where we start sticking in UID if that parameter
		# is turned on:	
		my $string = ( $self->Uid ? "UID " : "" ) . "FETCH $mid $cmd";

		# Clean up history buffer if necessary:
		$self->Clear($clear)
			if $self->Count >= $clear and $clear > 0;


	   # position will tell us how far from beginning of msg the
	   # next IMAP FETCH should start (1st time start at offet zero):
	   my $position = 0;
	   #$self->_debug("There are $leftSoFar bytes left versus a buffer of $bufferSize bytes.\n");
	   my $chunkCount = 0;
	   while ( $leftSoFar > 0 ) {
		$self->_debug("Starting chunk " . ++$chunkCount . "\n");

		my $newstring         ="$trans $string<$position."  .
					( $leftSoFar > $bufferSize ? $bufferSize : $leftSoFar ) . 
					">" ;

		$self->_record($trans,[ 0, "INPUT", "$newstring\x0d\x0a"] );
		$self->_debug("Issuing migration command: $newstring\n" )
			if $self->Debug;;

		my $feedback = $self->_send_line("$newstring");

		unless ($feedback) {
		   $self->LastError("Error sending '$newstring' to source IMAP: $!\n");
		   return undef;
		}
		my $chunk = "";
		until ($chunk = $pattern->($fromBuffer) ) {
		   $fromBuffer = "" ;
	    	   until ( $fromBuffer=~/\x0d\x0a$/ ) {
	    	   	sysread($fromSock,$fromBuffer,1,length($fromBuffer)) ; 
			#$self->_debug("migrate chunk $chunkCount:" . 
			#	"Read from source: $fromBuffer<END>\n");
		   }
		   
		   $self->_record($trans,[ 0, "OUTPUT", "$fromBuffer"] ) ;

		   if ( $fromBuffer =~ /^$trans (?:NO|BAD)/ ) {
			$self->LastError($fromBuffer) ;
			next MIGMSG;
		   }

		   if ( $fromBuffer =~ /^$trans (?:OK)/ ) {
			$self->LastError("Unexpected good return code " .
				"from source host: " . $fromBuffer) ;
			next MIGMSG;
		   }

		}
		$fromBuffer = "";
		my $readSoFar = 0 ;
		$readSoFar += sysread($fromSock,$fromBuffer,$chunk-$readSoFar,$readSoFar)||0
			until $readSoFar >= $chunk;
		#$self->_debug("migrateRead: chunk=$chunk readSoFar=$readSoFar " .
		#	"Buffer=$fromBuffer<END_OF_BUFFER\n") if $self->Debug;

		my $wroteSoFar = 0;
		until ( $wroteSoFar >= $chunk ) {
			#$peer->_debug("Chunk $chunkCount: Next write will attempt to write " .
			#	"this substring:\n" .
			#	substr($fromBuffer,$wroteSoFar,$chunk-$wroteSoFar) .
			#	"<END_OF_SUBSTRING>\n"
			#);
			$wroteSoFar += syswrite(
					$toSock,
					$fromBuffer,
					$chunk - $wroteSoFar, 
					$wroteSoFar )||0
			until $wroteSoFar >= $readSoFar;
			$peer->_debug("Chunk $chunkCount: Wrote $wroteSoFar bytes (out of $chunk)\n");
		}
		$position += $readSoFar ;
		$leftSoFar -= $readSoFar;
		$fromBuffer = "";
		# Finish up reading the server response from the fetch cmd
		# 	on the source system:
		{
		my $code = 0;
		until ( $code)  {
			# escape infinite loop if read_line never returns any data:
			$self->_debug("Reading from source server; expecting ') OK' type response\n")
				if $self->Debug;
			$output = $self->_read_line or return undef; 
			for my $o (@$output) {

				$self->_record($trans,$o);      # $o is a ref

				# $self->_debug("Received from readline: " .
				# "${\($o->[DATA])}<<END OF RESULT>>\n");

				next unless $self->_is_output($o);

				($code) = $o->[DATA] =~ /^$trans (OK|BAD|NO)/mi ;

				if ($o->[DATA] =~ /^\*\s+BYE/im) {
					$self->State(Unconnected);
					return undef ;
				}
	   		}
	   	}
	   	} # end scope for my $code
	   }
	   # Now let's send a <CR><LF> to the peer to signal end of APPEND cmd:
	   {
	    my $wroteSoFar = 0;
	    $fromBuffer = "\x0d\x0a";
	    $wroteSoFar += syswrite($toSock,$fromBuffer,2-$wroteSoFar,$wroteSoFar)||0 
	    		until $wroteSoFar >= 2;

	   }
	   # Finally, let's get the new message's UID from the peer:
	   my $new_mid = "";
           {
                my $code = 0;
                until ( $code)  {
                        # escape infinite loop if read_line never returns any data:
			$peer->_debug("Reading from target: expecting new uid in response\n")
				if $peer->Debug;
                        $output = $peer->_read_line or next MIGMSG;
                        for my $o (@$output) {

                                $peer->_record($ptrans,$o);      # $o is a ref

                                # $peer->_debug("Received from readline: " .
                                # "${\($o->[DATA])}<<END OF RESULT>>\n");

                                next unless $peer->_is_output($o);

                                ($code) = $o->[DATA] =~ /^$ptrans (OK|BAD|NO)/mi ;
				($new_mid)= $o->[DATA] =~ /APPENDUID \d+ (\d+)/ if $code;
				#$peer->_debug("Code line: " . $o->[DATA] . 
				#	"\nCode=$code mid=$new_mid\n" ) if $code;

                                if ($o->[DATA] =~ /^\*\s+BYE/im) {
                                        $peer->State(Unconnected);
                                        return undef ;
                                }
                        }
			$new_mid||="unknown" ;
                }
             } # end scope for my $code

	     $self->_debug("Copied message $mid in folder $folder to " . $peer->User .
			    '@' . $peer->Server . ". New Message UID is $new_mid.\n" 
	     ) if $self->Debug;

	     $peer->_debug("Copied message $mid in folder $folder from " . $self->User .
			    '@' . $self->Server . ". New Message UID is $new_mid.\n" 
	     ) if $peer->Debug;


	  # ... and finish up reading the server response from the fetch cmd
	  # 	on the source system:
	      # {
	#	my $code = 0;
	#	until ( $code)  {
	#		# escape infinite loop if read_line never returns any data:
        #      		unless ($output = $self->_read_line ) {
	#			$self->_debug($self->LastError) ;
	#			next MIGMSG;
	#		}
	#		for my $o (@$output) {
#
#				$self->_record($trans,$o);      # $o is a ref
#
#				# $self->_debug("Received from readline: " .
#				# "${\($o->[DATA])}<<END OF RESULT>>\n");
#
#				next unless $self->_is_output($o);
#
#			 	($code) = $o->[DATA] =~ /^$trans (OK|BAD|NO)/mi ;
#
#			      	if ($o->[DATA] =~ /^\*\s+BYE/im) {
#					$self->State(Unconnected);
#					return undef ;
#				}
#			}
#		}
#		}
		
	     	# and clean up the I/O buffer:
	     	$fromBuffer = "";
	     }
	return $self;	
}

#sub old_body_string {
#     my $self = shift;
#     my $msg  = shift;
#     my @torso = $self->fetch($msg,"RFC822.TEXT");
#     $torso[0] =~ s/.*FETCH \(.*RFC822\.TEXT //i;
#     pop @torso and pop @torso;
#     return join("",@torso);
#}


sub body_string {
	my $self = shift;
	my $msg  = shift;
	my $ref = $self->fetch($msg,"BODY" . ( $self->Peek ? ".PEEK" : "" ) . "[TEXT]");

        my $string = "";
    	foreach my $result  (@{$ref}) 	{ 
                $string .= $result->[DATA] if defined($result) and $self->_is_literal($result) ;
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
	my $popped ; $popped = pop @$ref until 	
			( 
				( 	defined($popped) and 
					# (-:	Smile!
					$popped =~ /\)\x0d\x0a$/ 
				) 	or
					not grep(
						# (-:	Smile again!
						/\)\x0d\x0a$/,
						@$ref
					)
			);

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

	$self->_record($count,[ 0, "INPUT", "$string\x0d\x0a"] );

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
	    	# escape infinite loop if read_line never returns any data:
              	$output = $self->_read_line or return undef; 

		for my $o (@$output) { 
			$self->_record($count,$o);	# $o is a ref
                      # $self->_debug("Received from readline: ${\($o->[DATA])}<<END OF RESULT>>\n");
			next unless $self->_is_output($o);
                      ($code) = $o->[DATA] =~ /^$count (OK|BAD|NO|$good)/mi ;
                      if ($o->[DATA] =~ /^\*\s+BYE/im) {
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

	$self->_record($count,[ $self->_next_index($count), "INPUT", "$string\x0d\x0a"] );

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
                      ($code) = $o->[DATA] =~ /^(?:$tag|\*) (OK|BAD|NO|$good)/m  ;
                      if ($o->[DATA] =~ /^\*\s+BYE/) {
				$self->State(Unconnected);
			}
		}
	}	
	$self->{'History'}{$tag} = $self->{"History"}{$count} unless $tag eq $count;
	return $code =~ /^OK|$good/ ? @{$self->Results} : undef ;

}
#sub bodystruct {	# return bodystruct 
#}

# _record saves the conversation into the History structure:
sub _record {

	my ($self,$count,$array) = ( shift, shift, shift);
	local($^W)= undef;

	#$self->_debug(sprintf("in _record: count is $count, values are %s/%s/%s and caller is " . 
	#	join(":",caller()) . "\n",@$array));
	
      if (    #       $array->[DATA] and 
              $array->[DATA] =~ /^\d+ LOGIN/i ) { 

              $array->[DATA] =~ s/LOGIN.*/LOGIN XXXXXXXX XXXXXXXX/i ;
	}

	push @{$self->{"History"}{$count}}, $array;

      if ( $array->[DATA] =~ /^\d+\s+(BAD|NO)\s/im ) {
              $self->LastError("$array->[DATA]") ;
              $@ = $array->[DATA];
              carp "$array->[DATA]" if $^W ;
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

	unless ($string =~ /\x0d\x0a$/ or $suppress ) {

		chomp $string;
		$string .= "\x0d" unless $string =~ /\x0d$/;	
		$string .= "\x0a" ;
	}
	if ( 
		$string =~ /^[^\x0a{]*\{(\d+)\}\x0d\x0a/ 	# ;-}
	) 	{
		my($p1,$p2,$len) ;
		if ( ($p1,$len)   = $string =~ /^([^\x0a{]*\{(\d+)\}\x0d\x0a)/ 		# } for vi
			and  (
				$len < 32766 ? 
				( ($p2) = $string =~ /^[^\x0a{]*\{\d+\}\x0d\x0a(.{$len}.*\x0d\x0a)/  ) :	
				( ($p2) = $string =~ /^[^\x0a{]*\{\d+\}\x0d\x0a(.*\x0d\x0a)/ and length($p2) == $len  )
				# }} for vi
			)
		) {
			$self->_debug("Sending literal string in two parts: $p1\n\tthen: $p2\n");
			$self->_send_line($p1) or return undef;
			$output = $self->_read_line or return undef;
			foreach my $o (@$output) {
				$self->_record($self->Count,$o);              # $o is already an array ref
                              ($code) = $o->[DATA] =~ /(^\+|NO|BAD)/i;
                              if ($o->[DATA] =~ /^\*\s+BYE/) {
					$self->State(Unconnected);
					close $fh;
					return undef ;
                              } elsif ( $o->[DATA]=~ /^\d+\s+(NO|BAD)/i ) {
					close $fh;
					return undef;
				}
			}
			if ( $code eq '+' ) { $string = $p2; } else { return undef ; }
		}
		
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
	my $temperrs = 0;
	until ($total >= length($string)) {
		my $ret =	syswrite(	
					$self->Socket, 
					$string, 
					length($string)-$total, 
					$total
					);
		if ($! == &EAGAIN ) {
			if ( 	$self->{Maxtemperrors} !~ /^unlimited/i 	and
				$temperrs++ > ( $self->{Maxtemperrors}||10) 
			) {
				$self->LastError("Persistent '${!}' errors\n");
				$self->_debug("Persistent '${!}' errors\n");
				return undef;
			}
			CORE::select(undef, undef, undef, .25 * $temperrs);
		} else {
			# avoid infinite loops on syswrite error
			return undef unless(defined $ret);	 
		}
		
		if ( defined($ret) ) {
			$temperrs = 0  ;
			$total += $ret ;
		}
	}
	_debug $self,"Sent $total bytes\n" if $self->Debug;
	return $total;
}

#_read_line reads from the socket:

# sub _old_read_line {
	
#     my $self        = shift;        
#     my $sh          = $self->Socket;
	
#     my $buffer      = ""; 
#     my $count       = ""; $count = 0;
#     my $rvec        = my $ready = my $errors = 0; 
#     my $timeout     = $self->Timeout;

#     my $readlen     = 1;
#     my $fcntl       = '';
#     my($flags,$in_literal)  = ('0',0);

#     if ( $self->Fast_io ) {
#             eval { $fcntl=fcntl($sh, F_GETFL, $flags) } ;
#             # _debug $self, STDERR 
#             # "Setfl = ",F_SETFL," and GETFL = ",F_GETFL," and NONBLOCK = ",O_NONBLOCK,"\n";
#             # _debug $self, STDERR "Fcntl flag is now $fcntl\n";
#             if ($@) {
#                     $self->Fast_io(0);
#                     carp ref($self) . " not using Fast_IO; not available on this platform.\n" 
#                             if ( $^W or $self->Debug);
#             } else {
 
#                     my $newflags = $fcntl;
#                     $newflags |= O_NONBLOCK;
#                     fcntl($sh, F_SETFL, $newflags) and $readlen = ($self->{Buffer}||4096);
#             }
#     }
#     my $offset = 0;

#     until ($buffer =~ /\r?\n$/ ) {
#             # _debug $self,"Entering read engine.\n" if $self->Debug;
#             if ($timeout) {
#                     vec($rvec, fileno($self->Socket), 1) = 1;
#                     CORE::select( $ready = $rvec, undef, $errors = $rvec, $timeout) ;
#                     unless ( vec ( $ready, fileno($self->Socket), 1 ) ) {
#                             $self->LastError("Tag " . $self->Transaction . 
#                                     ": Timeout waiting for data from server\n");    
#                             fcntl($sh, F_SETFL, $fcntl) 
#                                     if $self->Fast_io and defined($fcntl);
#                             $self->_record($self->Transaction,
#                                     [       $self->_next_index($self->Transaction),
#                                             "ERROR",
#                                             $self->Transaction . "* NO Timeout during read from server\r\n"
#                                     ]
#                             );
#                             $@ = "Timeout during read from server\r\n";
#                             return undef;
#                     }
#             }
#             # _debug($self,"count is $count and length of buffer is " . length($buffer) . "\n");
#             local($^W) = undef;
#             $count += sysread(
#                                     $sh,
#                                     $buffer,
#                                     $readlen,
#                                     $offset
#             ) ;
#             $offset = $count ;
#             pos $buffer = 1;
#             LITERAL: while ( $buffer =~ /\{(\d+)\}\r\n/g and ! $in_literal ) {
#                     my $len = $1 ;
#                     $in_literal++;
#                     _debug $self,   "Buffer:\n$buffer" . ('-' x 30) . "\n" if $self->Debug;
#                     $offset = $count - length($buffer) ;
#                     $count -= length("{" . "$len" . "}\r\n" ) ;

#                     # _debug($self, "Count = $count and offset = $offset\n") if $self->Debug;

#                     # If I used anything from the buffer for my literal then it needs to come
#                     # out of the buffer now:                (use substr to avoid regexp overhead)

#                     substr($buffer , index($buffer, "{" . $len . "}\r\n"), length("{}\n\r" . $len)) = "";
				

#                     if ($timeout) {
#                             vec($rvec, fileno($self->Socket), 1) = 1;
#                             unless ( CORE::select( $ready = $rvec, 
#                                                     undef, 
#                                                     $errors = $rvec, 
#                                                     $timeout) 
#                             ) {
#                                     $self->LastError("Tag " . $self->Transaction . 
#                                             ": Timeout waiting for literal data from server\n");    
#                                     return undef;
#                             }       
#                     }
#                     until ( $offset >= $len ) {
#                             # _debug $self, "Reading literal data\n";
#                             $offset += sysread($sh,$buffer,$len-$offset, $count+$offset) ;
#                     }
#                     $count += $offset;
#                     pos $buffer = 1;
#             }
#             $offset = length($buffer);

#             pos $buffer = 1;
#     }
#     fcntl($sh, F_SETFL, $fcntl) if $self->Fast_io and defined($fcntl);
#     #       _debug $self, "Buffer is now $buffer\n";
#     _debug $self, "Read: $buffer\n" if $self->Debug;
#     return defined($buffer) ? $buffer : undef ;
# }


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
	until (	
		# there's stuff in output buffer:
		scalar(@$oBuffer)	and 			

		# the last thing there has cr-lf:
                $oBuffer->[-1][DATA] =~ /\x0d\x0a$/  and     

		# that thing is an output line:
                $oBuffer->[-1][TYPE]    eq "OUTPUT"  and     

		# and the input buffer has been MT'ed:
		$iBuffer		eq "" 		

	) {
              my $transno = $self->Transaction;  # used below in several places
		if ($timeout) {
			vec($rvec, fileno($self->Socket), 1) = 1;
			my @ready = $self->{_select}->can_read($timeout) ;
			unless ( @ready ) {
				$self->LastError("Tag $transno: " .
					"Timeout after $timeout seconds " .
					"waiting for data from server\n");	
				$self->_record($transno,
					[	$self->_next_index($transno),
						"ERROR",
						"$transno * NO Timeout after ".
						"$timeout seconds " .
						"during read from " .
						"server\x0d\x0a"
					]
				);
				$self->LastError(
					"Timeout after $timeout seconds " .
					"during read from server\x0d\x0a"
				);
				return undef;
			}
		}
		
		local($^W) = undef;	# Now quiet down warnings

		# read "$readlen" bytes (or less):
              # need to check return code from sysread in case other end has shut down!!!
              my $ret = sysread( $sh, $iBuffer, $readlen, length($iBuffer)) ;
		# $self->_debug("Read so far: $iBuffer<<END>>\n");
              if($timeout and ! defined($ret)) { # Blocking read error...
                  my $msg = "Error while reading data from server: $!\x0d\x0a";
                  $self->_record($transno,
                                 [ $self->_next_index($transno),
                                   "ERROR", "$transno * NO $msg "
                                   ]);
                  $@ = "$msg";
                  return undef;
              }
              elsif(defined($ret) and $ret == 0) {    # Caught EOF...
                  my $msg="Socket closed while reading data from server.\x0d\x0a";
                  $self->_record($transno,
                                 [ $self->_next_index($transno),
                                   "ERROR", "$transno * NO $msg "
                                   ]);
                  $@ = "$msg";
                  return undef;
              }
              # successfully wrote to other end, keep going...
              $count += $ret;
		LINES: while ( $iBuffer =~ s/^(.*?\x0d?\x0a)// ) {
		   my $current_line = $1;

		   # $self->_debug("BUFFER: pulled from buffer: <BEGIN>${current_line}<END>\n" .
		   # 	"and left with buffer contents of: <BEGIN>${iBuffer}<END>\n");

		   LITERAL: if ($current_line =~ s/\{(\d+)\}\x0d\x0a$//) {
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
			my $callback_value = "";

			if ( defined($literal_callback) ) 	{	
				if 	( $literal_callback =~ /GLOB/) 	{	
					print $literal_callback $litstring ;
					$litstring = "";
				} elsif ($literal_callback =~ /CODE/ ) {
					# Don't do a thing

				} else 	{
					$self->LastError(
						ref($literal_callback) . 
						" is an invalid callback type; must be a filehandle or coderef"
					); 
				}

		
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
							"$transno * NO Error reading data from server: $!\n"	
						]
					);
					return undef;
				} elsif ( $ret == 0 and eof($sh) ) {
					$self->_record($transno, [ 
							$self->_next_index($transno),
							"ERROR",
							"$transno * ".
							"BYE Server unexpectedly closed connection: $!\n"	
						]
					);
					$self->State(Unconnected);
					return undef;
				}
				$remainder_count -= $ret;	   # decrement remaining bytes by amt read
				if ( defined($literal_callback) ) {
					if ( $literal_callback =~ /GLOB/ ) {
						print $literal_callback $litstring;
						$litstring = "";
					} 
				}

			}
			$literal_callback->($litstring) 
				if defined($litstring) and 
				$literal_callback =~ /CODE/;

			$self->Fast_io($fast_io) if $fast_io;

		# Now let's make sure there are no IMAP server output lines 
		# (i.e. [tag|*] BAD|NO|OK Text) embedded in the literal string
		# (There shouldn't be but I've seen it done!), but only if
		# EnableServerResponseInLiteral is set to true

			my $embedded_output = 0;
			my $lastline = ( split(/\x0d?\x0a/,$litstring))[-1] 
				if $litstring;

			if ( 	$self->EnableServerResponseInLiteral and
				$lastline and 
				$lastline =~ /^(?:\*|(\d+))\s(BAD|NO|OK)/i 
			) {
			  $litstring =~ s/\Q$lastline\E\x0d?\x0a//;
			  $embedded_output++;

			  $self->_debug("Got server output mixed in " .
					"with literal: $lastline\n"
			  ) 	if $self->Debug;

			}
		  	# Finally, we need to stuff the literal onto the 
			# end of the oBuffer:
			push @$oBuffer, [ $index++, "OUTPUT" , $current_line],
					[ $index++, "LITERAL", $litstring   ];
			push @$oBuffer,	[ $index++, "OUTPUT",  $lastline    ] 
					if $embedded_output;

		  } else { 
			push @$oBuffer, [ $index++, "OUTPUT" , $current_line ]; 
		  }
		
		}
		#$self->_debug("iBuffer is now: $iBuffer<<END OF BUFFER>>\n");
	}
	#	_debug $self, "Buffer is now $buffer\n";
      _debug $self, "Read: " . join("",map {$_->[DATA]} @$oBuffer) ."\n" 
		if $self->Debug;
	return scalar(@$oBuffer) ? $oBuffer : undef ;
}

sub Report {
	my $self = shift;
#	$self->_debug( "Dumper: " . Data::Dumper::Dumper($self) . 
#			"\nReporting on following keys: " . join(", ",keys %{$self->{'History'}}). "\n");
	return 	map { 
                      map { $_->[DATA] } @{$self->{"History"}{$_}} 
	}		sort { $a <=> $b } keys %{$self->{"History"}}
	;
}


sub Results {
	my $self 	= shift	;
	my $transaction = shift||$self->Count;
	
	return wantarray 							? 
              map {$_->[DATA] }       @{$self->{"History"}{$transaction}}     : 
              [ map {$_->[DATA] }     @{$self->{"History"}{$transaction}} ]   ;
}


sub LastIMAPCommand {
      my @a = map { $_->[DATA] } @{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}};
	return shift @a;
}


sub History {
      my @a = map { $_->[DATA] } @{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}};
	shift @a;
	return wantarray ? @a : \@a ;

}

sub Escaped_results {
	my @a;
	foreach  my $line (@{$_[0]->{"History"}{$_[1]||$_[0]->Transaction}} ) {
		if (  defined($line) and $_[0]->_is_literal($line) ) { 
			$line->[DATA] =~ s/([\\\(\)"\x0d\x0a])/\\$1/g ;
			push @a, qq("$line->[DATA]");
		} else {
      			push @a, $line->[DATA] ;
		}
	}
	shift @a;	# $a[0] is the ALWAYS the command ; I make sure of that in _imap_command
	return wantarray ? @a : \@a ;

}

sub Unescape {
	shift @_ if $_[1];
	my $whatever = shift;
	$whatever =~ s/\\([\\\(\)"\x0d\x0a])/$1/g if defined $whatever;
	return $whatever;
}

sub logout {
	my $self = shift;
	my $string = "LOGOUT";
	$self->_imap_command($string) ; 
	$self->State(Unconnected);
	$self->{Folders} = undef;
	$self->{_IMAP4REV1} = undef;
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
	# my @list = 
	# foreach (@list) { $self->_debug("Pushing $_\n"); }
	my $m;

	for ($m = 0; $m < scalar(@list); $m++ ) {
		# $self->_debug("Folders: examining $list[$m]\n");

		if ($list[$m] && $list[$m]  !~ /\x0d\x0a$/ ) {
			$self->_debug("folders: concatenating $list[$m] and " . $list[$m+1] . "\n") ;
			$list[$m] .= $list[$m+1] ;
			$list[$m+1] = "";	
			$list[$m] .= "\x0d\x0a" unless $list[$m] =~ /\x0d\x0a$/;
		}
			
		

		push @folders, $1||$2 
			if $list[$m] =~
                        /       ^\*\s+LIST               # * LIST
                                \s+\([^\)]*\)\s+         # (Flags)
                                (?:"[^"]*"|NIL)\s+	 # "delimiter" or NIL
                                (?:"([^"]*)"|(.*))\x0d\x0a$  # Name or "Folder name"
                        /ix;
		$folders[-1] = '"' . $folders[-1] . '"' 
			if $1 and !$self->exists($folders[-1]) ;
		# $self->_debug("folders: line $list[$m]: 1=$1 and 2=$2\n");
        } 

        # for my $f (@folders) { $f =~ s/^\\FOLDER LITERAL:://;}
	my @clean = (); my %memory = ();
	foreach my $f (@folders) { push @clean, $f unless $memory{$f}++ }
        $self->{Folders} = \@clean unless $what;

        return wantarray ? @clean : \@clean ;
}


sub exists {
	my ($self,$what) = (shift,shift);
	return $self if $self->STATUS($self->Massage($what),"(MESSAGES)");
	return undef;
}
	
sub get_bodystructure {
	my($self,$msg) = @_;
	unless ( eval {require Mail::IMAPClient::BodyStructure ; 1 } ) {
		$self->LastError("Unable to use get_bodystructure: $@\n");
		return undef;
	}
	my $bs = "";
	eval { $bs = Mail::IMAPClient::BodyStructure->new(
		grep(	/bodystructure \(/i, 	# Wee! ;-)
			$self->fetch($msg,"BODYSTRUCTURE")
		)
	)};  
	$self->_debug("get_bodystructure: msg $msg returns this ref: ". 
		( $bs ? " $bs" : " UNDEF" ) 
		."\n");
	return $bs;
}

sub get_envelope {
	my($self,$msg) = @_;
	unless ( eval {require Mail::IMAPClient::BodyStructure ; 1 } ) {
		$self->LastError("Unable to use get_envelope: $@\n");
		return undef;
	}
	my $bs = "";
	eval { $bs = Mail::IMAPClient::BodyStructure::Envelope->new(
		grep(	/ENVELOPE \(/i, 	# Wee! ;-)
			$self->fetch($msg,"ENVELOPE")
		)
	)};  
	$self->_debug("get_envelope: msg $msg returns this ref: ". 
		( $bs ? " $bs" : " UNDEF" ) 
		."\n");
	return $bs;
}
	
sub fetch {

	my $self = shift;
	my $what = shift||"ALL";
	ref($what) and $what = join(",",@$what);	
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) .
				"FETCH $what" . ( @_ ? " " . join(" ",@_) : '' )
	) 	 					or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
                              [ map { $_->[DATA] } @{$self->{'History'}{$self->Count}} ];

}
	
sub AUTOLOAD {

	my $self = shift;
	return undef if $Mail::IMAPClient::AUTOLOAD =~ /DESTROY$/;
	delete $self->{Folders}  ;
	my $autoload = $Mail::IMAPClient::AUTOLOAD;
	$autoload =~ s/.*:://;
	if (	
			$^W
		and	$autoload =~ /^[a-z]+$/
		and	$autoload !~ 
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
	) {
		carp 	"$autoload is all lower-case. " .
			"May conflict with future methods. " .
			"Change method name to be mixed case or all upper case to ensure " .
			"upward compatability"
	}
	if (scalar(@_)) {
		my @a = @_;
		if (	
			$autoload =~ 
				/^(?:subscribe|delete|myrights)$/i
		) {
			$a[-1] = $self->Massage($a[-1]) ;
		} elsif (	
			$autoload =~ 
				/^(?:create)$/i
		) {
			$a[0] = $self->Massage($a[0]) ;
		} elsif (
			$autoload =~ /^(?:store|copy)$/i
		) {
			$autoload = "UID $autoload"
				if $self->Uid;
		} elsif (
			$autoload =~ /^(?:expunge)$/i and defined($_[0])
		) {
			my $old;
			if ( $_[0] ne $self->Folder ) {
				$old = $self->Folder; $self->select($_[0]); 
			} 	
			my $succ = $self->_imap_command(qq/$autoload/) ;
			$self->select($old);
			return undef unless $succ;
			return wantarray ? 	$self->History($self->Count) 	: 
                                              map {$_->[DATA]}@{$self->{'History'}{$self->Count}}     ;
			
		}
		$self->_debug("Autoloading: $autoload " . ( @a ? join(" ",@a):"" ) ."\n" )
			if $self->Debug;
		return undef 
			unless $self->_imap_command(
			 	qq/$autoload/ .  ( @a ? " " . join(" ",@a) : "" )
			)  ;
	} else {
		$self->Folder(undef) if $autoload =~ /^(?:close)/i ; 
		$self->_imap_command(qq/$autoload/) or return undef;
	}
	return wantarray ? 	$self->History($self->Count) 	: 
                              [map {$_->[DATA] } @{$self->{'History'}{$self->Count}}] ;

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
                              [map{$_->[DATA]}@{$self->{'History'}{$self->Count}}];

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
			#$self->_debug("flags: line = '$resultline' and 1,2,3,4 = $1,$2,$3,$4\n") 
			#		if $self->Debug;
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
	
        for my $header (map { split(/(?:\x0d\x0a)/,$_) } @raw) {
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
	
       	for my $header (map { split(/(?:\x0d\x0a)/,$_) } @raw) {
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
        foreach my $result  (@{$self->{"History"}{$self->Transaction}}) {
              return $1 if $result->[DATA] =~ /\(MESSAGES\s+(\d+)\s*\)/ ;
        }

	return undef;

}

{
for my $datum (
                qw(     recent seen
                        unseen messages
                 )
) {
        no strict 'refs';
        *$datum = sub {
		my $self = shift;
		#my @hits;

		#my $hits = $self->search($datum eq "messages" ? "ALL" : "$datum")
		#	 or return undef;
		#print "Received $hits from search and array context flag is ",wantarry,"\n";
		#if ( scalar(@$hits) ) {
		#	return wantarray ? @$hits : $hits ;
		#}
		return $self->search($datum eq "messages" ? "ALL" : "$datum") ;


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
	$@ = "";
	# massage?
	$a[-1] = $self->Massage($a[-1],1) 
		if scalar(@a) > 1 and !exists($SEARCH_KEYS{uc($a[-1])}); 
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) . "SEARCH ". join(' ',@a)) 
			 or return undef;
	my $results =  $self->History($self->Count) ;


	for my $r (@$results) {
	#$self->_debug("Considering the search result line: $r");			
               chomp $r;
               $r =~ s/\r\n?/ /g;
               $r =~ s/^\*\s+SEARCH\s+(?=.*\d.*)// or next;
               my @h = grep(/^\d+$/,(split(/\s+/,$r)));
	       push @hits, @h if scalar(@h) ; # and grep(/\d/,@h) );

	}

	$self->{LastError}="Search completed successfully but found no matching messages\n"
		unless scalar(@hits);

	if ( wantarray ) {

		return @hits;
	} else {
		return scalar(@hits) ? \@hits : undef;
	}
}

sub thread {
	# returns a Thread data structure
	#
	# $imap->thread($algorythm, $charset, @search_args);
	my $self = shift;

	my $algorythm     = shift;
	   $algorythm   ||= $self->has_capability("THREAD=REFERENCES") ? "REFERENCES" : "ORDEREDSUBJECT";
	my $charset 	  = shift;
	   $charset 	||= "UTF-8";

	my @a = @_;

	$a[0]||="ALL" ;
	my @hits;
	# massage?

	$a[-1] = $self->Massage($a[-1],1) 
		if scalar(@a) > 1 and !exists($SEARCH_KEYS{uc($a[-1])}); 
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) . 
				"THREAD $algorythm $charset " . 
				join(' ',@a)
	) or return undef;
	my $results =  $self->History($self->Count) ;

	my $thread = "";
	for my $r (@$results) {
		#$self->_debug("Considering the search result line: $r");			
               	chomp $r;
               	$r =~ s/\r\n?/ /g;
               	if ( $r =~ /^\*\s+THREAD\s+/ ) {
			eval { require "Mail/IMAPClient/Thread.pm" }
				or ( $self->LastError($@), return undef);
			my $parser = Mail::IMAPClient::Thread->new();
			$thread = $parser->start($r) ;
		} else {
			next;
		}
	       	#while ( $r =~ /(\([^\)]*\))/ ) { 
		#	push @hits, [ split(/ /,$1) ] ;
		#}
	}

	$self->{LastError}="Thread search completed successfully but found no matching messages\n"
		unless ref($thread);
	return $thread ||undef;

	if ( wantarray ) {

		return @hits;
	} else {
		return scalar(@hits) ? \@hits : undef;
	}
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

	my @caps = ref($self->{CAPABILITY}) 		? 
			keys %{$self->{CAPABILITY}} 	: 
			map { split } 
				grep (s/^\*\s+CAPABILITY\s+//, 
				$self->History($self->Count));

	unless ( exists $self->{CAPABILITY} ) { 
		for (@caps) { 
			$self->{CAPABILITY}{uc($_)}++ ;
			if (/=/) {
				my($k,$v)=split(/=/,$_) ;
				$self->{uc($k)} = uc($v) ;
			}
		} 
	}
	

	return wantarray ? @caps : \@caps;
}

sub has_capability {
	my $self = shift;
	$self->capability;
	return $self->{CAPABILITY}{uc($_[0])};
}

sub imap4rev1 {
	my $self = shift;
	return exists($self->{_IMAP4REV1}) ?  
		$self->{_IMAP4REV1} : 
		$self->{_IMAP4REV1} = $self->has_capability(IMAP4REV1) ;
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
	$namespace =~ s/\x0d?\x0a$//;
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

# Contributed by jwm3
sub internaldate {
        my $self = shift;
        my $msg  = shift;
        $self->_imap_command( ( $self->Uid ? "UID " : "" ) . "FETCH $msg INTERNALDATE") or return undef;
        my $internalDate = join("", $self->History($self->Count));
        $internalDate =~ s/^.*INTERNALDATE "//si;
        $internalDate =~ s/\".*$//s;
        return $internalDate;
}

sub is_parent {
	my ($self, $folder) = (shift, shift);
	# $self->_debug("Checking parentage " . ( $folder ? "for folder $folder" : "" ) . "\n");
        my $list = $self->list(undef, $folder);
	my $line;

        for (my $m = 0; $m < scalar(@$list); $m++ ) {
		#$self->_debug("Judging whether or not $list->[$m] is fit for parenthood\n");
		return undef if $list->[$m] =~ /NoInferior/i; 	# let's not beat around the bush
                if ($list->[$m]  =~ s/(\{\d+\})\x0d\x0a$// ) {
                        $list->[$m] .= $list->[$m+1];
                        $list->[$m+1] = "";
                }

	    	$line = $list->[$m]
                        if $list->[$m] =~
                        /       ^\*\s+LIST              # * LIST
                                \s+\([^\)]*\)\s+            # (Flags)
                                "[^"]*"\s+              # "delimiter"
                                (?:"([^"]*)"|(.*))\x0d\x0a$  # Name or "Folder name"
                        /x;
	}	
	my($f) = $line =~ /^\*\s+LIST\s+\(([^\)]*)\s*\)/ if $line;
	return  1 if $f =~ /HasChildren/i ;
	return 0 if $f =~ /HasNoChildren/i ;
	unless ( $f =~ /\\/) {		# no flags at all unless there's a backslash
		my $sep = $self->separator($folder);
		return 1 if scalar(grep /^${folder}${sep}/, $self->folders);
		return 0;
	}
}

sub selectable {my($s,$f)=@_;return grep(/NoSelect/i,$s->list("",$f))?0:1;}

sub append_string {

        my $self = shift;
        my $folder = $self->Massage(shift);

	my $text = shift;
	$text =~ s/\x0d?\x0a/\x0d\x0a/g;
 
	my($flags,$date) = (shift,shift);

	if (defined($flags)) {
		$flags =~ s/^\s+//g;
		$flags =~ s/\s+$//g;
	}

	if (defined($date)) {
		$date =~ s/^\s+//g;
		$date =~ s/\s+$//g;
	}

	$flags = "($flags)"  if $flags and $flags !~ /^\(.*\)$/ ;
	$date  = qq/"$date"/ if $date  and $date  !~ /^"/ 	;

        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);

        my $string = 	  "$count APPEND $folder "  	  . 
			( $flags ? "$flags " : "" 	) . 
			( $date ? "$date " : "" 	) . 
			"{" . length($text)  . "}\x0d\x0a" ;

        $self->_record($count,[ $self->_next_index($count), "INPUT", "$string\x0d\x0a" ] );

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

                      ($code) = $o->[DATA] =~ /(^\+|^\d*\s*NO|^\d*\s*BAD)/i ;

                      if ($o->[DATA] =~ /^\*\s+BYE/i) {
                              $self->LastError("Error trying to append string: " . 
						$o->[DATA]. "; Disconnected.\n");
                              $self->_debug("Error trying to append string: " . $o->[DATA]. 
					"; Disconnected.\n");
                              carp("Error trying to append string: " . $o->[DATA] ."; Disconnected") if $^W;
				$self->State(Unconnected);

                      } elsif ( $o->[DATA] =~ /^\d*\s*(NO|BAD)/i ) { # i and / transposed!!!
                              $self->LastError("Error trying to append string: " . $o->[DATA]  . "\n");
                              $self->_debug("Error trying to append string: " . $o->[DATA] . "\n");
                              carp("Error trying to append string: " . $o->[DATA]) if $^W;
				return undef;
			}
		}
	}	
	
	$self->_record($count,[ $self->_next_index($count), "INPUT", "$text\x0d\x0a" ] );

	# Step 3: Send the actual text of the message:
        $feedback = $self->_send_line("$text\x0d\x0a");

        unless ($feedback) {
                $self->LastError("Error sending append msg text to IMAP: $!\n");
                return undef;
        }
	$code = undef;			# clear out code

	# Step 4: Figure out the results:
        until ($code) {
                $output = $self->_read_line or return undef;
              $self->_debug("Append results: " . map({ $_->[DATA] } @$output) . "\n" )
			if $self->Debug;
                foreach my $o (@$output) {
			$self->_record($count,$o); # $o is already an array ref

                      ($code) = $o->[DATA] =~ /^(?:$count|\*) (OK|NO|BAD)/im  ;
			
                      if ($o->[DATA] =~ /^\*\s+BYE/im) {
				$self->State(Unconnected);
                              $self->LastError("Error trying to append: " . $o->[DATA] . "\n");
                              $self->_debug("Error trying to append: " . $o->[DATA] . "\n");
                              carp("Error trying to append: " . $o->[DATA] ) if $^W;
			}
			if ($code and $code !~ /^OK/im) {
                              $self->LastError("Error trying to append: " . $o->[DATA] . "\n");
                              $self->_debug("Error trying to append: " . $o->[DATA] . "\n");
                              carp("Error trying to append: " . $o->[DATA] ) if $^W;
				return undef;
			}
        	}
	}

      my($uid) = join("",map { $_->[TYPE] eq "OUTPUT" ? $_->[DATA] : () } @$output ) =~ m#\s+(\d+)\]#;

        return defined($uid) ? $uid : $self;
}
sub append {

        my $self = shift;
	# now that we're passing thru to append_string we won't massage here
        # my $folder = $self->Massage(shift); 
        my $folder = shift;

	my $text = join("\x0d\x0a",@_);
	$text =~ s/\x0d?\x0a/\x0d\x0a/g;
	return $self->append_string($folder,$text);
}

=begin legacy
	#{
        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);

        my $string = "$count APPEND $folder {" . length($text)  . "}\x0d\x0a" ;

        $self->_record($count,[ $self->_next_index($count), "INPUT", "$string\x0d\x0a" ] );

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

                      ($code) = $o->[DATA] =~ /(^\+|^\d*\s*NO|^\d*\s*BAD)/i ;

                      if ($o->[DATA] =~ /^\*\s+BYE/i) {
                              $self->LastError("Error trying to append: " . $o->[DATA]. "; Disconnected.\n");
                              $self->_debug("Error trying to append: " . $o->[DATA]. "; Disconnected.\n");
                              carp("Error trying to append: " . $o->[DATA] ."; Disconnected") if $^W;
				$self->State(Unconnected);

                      } elsif ( $o->[DATA] =~ /^\d*\s*(NO|BAD)/i ) { # i and / transposed!!!
                              $self->LastError("Error trying to append: " . $o->[DATA]  . "\n");
                              $self->_debug("Error trying to append: " . $o->[DATA] . "\n");
                              carp("Error trying to append: " . $o->[DATA]) if $^W;
				return undef;
			}
		}
	}	

	$self->_record($count,[ $self->_next_index($count), "INPUT", "$text\x0d\x0a" ] );

	# Step 3: Send the actual text of the message:
        $feedback = $self->_send_line("$text\x0d\x0a");

        unless ($feedback) {
                $self->LastError("Error sending append msg text to IMAP: $!\n");
                return undef;
        }
	$code = undef;			# clear out code

	# Step 4: Figure out the results:
        until ($code) {
                $output = $self->_read_line or return undef;
              $self->_debug("Append results: " . map({ $_->[DATA] } @$output) . "\n" )
			if $self->Debug;
                foreach my $o (@$output) {
			$self->_record($count,$o); # $o is already an array ref

                      ($code) = $o->[DATA] =~ /^(?:$count|\*) (OK|NO|BAD)/im  ;

                      if ($o->[DATA] =~ /^\*\s+BYE/im) {
				$self->State(Unconnected);
                              $self->LastError("Error trying to append: " . $o->[DATA] . "\n");
                              $self->_debug("Error trying to append: " . $o->[DATA] . "\n");
                              carp("Error trying to append: " . $o->[DATA] ) if $^W;
			}
			if ($code and $code !~ /^OK/im) {
                              $self->LastError("Error trying to append: " . $o->[DATA] . "\n");
                              $self->_debug("Error trying to append: " . $o->[DATA] . "\n");
                              carp("Error trying to append: " . $o->[DATA] ) if $^W;
				return undef;
			}
        	}
	}

      my($uid) = join("",map { $_->[TYPE] eq "OUTPUT" ? $_->[DATA] : () } @$output ) =~ m#\s+(\d+)\]#;

        return defined($uid) ? $uid : $self;
}

=end legacy

=cut

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

	my $bare_nl_count = scalar grep { /^\x0a$|[^\x0d]\x0a$/} <$fh>;

	seek($fh,0,0);
	
        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $length = ( -s $file ) + $bare_nl_count;

        my $string = "$count APPEND $folder {" . $length  . "}\x0d\x0a" ;

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
                      ($code) = $o->[DATA] =~ /(^\+|^\d+\sNO|^\d+\sBAD)/i; 
                      if ($o->[DATA] =~ /^\*\s+BYE/) {
                              carp $o->[DATA] if $^W;
				$self->State(Unconnected);
				close $fh;
				return undef ;
                      } elsif ( $o->[DATA]=~ /^\d+\s+(NO|BAD)/i ) {
                              carp $o->[DATA] if $^W;
				close $fh;
				return undef;
			}
		}
	}	
	
	{ 	# Narrow scope
		# Slurp up headers: later we'll make this more efficient I guess
		local $/ = "\x0d\x0a\x0d\x0a"; 
		my $text = <$fh>;
		$text =~ s/\x0d?\x0a/\x0d\x0a/g;
		$self->_record($count,[ $self->_next_index($count), "INPUT", "{From file $file}" ] ) ;
		$feedback = $self->_send_line($text);

		unless ($feedback) {
			$self->LastError("Error sending append msg text to IMAP: $!\n");
			close $fh;
			return undef;
		}
		_debug $self, "control points to $$control\n" if ref($control) and $self->Debug;
		$/ = 	ref($control) ?  "\x0a" : $control ? $control : 	"\x0a";	
		while (defined($text = <$fh>)) {
			$text =~ s/\x0d?\x0a/\x0d\x0a/g;
			$self->_record(	$count,
					[ $self->_next_index($count), "INPUT", "{from $file}\x0d\x0a" ] 
			);
			$feedback = $self->_send_line($text,1);

			unless ($feedback) {
				$self->LastError("Error sending append msg text to IMAP: $!\n");
				close $fh;
				return undef;
			}
		}
		$feedback = $self->_send_line("\x0d\x0a");

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
                      $self->_debug("append_file: Deciding if " . $o->[DATA] . " has the code.\n") 
				if $self->Debug;
                      ($code) = $o->[DATA]  =~ /^\d+\s(NO|BAD|OK)/i; 
			# try to grab new msg's uid from o/p
                      $o->[DATA]  =~ m#UID\s+\d+\s+(\d+)\]# and $uid = $1; 
                      if ($o->[DATA] =~ /^\*\s+BYE/) {
                              carp $o->[DATA] if $^W;
				$self->State(Unconnected);
				close $fh;
				return undef ;
                      } elsif ( $o->[DATA]=~ /^\d+\s+(NO|BAD)/i ) {
                              carp $o->[DATA] if $^W;
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

        $self->_record($count,[ $self->_next_index($self->Transaction), "INPUT", "$string\x0d\x0a"] );

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
			($code) = $o->[DATA] =~ /^\+(.*)$/ ;
			if ($o->[DATA] =~ /^\*\s+BYE/) {
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

	$code = ""; 	# clear code
        until ($code) {
                $output = $self->_read_line or return undef;
		foreach my $o (@$output) {
                	$self->_record($count,$o);	# $o is a ref
			if ( ($code) = $o->[DATA] =~ /^\+ (.*)$/ ) {
				$feedback = $self->_send_line($response->($code));
				unless ($feedback) {
					$self->LastError("Error sending append msg text to IMAP: $!\n");
					return undef;
				}
				$code = "" ;		# Clear code; we're still not finished
			} else {
				$o->[DATA] =~ /^$count (OK|NO|BAD)/ and $code = $1;
				if ($o->[DATA] =~ /^\*\s+BYE/) {
					$self->State(Unconnected);
					return undef ;
				}
			}
		}
        }

        $code =~ /^OK/ and $self->State(Authenticated) ;
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
	$flag =~ /^\\/ or $flag = "\\" . $flag 
		if $flag =~ /^(Answered|Flagged|Deleted|Seen|Draft)$/i;
	$self->store( join(",",@msgs), "+FLAGS.SILENT (" . $flag . ")" );
}

sub see {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->set_flag('\\Seen', @msgs);
}

sub mark {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->set_flag('\\Flagged', @msgs);
}

sub unmark {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->unset_flag('\\Flagged', @msgs);
}

sub unset_flag {
	my($self, $flag, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$flag =~ /^\\/ or $flag = "\\" . $flag 
		if $flag =~ /^(Answered|Flagged|Deleted|Seen|Draft)$/i;
	$self->store( join(",",@msgs), "-FLAGS.SILENT (" . $flag . ")" );
}

sub deny_seeing {
	my($self, @msgs) = @_;
	if ( ref($msgs[0]) ) { @msgs = @{$msgs[0]} };
	$self->unset_flag('\\Seen', @msgs);
}

sub size {

	my ($self,$msg) = @_;
	# return undef unless fetch is successful
	my @data = $self->fetch($msg,"(RFC822.SIZE)");
	return undef unless defined($data[0]);
	my($size) = grep(/RFC822\.SIZE/,@data);

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
	my $notFolder = shift;
	return unless $arg;
	my $escaped_arg = $arg; $escaped_arg =~ s/"/\\"/g;
	$arg 	= substr($arg,1,length($arg)-2) if $arg =~ /^".*"$/
                and ! ( $notFolder or $self->STATUS(qq("$escaped_arg"),"(MESSAGES)"));


	if ($arg =~ /["\\]/) {
		$arg = "{" . length($arg) . "}\x0d\x0a$arg" ;
	} elsif ($arg =~ /\s/) {
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


sub Status            { $_[0]->State                           ;       }
sub IsUnconnected     { ($_[0]->State == Unconnected)  ? 1 : 0 ;       }
sub IsConnected       { ($_[0]->State >= Connected)    ? 1 : 0 ;       }
sub IsAuthenticated   { ($_[0]->State >= Authenticated)? 1 : 0 ;       }
sub IsSelected        { ($_[0]->State == Selected)     ? 1 : 0 ;       }               


# The following private methods all work on an output line array.
# _data returns the data portion of an output array:
sub _data {   defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] or return undef; $_[1]->[DATA]; }

# _index returns the index portion of an output array:
sub _index {  defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] or return undef; $_[1]->[INDEX]; }

# _type returns the type portion of an output array:
sub _type {  defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] or return undef; $_[1]->[TYPE]; }

# _is_literal returns true if this is a literal:
sub _is_literal { defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] and $_[1]->[TYPE] eq "LITERAL" };

# _is_output_or_literal returns true if this is an 
#  	output line (or the literal part of one):
sub _is_output_or_literal { 
              defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] and 
			($_[1]->[TYPE] eq "OUTPUT" || $_[1]->[TYPE] eq "LITERAL") 
};

# _is_output returns true if this is an output line:
sub _is_output { defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] and $_[1]->[TYPE] eq "OUTPUT" };

# _is_input returns true if this is an input line:
sub _is_input { defined $_[1] and ref $_[1] and defined $_[1]->[TYPE] and $_[1]->[TYPE] eq "INPUT" };

# _next_index returns next_index for a transaction; may legitimately return 0 when successful.
sub _next_index { 
      defined(scalar(@{$_[0]->{'History'}{$_[1]||$_[0]->Transaction}}))       ? 
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

This module provides methods implementing the IMAP protocol. It allows
perl scripts to interact with IMAP message stores.

The module is used by constructing or instantiating a new IMAPClient
object via the L<new> constructor method. Once the object has been
instantiated, the L<connect> method is either implicitly or explicitly
called. At that point methods are available that implement the IMAP
client commands as specified in I<RFC2060>. When processing is
complete, the I<logoff> object method is called, either explicitly by
the program or implicitly when the object goes out of scope (or at
program termination). 

This documentation is not meant to be a replacement for RFC2060, and
the wily programmer will have a copy of that document handy when coding
IMAP clients. 

Note that this documentation uses the term I<folder> in place of
RFC2060's use of I<mailbox>. This documentation reserves the use of the
term I<mailbox> to refer to the set of folders owned by a specific IMAP
id.

RFC2060 defines four possible states for an IMAP connection: not
authenticated, authenticated, selected, and logged out. These
correspond to the B<IMAPClient> constants C<Connected>,
C<Authenticated>, C<Selected>, and C<Unconnected>, respectively. These
constants are implemented as class methods, and can be used in
conjunction with the L<Status> method to determine the status of an
B<IMAPClient> object and its underlying IMAP session. Note that an
B<IMAPClient> object can be in the C<Unconnected> state both before a
server connection is made and after it has ended. This differs slightly
from RFC2060, which does not define a pre-connection status. For a
discussion of the methods available for examining the B<IMAPClient>
object's status, see the section labeled L<"Status Methods">, below.

=head2 Errors

If you attempt an operation that results in an error, then you can
retrieve the text of the error message by using the L<LastError>
method. However, since the L<LastError> method is an object method (and
not a class method) you will only be able to use this method if you've
successfully created your object. Errors in the L<new> method can
prevent your object from ever being created. Additionally, if you
supply the I<Server>, I<User>, and I<Password> parameters to L<new>, it
will attempt to call B<connect> and B<login>, either of which could
fail and cause your L<new> method call to return C<undef> (in which case
your object will have been created but its reference will have been
discarded before ever being returned to you).

If this happens to you, you can always check C<$@>. B<Mail::IMAPClient>
will populate that variable with something useful if either of the
L<new>, L<connect>, or L<login> methods fail. In fact, as of version 2,
the C<$@> variable will always contain error info from the last error,
so you can print that instead of calling L<LastError> if you wish. 

If you run your script with warnings turned on (which I'm sure you'll
do at some point because it's such a good idea) then any error message
that gets placed into the L<LastError> slot (and/or in C<$@>) will
automatically generate a warning. 

=head2 Transactions

RFC2060 requires that each line in an IMAP conversation be prefixed
with a tag. A typical conversation consists of the client issuing a
tag-prefixed command string, and the server replying with one of more
lines of output. Those lines of output will include a command
completion status code prefixed by the same tag as the original command
string.

The B<IMAPClient> module uses a simple counter to ensure that each
client command is issued with a unique tag value. This tag value is
referred to by the B<IMAPClient> module as the transaction number. A
history is maintained by the B<IMAPClient> object documenting each
transaction. The L<Transaction> method returns the number of the last
transaction, and can be used to retrieve lines of text from the
object's history. 

The L<Clear> parameter is used to control the size of the session
history so that long-running sessions do not eat up unreasonable
amounts of memory. See the discussion of L<Clear> under L<"Parameters">
for more information.

The L<Report> transaction returns the history of the entire IMAP
session since the initial connection or for the last I<Clear>
transactions. This provides a record of the entire conversation,
including client command strings and server responses, and is a
wonderful debugging tool as well as a useful source of raw data for
custom parsing.

=head1 CLASS METHODS

There are a couple of methods that can be invoked as class methods.
Generally they can be invoked as an object method as well, as a
convenience to the programmer. (That is, as a convenience to the
programmer who wrote this module, as well as the programmers using it.
It's easier I<not> to enforce a class method's classiness.) Note that
if the L<new> method is called as an object method, the object returned
is identical to what have would been returned if L<new> had been called
as a class method. It doesn't give you a copy of the original object or
anything like that.

=head2 new

Example:

	$imap->new(%args) or die "Could not new: $@\n";

The L<new> method creates a new instance of an B<IMAPClient> object. If
the I<Server> parameter is passed as an argument to B<new>, then B<new>
will implicitly call the L<connect> method, placing the new object in
the I<Connected> state. If I<User> and I<Password> values are also
provided, then L<connect> will in turn call L<login>, and the resulting
object will be returned from B<new> in the I<Authenticated> state.

If the I<Server> parameter is not supplied then the B<IMAPClient>
object is created in the I<Unconnected> state.

If the B<new> method is passed arguments then those arguments will be
treated as a list of key=>value pairs. The key should be one of the
parameters as documented under L<"Parameters">, below. 

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



See also L<"Parameters">, below, and L<"connect"> and L<"login"> for
information on how to manually connect and login after B<new>.

=cut

=head2 Unconnected

Example:

	$Unconnected = $imap->Unconnected();
	# or:
	$imap->Unconnected($new_value);

returns a value equal to the numerical value associated with an object
in the B<Unconnected> state.

=head2 Connected

Example:

	$Connected = $imap->Connected();
	# or:
	$imap->Connected($new_value);

returns a value equal to the numerical value associated with an object
in the B<Connected> state.

=head2 Authenticated

Example:

	$Authenticated = $imap->Authenticated();
	# or:
	$imap->Authenticated($new_value);

returns a value equal to the numerical value associated with an object
in the B<Authenticated> state.

=head2 Selected

Example:

	$Selected = $imap->Selected();
	# or:
	$imap->Selected($new_value);

returns a value equal to the numerical value associated with an object
in the B<Selected> state.

=head2 Strip_cr

Example:

	$Strip_cr = $imap->Strip_cr();
	# or:
	$imap->Strip_cr($new_value);

The B<Strip_cr> method strips carriage returns from IMAP client command
output. Although RFC2060 specifies that lines in an IMAP conversation
end with <CR><LF>, it is often cumbersome to have the carriage returns
in the returned data. This method accepts one or more lines of text as
arguments, and returns those lines with all <CR><LF> sequences changed
to <LF>. Any input argument with no carriage returns is returned
unchanged. If the first argument (not counting the class name or object
reference) is an array reference, then members of that array are
processed as above and subsequent arguments are ignored. If the method
is called in scalar context then an array reference is returned instead
of an array of results.

Taken together, these last two lines mean that you can do something
like:

	my @list = $imap->some_imap_method ;
        @list = $imap->Strip_cr(@list) ; 
	# or: 
	my $list = [ $imap->some_imap_method ] ; # returns an array ref
	$list = $imap->Strip_cr($list);

B<NOTE: Strip_cr> does not remove new line characters.

=cut

=head2 Rfc2060_date

Example:

	$Rfc2060_date = $imap->Rfc2060_date($seconds);
	# or:
	$Rfc2060_date = Mail::IMAPClient->Rfc2060_date($seconds);

The B<Rfc2060_date> method accepts one input argument, a number of
seconds since the epoch date. It returns an RFC2060 compliant date
string for that date (as required in date-related arguments to SEARCH,
such as "since", "before", etc.). 

=head2 Rfc822_date

Example:

	$Rfc822_date = $imap->Rfc822_date($seconds);
	# or:
	$Rfc822_date = Mail::IMAPClient->Rfc822_date($seconds);

The B<Rfc822_date> method accepts one input argument, a number of
seconds since the epoch date. It returns an RFC822 compliant date
string for that date (without the 'Date:' prefix). Useful for putting
dates in message strings before calling L<append>, L<search>, etcetera.


=head1 OBJECT METHODS

Object methods must be invoked against objects created via the L<new>
method. They cannot be invoked as class methods, which is why they are
called "object methods" and not "class methods". 

There are basically two types of object methods--mailbox methods, which 
participate in the IMAP session's conversation (i.e. they issue IMAP 
client commands) and object control methods, which do not result in 
IMAP commands but which may affect later commands or provide details
of previous ones. This latter group can be further broken down into
two types, Parameter accessor methods, which affect the behavior of 
future mailbox methods, and Status methods, which report on the affects
of previous mailbox methods.

Methods that do not result in new IMAP client commands being issued 
(such as the L<Transaction>, L<Status>, and L<History> methods) all 
begin with an uppercase letter, to distinguish them from methods that 
do correspond to IMAP client commands. Class methods and eponymous 
parameter methods likewise begin with an uppercase letter because 
they also do not correspond to an IMAP client command.

As a general rule, mailbox control methods return C<undef> on failure 
and something besides C<undef> when they succeed. This rule is modified 
in the case of methods that return search results. When called in a list 
context, searches that do not find matching results return an empty list. 
When called in a scalar context, searches with no hits return 'undef' 
instead of an array reference. If you want to know why you received no hits,
you should check C<$@>, which will be empty if the search was successful
but had no matching results but populated with an error message if the 
search encountered a problem (such as invalid parameters).

A number of IMAP commands do not have corresponding B<Mail::IMAPClient>
methods. Instead, they are implemented via a default method and Perl's 
L<AUTOLOAD|perlsub/autoload> facility. If you are looking for a specific
IMAP client command (or IMAP extension) and cannot find it here then
that does not necessarily mean you can not use B<Mail::IMAPClient> to
issue the command. In fact, you can issue virtually any IMAP client
command simply by I<pretending> that there is a B<Mail::IMAPClient> method.
See the section on 
L<"Other IMAP Client Commands and the Default Object Method">
below for details on the default method.

=head1 Mailbox Control Methods

=head2 append

Example:

	my $uid = $imap->append($folder,$msg_text) 
		or die "Could not append: $@\n";

The B<append> method adds a message to the specified folder. It takes
two arguments, the name of the folder to append the message to, and the
text of the message (including headers). Additional arguments are added
to the message text, separated with <CR><LF>.

The B<append> method returns the UID of the new message (a true value)
if successful, or C<undef> if not, if the IMAP server has the UIDPLUS
capability. If it doesn't then you just get true on success and undef
on failure.

Note that many servers will get really ticked off if you try to append
a message that contains "bare newlines", which is the titillating term
given to newlines that are not preceded by a carrage return. To protect
against this, B<append> will insert a carrage return before any newline
that is "bare". If you don't like this behavior then you can avoid it
by not passing naked newlines to B<append>.

Note that B<append> does not allow you to specify the internal date or
initial flags of an appended message. If you need this capability then
use L<append_string>, below.

=cut

=head2 append_file

Example:

	my $new_msg_uid = $imap->append_file(
		$folder,
		$filename 
		[ , $input_record_separator ]	# optional (not arrayref)
	) 	or die "Could not append_file: $@\n";

The B<append_file> method adds a message to the specified folder. It
takes two arguments, the name of the folder to append the message to,
and the file name of an RFC822-formatted message.

An optional third argument is the value to use for
C<input_record_separator>. The default is to use "" for the first read
(to get the headers) and "\n" for the rest. Any valid value for C<$/>
is acceptable, even the funky stuff, like C<\1024>. (See L<perlvar|perlvar> 
for more information on C<$/>). (The brackets in the example indicate
that this argument is optional; they do not mean that the argument 
should be an array reference.)

The B<append_file> method returns the UID of the new message (a true
value) if successful, or C<undef> if not, if the IMAP server has the
UIDPLUS capability. If it doesn't then you just get true on success and
undef on failure. If you supply a filename that doesn't exist then you
get an automatic C<undef>. The L<LastError> method will remind you of this
if you forget that your file doesn't exist but somehow manage to
remember to check L<LastError>.

In case you're wondering, B<append_file> is provided mostly as a way to
allow large messages to be appended without having to have the whole
file in memory. It uses the C<-s> operator to obtain the size of the
file and then reads and sends the contents line by line (or not,
depending on whether you supplied that optional third argument).

=cut

=head2 append_string

Example:

	# brackets indicate optional arguments (not array refs):

	my $uid = $imap->append_string( $folder, $text [ , $flags [ , $date ] ]) 	
		or die "Could not append_string: $@\n";

The B<append_string> method adds a message to the specified folder. It
requires two arguments, the name of the folder to append the message
to, and the text of the message (including headers). The message text
must be included in a single string (unlike L<append>, above).

You can optionally specify a third and fourth argument to
B<append_string>. The third argument, if supplied, is the list of flags
to set for the appended message. The list must be specified as a
space-separated list of flags, including any backslashes that may be
necessary. The enclosing parentheses that are required by RFC2060 are
optional for B<append_string>. The fourth argument, if specified, is
the date to set as the internal date. It should be in the format
described for I<date_time> fields in RFC2060, i.e. "dd-Mon-yyyy
hh:mm:ss +0000".

If you want to specify a date/time but you don't want any flags then
specify I<undef> as the third argument.

The B<append_string> method returns the UID of the new message (a true
value) if successful, or C<undef> if not, if the IMAP server has the
UIDPLUS capability. If it doesn't then you just get true on success and
undef on failure.

Note that many servers will get really ticked off if you try to append
a message that contains "bare newlines", which is the titillating term
given to newlines that are not preceded by a carrage return. To protect
against this, B<append_string> will insert a carrage return before any
newline that is "bare". If you don't like this behavior then you can
avoid it by not passing naked newlines to B<append_string>.

=cut

=head2 authenticate

Example:

	$imap->authenticate($authentication_mechanism, $coderef) 
		or die "Could not authenticate: $@\n";

The B<authenticate> method accepts two arguments, an authentication
type to be used (ie CRAM-MD5) and a code or subroutine reference to
execute to obtain a response. The B<authenticate> method assumes that 
the authentication type specified in the first argument follows a
challenge-response flow. The B<authenticate> method issues the IMAP
Client AUTHENTICATE command and receives a challenge from the server.
That challenge (minus any tag prefix or enclosing '+' characters but
still in the original base64 encoding) is passed as the only argument
to the code or subroutine referenced in the second argument. The return
value from the 2nd argument's code is written to the server as is,
except that a <CR><NL> sequence is appended if neccessary.

If you are interested in doing NTLM authentication then please see Mark
Bush's L<Authen::NTLM>, which can work with B<Mail::IMAPClient> to
provide NTLM authentication.

See also the L<login> method, which is the simplest form of
authentication defined by RFC2060.

=cut

=head2 before

Example:

	my @msgs = $imap->before($Rfc2060_date) 
		or warn "No messages found before $Rfc2060_date.\n";

The B<before> method works just like the L<"since"> method, below,
except it returns a list of messages whose internal system dates are
before the date supplied as the argument to the B<before> method.

=cut

=head2 body_string

Example:

	my $string = $imap->body_string($msgId) 
		or die "Could not body_string: $@\n";

The B<body_string> method accepts a message sequence number (or a
message UID, if the L<Uid> parameter is set to true) as an argument and
returns the message body as a string. The returned value contains the
entire message in one scalar variable, without the message headers.

=cut

=head2 bodypart_string

Example:

	my $string=$imap->bodypart_string( 	$msgid, $part_number , 
						$length ,$offset  
	) 	or die "Could not get bodypart string: $@\n";


The B<bodypart_string> method accepts a message sequence number (or a
message UID, if the L<Uid> parameter is set to true) and a body part as
arguments and returns the message part as a string. The returned value
contains the entire message part (or, optionally, a portion of the part) 
in one scalar variable.

If an optional third argument is provided, that argument is the number
of bytes to fetch. (The default is the whole message part.) If an
optional fourth argument is provided then that fourth argument is the
offset into the part at which the fetch should begin. The default is
offset zero, or the beginning of the message part.

If you specify an offset without specifying a length then the offset
will be ignored and the entire part will be returned.

B<bodypart_string> will return C<undef> if it encounters an error.

=cut

=head2 capability

Example:

	my @features = $imap->capability
		or die "Could not determine capability: $@\n";

The B<capability> method returns an array of capabilities as returned
by the CAPABILITY IMAP Client command, or a reference to an array of
capabilities if called in scalar context. If the CAPABILITY IMAP Client
command fails for any reason then the B<capability> method will return
C<undef>.

=head2 close

Example:

	$imap->close or die "Could not close: $@\n";

The B<close> method is implemented via the default method and is used
to close the currently selected folder via the CLOSE IMAP client
command. According to RFC2060, the CLOSE command performs an implicit
EXPUNGE, which means that any messages that you've flagged as
I<\Deleted> (say, with the L<delete_message> method) will now be
deleted. If you haven't deleted any messages then B<close> can be
thought of as an "unselect".

Note again that this closes the currently selected folder, not the 
IMAP session.

See also L<delete_message>, L<expunge>, and your tattered copy of
RFC2060.

=head2 connect

Example:

	$imap->connect or die "Could not connect: $@\n";

The B<connect> method connects an imap object to the server. It returns
C<undef> if it fails to connect for any reason. If values are available
for the I<User> and I<Password> parameters at the time that B<connect>
is invoked, then B<connect> will call the L<login> method after
connecting and return the result of the L<login> method to B<connect>'s
caller. If either or both of the I<User> and I<Password> parameters are
unavailable but the connection to the server succeeds then B<connect>
returns a pointer to the B<IMAPClient> object.

The I<Server> parameter must be set (either during L<new> method
invocation or via the L<Server> object method) before invoking
B<connect>. If the L<Server> parameter is supplied to the L<new> method
then B<connect> is implicitly called during object construction.

The B<connect> method sets the state of the object to C<connected> if
it successfully connects to the server. It returns C<undef> on failure.

=head2 copy

Example:

	# Here brackets indicate optional arguments:
	my $uidList = $imap->copy($folder, $msg_1 [ , ... , $msg_n ]) 
	or die "Could not copy: $@\n";

Or:
	# Now brackets indicate an array ref!
	my $uidList = $imap->copy($folder, [ $msg_1, ... , $msg_n ]) 
	or die "Could not copy: $@\n";


The B<copy> method requires a folder name as the first argument, and a
list of one or more messages sequence numbers (or messages UID's, if
the I<UID> parameter is set to a true value). The message sequence
numbers or UID's should refer to messages in the currenly selected
folder. Those messages will be copied into the folder named in the
first argument.

The B<copy> method returns C<undef> on failure and a true value if
successful. If the server to which the current Mail::IMAPClient object
is connected supports the UIDPLUS capability then the true value
returned by B<copy> will be a comma separated list of UID's, which are
the UID's of the newly copied messages in the target folder. 

=cut

=head2 create

Example:

	$imap->create($new_folder) 
		or die "Could not create $new_folder: $@\n";

The B<create> method accepts one argument, the name of a folder (or
what RFC2060 calls a "mailbox") to create. If you specifiy additional
arguments to the B<create> method and your server allows additional
arguments to the CREATE IMAP client command then the extra argument(s)
will be passed to your server. 

If you specifiy additional arguments to the B<create> method and your
server does not allow additional arguments to the CREATE IMAP client
command then the extra argument(s) will still be passed to your server
and the create will fail, so don't do that.

B<create> returns a true value on success and C<undef> on failure, as
you've probably guessed.

=head2 delete

Example:

	$imap->delete($folder) or die "Could not delete $folder: $@\n";

The B<delete> method accepts a single argument, the name of a folder to
delete. It returns a true value on success and C<undef> on failure.

=head2 delete_message

Example:

	my @msgs = $imap->seen;
	$imap->delete_message(\@msgs) 
		or die "Could not delete_message: $@\n";

The above could also be rewritten like this:

	# scalar context returns array ref
	my $msgs = scalar($imap->seen);	

	$imap->delete_message($msgs) 
		or die "Could not delete_message: $@\n";

Or, as a one-liner:


	$imap->delete_message( scalar($imap->seen) )
		or die "Could not delete_message: $@\n";

The B<delete_message> method accepts a list of arguments. If the L<Uid>
parameter is not set to a true value, then each item in the list should
be either: 

=over 4

=item >

a message sequence number,

=item >

a comma-separated list of message sequence numbers, 

=item >

a reference to an array of message sequence numbers, or

=back

If the L<Uid> parameter is set to a true value, then each item in the
list should be either: 

=over 4

=item >

a message UID, 

=item >

a comma-separated list of UID's, or 

=item >

a reference to an array of message UID's.

=back

The messages identified by the sequence numbers or UID's will be
deleted. If successful, B<delete_message> returns the number 
of messages it was told to delete. However, since the delete is 
done by issuing the I<+FLAGS.SILENT> option of the STORE IMAP 
client command, there is no guarantee that the delete was successful 
for every message. In this manner the B<delete_message> method sacrifices 
accuracy for speed. Generally, though, if a single message in a list 
of messages fails to be deleted it's because it was already deleted,
which is what you wanted anyway so why worry about it? If there is
a more severe error, i.e. the server replies "NO", "BAD", or, 
banish the thought, "BYE", then B<delete_message> will return C<undef>.

If you must have guaranteed results then use the IMAP STORE client
command (via the default method) and use the +FLAGS (\Deleted) option,
and then parse your results manually. Eg: 

	$imap->store($msg_id,'+FLAGS (\Deleted)');
	my @results = $imap->History($imap->Transaction);
	...			# code to parse output goes here



The B<IMAPClient> object must be in C<Selected> status to use the
B<delete_message> method. 

B<NOTE:> All the messages identified in the input argument(s) must be
in the currently selected folder. Failure to comply with this
requirement will almost certainly result in the wrong message(s) being
deleted. This would be a crying shame. 

B<NOTE SOME MORE:> In the grand tradition of the IMAP protocol,
deleting a message doesn't actually delete the message. Really. If you
want to make sure the message has been deleted, you need to expunge the
folder (via the L<expunge> method, which is implemented via the default
method). Or at least L<close> it. This is generally considered a
feature, since after deleting a message, you can change your mind and
undelete it at any time before your L<expunge> or L<close>.

I<See also:> The L<delete> method, to delete a folder, the L<expunge>
method, to expunge a folder, the L<restore_message> method to undelete
a message, and the L<close> method (implemented here via the default
method) to close a folder. Oh, and don't forget about RFC2060. 

=cut

=head2 deny_seeing

Example:

	# Reset all read msgs to unread 
	# (produces error if there are no seen msgs):
	$imap->deny_seeing( scalar($imap->seen) ) 
		or die "Could not deny_seeing: $@\n" ;

The B<deny_seeing> method accepts a list of one or more message
sequence numbers, or a single reference to an array of one or more
message sequence numbers, as its argument(s). It then unsets the
"\Seen" flag for those messages. Of course, if the L<Uid> parameter is
set to a true value then those message sequence numbers should be
unique message id's.

Note that specifying C<$imap-E<gt>deny_seeing(@msgs)> is just a
shortcut for specifying C<$imap-E<gt>unset_flag("Seen",@msgs)>. 

=cut

=head2 disconnect

Example:

	$imap->disconnect or warn "Could not disconnect: $@\n";

Disconnects the B<IMAPClient> object from the server. Functionally
equivalent to the L<logout> method. (In fact it's actually a synonym
for L<logout>.)

=cut

=head2 examine

Example:

	$imap->examine($folder) or die "Could not examine: $@\n";

The B<examine> method selects a folder in read-only mode and changes
the object's state to "Selected". The folder selected via the
B<examine> method can be examined but no changes can be made unless it
is first selected via the L<select> method. 

The B<examine> method accepts one argument, which is the name of the
folder to select. 

=cut

=head2 exists

Example:

	$imap->exists($folder) or warn "$folder not found: $@\n";

Accepts one argument, a folder name. Returns true if the folder exists
or false if it does not exist.

=cut

=head2 expunge

Example:

	$imap->expunge($folder) or die "Could not expunge: $@\n";

The B<expunge> method accepts one optional argument, a folder name. It
expunges the folder specified as the argument, or the currently
selected folder if no argument is supplied. 

Although RFC2060 does not permit optional arguments (like a folder
name) to the EXPUNGE client command, the L<expunge> method does, which
is especially interesting given that the L<expunge> method doesn't
technically exist. In case you're curious, expunging a folder deletes
the messages that you thought were already deleted via
L<delete_message> but really weren't, which means you have to use a
method that doesn't exist to delete messages that you thought didn't
exist. (Seriously, I'm not making any of this stuff up.)

Or you could use the L<close> method, which de-selects as well as
expunges and which likewise doesn't technically exist. As with any IMAP
client command, that fact that these methods don't exist will not stop
them from working anyway. This is a feature of the B<Mail::IMAPClient>
module. (See L<"Other IMAP Client Commands and the Default Object Method"> 
if you still don't believe me.)

=cut

=head2 fetch

Example:

	my $output = $imap->fetch(@args) or die "Could not fetch: $@\n";

The B<fetch> method implements the FETCH IMAP client command. It
accepts a list of arguments, which will be converted into a
space-delimited list of arguments to the FETCH IMAP client command. If
no arguments are supplied then B<fetch> does a FETCH ALL. If the L<Uid>
parameter is set to a true value then the first argument will be
treated as a UID or list of UID's, which means that the UID FETCH IMAP
client command will be run instead of FETCH. (It would really be a good
idea at this point to review RFC2060.) 

If called in array context, B<fetch> will return an array of output
lines. The output lines will be returned just as they were received
from the server, so your script will have to be prepared to parse out
the bits you want. The only exception to this is literal strings, which
will be inserted into the output line at the point at which they were
encountered (without the {nnn} literal field indicator). See RFC2060
for a description of literal fields.

If B<fetch> is called in a scalar context, then a reference to an array
(as described above) is returned instead of the entire array. 

B<fetch> returns C<undef> on failure. Inspect L<LastError> or C<$@> for
an explanation of your error.

=cut

=head2 flags

Example:

	my @flags = $imap->flags($msgid) 
		or die "Could not flags: $@\n";

The B<flags> method implements the FETCH IMAP client command to list a
single message's flags. It accepts one argument, a message sequence
number (or a message UID, if the L<Uid> parameter is true), and returns
an array (or a reference to an array, if called in scalar context)
listing the flags that have been set. Flag names are provided with
leading backslashes.

As of version 1.11, you can supply either a list of message id's or a
reference to an array of of message id's (which means either sequence
number, if the Uid parameter is false, or message UID's, if the Uid
parameter is true) instead of supplying a single message sequence
number or UID. If you do, then the return value will not be an array or
array reference; instead, it will be a hash reference, with each key
being a message sequence number (or UID) and each value being a
reference to an array of flags set for that message.

For example, if you want to display the flags for every message in the
folder where you store e-mail related to your plans for world
domination, you could do something like this:

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

=head2 folders

Example:

	$imap->folders	or die "Could not list folders: $@\n";

The B<folders> method returns an array listing the available folders.
It will only be successful if the object is in the I<Authenticated> or
I<Selected> states.

The B<folders> argument accepts one optional argument, which is a prefix.
If a prefix is supplied to the B<folders> method, then only folders beginning 
with the prefix will be returned. 

For example:

	print join(", ",$imap->folders),".\n";
	# Prints: 
	# INBOX, Sent, Projects, Projects/Completed, Projects/Ongoing, Projects Software.
	print join(", ",$imap->folders("Projects"),".\n";
	# Prints: 
	# Projects, Projects/Completed, Projects/Ongoing, Projects Software.
	print join(", ",$imap->folders("Projects" . $imap->separator),".\n";
	# Prints: 
	# Projects/Completed, Projects/Ongoing

Notice that if you just want to list a folder's subfolders (and not the 
folder itself), then you need to include the hierarchy separator character 
(as returned by the L<separator> method).

=cut

=head2 has_capability

Example:

	my $has_feature = $imap->has_capability($feature) 
		or die "Could not do has_capability($feature): $@\n";

Returns true if the IMAP server to which the B<IMAPClient> object is
connected has the capability specified as an argument to
B<has_capability>.

=head2 imap4rev1

Example:

	$imap->imap4rev1 or die "Could not imap4rev1: $@\n";

Returns true if the IMAP server to which the B<IMAPClient> object is
connected has the IMAP4REV1 capability.

=head2 internaldate

Example:

	my $msg_internal_date = $imap->internaldate($msgid) 
		or die "Could not internaldate: $@\n";

B<internaldate> accepts one argument, a message id (or UID if the
L<Uid> parameter is true), and returns that message's internal date.

=head2 get_bodystructure

Example:

	my $bodyStructObject = $imap->get_bodystructure($msgid) 
		or die "Could not get_bodystructure: $@\n";

The B<get_bodystructure> method accepts one argument, a message
sequence number or, if L<Uid> is true, a message UID. It obtains the
message's body structure and returns a parsed
L<Mail::IMAPClient::BodyStructure> object for the message.

=head2 get_envelope

Example:

	my $envObject = $imap->get_envelope(@args) 
		or die "Could not get_envelope: $@\n";

The B<get_envelope> method accepts one argument, a message sequence
number or, if L<Uid> is true, a message UID. It obtains the message's
envelope and returns a B<Mail::IMAPClient::BodyStructure::Envelope>
object for the envelope, which is just a version of the envelope that's
been parsed into a perl object.

For more information on how to use this object once you've gotten it,
see the L<Mail::IMAPClient::BodyStructure> documention. (As of this
writing there is no separate pod document for
B<Mail::IMAPClient::BodyStructure::Envelope>.)

=head2 getacl

Example:

	my $hash = $imap->getacl($folder) 
		or die "Could not getacl for $folder: $@\n";

B<getacl> accepts one argument, the name of a folder. If no argument is
provided then the currently selected folder is used as the default. It
returns a reference to a hash. The keys of the hash are userids that
have access to the folder, and the value of each element are the
permissions for that user. The permissions are listed in a string in
the order returned from the server with no whitespace or punctuation
between them.

=cut

=head2 is_parent

Example:

	my $hasKids = $imap->is_parent($folder) ;

The B<is_parent> method accepts one argument, the name of a folder. It
returns a value that indicates whether or not the folder has children.
The value it returns is either 1) a true value (indicating that the
folder has children), 2) 0 if the folder has no children at this time,
or 3) C<undef> if the folder is not permitted to have children.

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

=head2 list

Example:

	my @raw_output = $imap->list(@args) 
		or die "Could not list: $@\n";

The B<list> method implements the IMAP LIST client command. Arguments
are passed to the IMAP server as received, separated from each other by
spaces. If no arguments are supplied then the default list command
C<tag LIST "" '*'> is issued.

The B<list> method returns an array (or an array reference, if called
in a scalar context). The array is the unadulterated output of the LIST
command. (If you want your output adulterated then see the L<folders>
method, above.)

=cut

=head2 listrights

Example:

	$imap->listrights($folder,$user) 
		or die "Could not listrights: $@\n";

The B<listrights> method implements the IMAP LISTRIGHTS client command
(L<RFC2086>). It accepts two arguments, the foldername and a user id.
It returns the rights the specified user has for the specified folder.
If called in a scalar context then the rights are returned a strings, with
no punction or whitespace or any nonsense like that. If called in array
context then B<listrights> returns an array in which each element is one
right.

=head2 login

Example:

	$imap->login or die "Could not login: $@\n";

The B<login> method uses the IMAP LOGIN client command (as defined in
RFC2060) to log into the server. The I<User> and I<Password> parameters
must be set before the B<login> method can be invoked. If successful,
the B<login> method returns a pointer to the B<IMAPClient> object and
sets the object status to I<Authenticated>. If unsuccessful, it returns
undef. See the L<new> method for more information on how B<login> can
be called automatically from L<new>.

B<login> is sometimes called automatically by L<connect>, which in turn
is sometimes called automatically by L<new>. You can predict this 
behavior once you've read the section on the L<new> method.

=cut

=head2 logout

Example:

	$imap->logout or die "Could not logout: $@\n";

The B<logout> method issues the LOGOUT IMAP client commmand. Since the
LOGOUT IMAP client command causes the server to end the connection,
this also results in the B<IMAPClient> client entering the
I<Unconnected> state. This method does not, however, destroy the
B<IMAPClient> object, so a program can re-invoke the L<connect> and
L<login> methods if it wishes to reestablish a session later in the
program.

=cut

=head2 lsub

Example:

	$imap->lsub(@args) or die "Could not lsub: $@\n";

The B<lsub> method implements the IMAP LSUB client command. Arguments
are passed to the IMAP server as received, separated from each other
by spaces. If no arguments are supplied then the default lsub command
C<tag LSUB "" '*'> is issued.

The B<lsub> method returns an array (or an array reference, if called
in a scalar context). The array is the unaltered output of the LSUB
command. If you want an array of subscribed folders then see the
L<subscribed> method, below.

=cut

=head2 mark

Example:

	$imap->mark(@msgs) or die "Could not mark: $@\n";

The B<mark> method accepts a list of one or more messages sequence
numbers, or a single reference to an array of one or more message
sequence numbers, as its argument(s). It then sets the "\Flagged" flag
for those message(s). Of course, if the L<Uid> parameter is set to a
true value then those message sequence numbers had better be unique
message id's.

Note that specifying C<$imap-E<gt>see(@msgs)> is just a shortcut for
specifying C<$imap-E<gt>set_flag("Flagged",@msgs)>. 

=cut

=head2 message_count

Example:

	my $msgcount = $imap->message_count($folder) or die "Could not message_count: $@\n";

The B<message_count> method accepts the name of a folder as an argument
and returns the number of messages in that folder. Internally, it
invokes the L<status> method (see above) and parses out the results to
obtain the number of messages. If you don't supply an argument to
B<message_count> then it will return the number of messages in the
currently selected folder (assuming of course that you've used the
L<select> or L<examine> method to select it instead of trying something
funky). Note that RFC2683 contains warnings about the use of the IMAP
I<STATUS> command (and thus the L<status> method and therefore the 
B<message_count> method) against the currenlty selected folder.
You should carefully consider this before using B<message_count> 
on the currently selected folder. You may be better off using 
L<search> or one of its variants (especially L<messages>), and then
counting the results. On the other hand, I regularly violate this
rule on my server without suffering any dire consequences. Your 
milage may vary.

=cut

=head2 message_string

Example:

	my $string = $imap->message_string($msgid) 
		or die "Could not message_string: $@\n";

The B<message_string> method accepts a message sequence number (or
message UID if L<Uid> is true) as an argument and returns the message
as a string. The returned value contains the entire message in one
scalar variable, including the message headers. Note that using this
method will set the message's "\Seen" flag as a side effect, unless
I<Peek> is set to a true value.

=cut

=head2 message_to_file

Example:

	$imap->message_to_file($file,@msgs) 
		or die "Could not message_to_file: $@\n";

The B<message_to_file> method accepts a filename or file handle and one
or more message sequence numbers (or message UIDs if L<Uid> is true) as
arguments and places the message string(s) (including RFC822 headers)
into the file named in the first argument (or prints them to the
filehandle, if a filehandle is passed). The returned value is true on
succes and C<undef> on failure. 

If the first argument is a reference, it is assumed to be an open
filehandle and will not be closed when the method completes, If it is a
file, it is opened in append mode, written to, then closed.

Note that using this method will set the message's "\Seen" flag as a
side effect. But you can use the L<deny_seeing> method to set it back,
or set the L<Peek> parameter to a true value to prevent setting the
"\Seen" flag at all.

This method currently works by making some basic assumptions about the
server's behavior, notably that the message text will be returned as a
literal string but that nothing else will be. If you have a better idea
then I'd like to hear it. 

=cut

=head2 message_uid

Example:

	my $msg_uid = $imap->message_uid($msg_seq_no) 
		or die "Could not get uid for $msg_seq_no: $@\n";

The B<message_uid> method accepts a message sequence number (or message
UID if L<Uid> is true) as an argument and returns the message's UID.
Yes, if L<Uid> is true then it will use the IMAP UID FETCH UID client
command to obtain and return the very same argument you supplied. This
is an IMAP feature so don't complain to me about it.

=cut

=head2 messages

Example:

	# Get a list of messages in the current folder:
	my @msgs = $imap->messages or die "Could not messages: $@\n";
	# Get a reference to an array of messages in the current folder:
	my $msgs = $imap->messages or die "Could not messages: $@\n";

If called in list context, the B<messages> method returns a list of all
the messages in the currenlty selected folder. If called in scalar
context, it returns a reference to an array containing all the messages
in the folder. If you have the L<Uid> parameter turned off, then this
is the same as specifying C<1 ... $imap-E<gt>L<message_count>>; if you
have UID set to true then this is the same as specifying
C<$imap-E<gt>L<search>("ALL")>. 

=cut

=head2 migrate

Example:

	$imap->migrate($imap_2, "ALL", $targetFolder ) 
		or die "Could not migrate: $@\n";

The B<migrate> method copies the indicated messages B<from> the
currently selected folder B<to> another B<Mail::IMAPClient> object's
session. It requires these arguments:

=over 4

=item 1. 

a reference to the target B<Mail::IMAPClient> object (not the calling
object, which is connected to the source account);

=item 2.

the message(s) to be copied, specified as either a) the message
sequence number (or message UID if the UID parameter is true) of a
single message, b) a reference to an array of message sequence numbers
(or message UID's if the UID parameter is true) or c) the special
string "ALL", which is a shortcut for the results of
C<L<search>("ALL")>.

=item 3.

the folder name of a folder on the target mailbox to receive the
message(s). If this argument is not supplied or if I<undef> is supplied
then a folder with the same name as the currently selected folder on
the calling object will be created if necessary and used. If you
specify something other then I<undef> for this argument, even if it's
'$imap1-E<gt>Folder' or the name of the currently selected folder, then
that folder will only be used if it exists on the target object's
mailbox; if it does not exist then B<migrate> will fail.

=back

The target B<Mail::IMAPClient> object should not be the same as the
source. The source object is the calling object, i.e. the one whose
B<migrate> method will be used. It cannot be the same object as the one
specified as the target, even if you are for some reason migrating
between folders on the same account (which would be silly anyway, since
L<copy> can do that much more efficiently). If you try to use the same
B<Mail::IMAPClient> object for both the caller and the reciever then
they'll both get all screwed up and it will be your fault because I
just warned you and you didn't listen.

B<migrate> will download messages from the source in chunks to minimize
memory usage. The size of the chunks can be controlled by changing the
source B<Mail::IMAPClient> object's the L<Buffer> parameter. The higher
the L<Buffer> value, the faster the migration, but the more memory your
program will require. TANSTAAFL. (See the L<Buffer> parameter and
eponymous accessor method, described above under the L<"Parameters">
section.)

The B<migrate> method uses Black Magic to hardwire the I/O between the
two B<Mail::IMAPClient> objects in order to minimize resource
consumption. If you have older scripts that used L<message_to_file> and
L<append_file> to move large messages between IMAP mailboxes then you
may want to try this method as a possible replacement.

=head2 move

Example:

	my $newUid = $imap->move($newFolder, $oldUid) 
		or die "Could not move: $@\n";
	$imap->expunge;

The B<move> method moves messages from the currently selected folder to
the folder specified in the first argument to B<move>. If the L<Uid>
parameter is not true, then the rest of the arguments should be either:

=over 4

=item >

a message sequence number,

=item >

a comma-separated list of message sequence numbers, or

=item >

a reference to an array of message sequence numbers.

=back

If the L<Uid> parameter is true, then the arguments should be:

=over 4

=item >

a message UID,

=item >

a comma-separated list of message UID's, or

=item >

a reference to an array of message UID's.

=back

If the target folder does not exist then it will be created.

If move is sucessful, then it returns a true value. Furthermore, if the
B<Mail::IMAPClient> object is connected to a server that has the
UIDPLUS capability, then the true value will be the comma-separated
list of UID's for the newly copied messages. The list will be in the
order in which the messages were moved. (Since B<move> uses the copy
method, the messages will be moved in numerical order.)

If the move is not successful then B<move> returns C<undef>.

Note that a move really just involves copying the message to the new
folder and then setting the I<\Deleted> flag. To actually delete the
original message you will need to run L<expunge> (or L<close>).

=cut

=head2 namespace

Example:

	my @refs = $imap->namespace
		or die "Could not namespace: $@\n";

The namespace method runs the NAMESPACE IMAP command (as defined in RFC
2342). When called in a list context, it returns a list of three
references. Each reference looks like this:

	[ [ $prefix_1, $separator_1 ] , 
	  [ $prefix_2, $separator_2 ], 
	  [ $prefix_n , $separator_n] 
	]

The first reference provides a list of prefices and separator
charactors for the available personal namespaces. The second reference
provides a list of prefices and separator charactors for the available
shared namespaces. The third reference provides a list of prefices and
separator charactors for the available public namespaces.

If any of the three namespaces are unavailable on the current server
then an 'undef' is returned instead of a reference. So for example if
shared folders were not supported on the server but personal and public
namespaces were both available (with one namespace each), the returned
value might resemble this: 

	( [ "", "/" ] , undef, [ "#news", "." ] ) ;

If the B<namespace> method is called in scalar context, it returns a
reference to the above-mentioned list of three references, thus
creating a single structure that would pretty-print something like
this:

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

Or, to look at our previous example (where shared folders are
unsupported) called in scalar context:

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

=head2 on

Example:

	my @msgs = $imap->on($Rfc2060_date) 
		or warn "Could not find messages sent on $Rfc2060_date: $@\n";

The B<on> method works just like the L<since> method, below, except it
returns a list of messages whose internal system dates are the same as
the date supplied as the argument.

=head2 parse_headers 

Example:

	my $hashref = $imap->parse_headers($msg||@msgs, "Date", "Subject") 
		or die "Could not parse_headers: $@\n";

The B<parse_headers> method accepts as arguments a message sequence
number and a list of header fields. It returns a hash reference in
which the keys are the header field names (without the colon) and the
values are references to arrays of values. A picture would look
something like this:

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

The text in the example for the "Received" array has been formated to
make reading the example easier. The actual values returned are just
strings of words separated by spaces and with newlines and carriage
returns stripped off. The I<Received> header is probably the main
reason that the B<parse_headers> method creates a hash of lists rather
than a hash of values. 

If the second argument to B<parse_headers> is 'ALL' or if it is
unspecified then all available headers are included in the returned
hash of lists.

If you're not emotionally prepared to deal with a hash of lists then
you can always call the L<fetch> method yourself with the appropriate
parameters and parse the data out any way you want to. Also, in the
case of headers whose contents are also reflected in the envelope, you
can use the L<get_envelope> method as an alternative to
L<parse_headers>.

If the L<Uid> parameter is true then the first argument will be treated
as a message UID. If the first argument is a reference to an array of
message sequence numbers (or UID's if L<Uid> is true), then
B<parse_headers> will be run against each message in the array. In this
case the return value is a hash, in which the key is the message
sequence number (or UID) and the value is a reference to a hash as
described above.

An example of using B<parse_headers> to print the date and subject of
every message in your smut folder could look like this:

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

=head2 recent

Example:

	my @recent = $imap->recent or warn "No recent msgs: $@\n";

The B<recent> method performs an IMAP SEARCH RECENT search against the
selected folder and returns an array of sequence numbers (or UID's, if
the L<Uid> parameter is true) of messages that are recent.

=cut

=head2 recent_count

Example:

	my $count = 0;
	defined($count = $imap->recent_count($folder)) 
		or die "Could not recent_count: $@\n";

The B<recent_count> method accepts as an argument a folder name. It
returns the number of recent messages in the folder (as returned by the
IMAP client command "STATUS folder RECENT"), or C<undef> in the case of an
error. The B<recent_count> method was contributed by Rob Deker
(deker@ikimbo.com).

=cut

=head2 rename

Example:

	$imap->rename($oldname,$nedwname) 
		or die "Could not rename: $@\n";

The B<rename> method accepts two arguments: the name of an existing
folder, and a new name for the folder. The existing folder will be
renamed to the new name using the RENAME IMAP client command. B<rename>
will return a true value if successful, or C<undef> if unsuccessful.

=cut

=head2 restore_message

Example:

	$imap->restore_message(@msgs) or die "Could not restore_message: $@\n";

The B<restore_message> method is used to undo a previous
L<delete_message> operation (but not if there has been an intervening
L<expunge> or L<close>). The B<IMAPClient> object must be in
L<Selected> status to use the B<restore_message> method. 

The B<restore_message> method accepts a list of arguments. If the
L<Uid> parameter is not set to a true value, then each item in the list
should be either:

=over 4

=item >

a message sequence number,

=item >

a comma-separated list of message sequence numbers,

=item >

a reference to an array of message sequence numbers, or

=back

If the L<Uid> parameter is set to a true value, then each item in the
list should be either:

=over 4

=item >

a message UID,

=item >

a comma-separated list of UID's, or

=item >

a reference to an array of message UID's.

=back

The messages identified by the sequence numbers or UID's will have
their I<\Deleted> flags cleared, effectively "undeleting" the messages.
B<restore_message> returns the number of messages it was able to
restore. 

Note that B<restore_messages> is similar to calling
C<L<unset_flag>("\Deleted",@msgs)>, except that B<restore_messages>
returns a (slightly) more meaningful value. Also it's easier to type.

=cut

=head2 run

Example:

	$imap->run(@args) or die "Could not run: $@\n";

Like Perl itself, the B<Mail::IMAPClient> module is designed to make
common things easy and uncommon things possible. The B<run> method is
provided to make those uncommon things possible.

The B<run> method excepts one or two arguments. The first argument is a
string containing an IMAP Client command, including a tag and all
required arguments. The optional second argument is a string to look
for that will indicate success. (The default is C</OK.*/>). The B<run>
method returns an array of output lines from the command, which you are
free to parse as you see fit.

The B<run> method does not do any syntax checking, other than
rudimentary checking for a tag.

When B<run> processes the command, it increments the transaction count
and saves the command and responses in the History buffer in the same
way other commands do. However, it also creates a special entry in the
History buffer named after the tag supplied in the string passed as the
first argument. If you supply a numeric value as the tag then you may
risk overwriting a previous transaction's entry in the History buffer.

If you want the control of B<run> but you don't want to worry about the
damn tags then see L<"tag_and_run">, below.

=cut

=head2 search

Example:

	my @msgs = $imap->search(@args) or warn "search: None found\n";
	if ($@) {
		warn "Error in search: $@\n";
	}

The B<search> method implements the SEARCH IMAP client command. Any
argument supplied to B<search> is prefixed with a space and appended to
the SEARCH IMAP client command. This method is another one of those
situations where it will really help to have your copy of RFC2060
handy, since the SEARCH IMAP client command contains a plethora of
options and possible arguments. I'm not going to repeat them here. 

Remember that if your argument needs quotes around it then you must
make sure that the quotes will be preserved when passing the argument.
I.e. use C<qq/"$arg"/> instead of C<"$arg">.

The B<search> method returns an array containing sequence numbers of
messages that passed the SEARCH IMAP client command's search criteria.
If the L<Uid> parameter is true then the array will contain message
UID's. If B<search> is called in scalar context then a pointer to the
array will be passed, instead of the array itself. If no messages meet
the criteria then B<search> returns an empty list (when in list context)
or C<undef> (in scalar context).

Since a valid, successful search can legitimately return zero matches,
you may wish to distinguish between a search that correctly returns 
zero hits and a search that has failed for some other reason (i.e. 
invalid search parameters).  Therefore, the C<$@> variable will always 
be cleared before the I<SEARCH> command is issued to the server, and 
will thus remain empty unless the server gives a I<BAD> or I<NO> response 
to the I<SEARCH> command. 

=cut

=head2 sort

Example:

	my @msgs = $imap->sort(@args) ;
	if ($@ ) {
		warn "Error in sort: $@\n";
	}

The B<sort> method is just like the L<search> method, only different.
It implements the SORT extension as described in
L<http://search.ietf.org/internet-drafts/draft-ietf-imapext-sort-10.txt>.
It would be wise to use the L<has_capability> method to verify that the
SORT capability is available on your server before trying to use the
B<sort> method. If you forget to check and you're connecting to a
server that doesn't have the SORT capability then B<sort> will return
undef. L<LastError> will then say you are "BAD". If your server doesn't
support the SORT capability then you'll have to use L<search> and then
sort the results yourself.

The first argument to B<sort> is a space-delimited list of sorting
criteria. The Internet Draft that describes SORT requires that this
list be wrapped in parentheses, even if there is only one sort
criterion. If you forget the parentheses then the B<sort> method will
add them. But you have to forget both of them, or none. This isn't CMS
running under VM!

The second argument is a character set to use for sorting. Different
character sets use different sorting orders, so this argument is
important. Since all servers must support UTF-8 and US-ASCII if they
support the SORT capability at all, you can use one of those if you
don't have some other preferred character set in mind.

The rest of the arguments are searching criteria, just as you would
supply to the L<search> method. These are all documented in RFC2060. If
you just want all of the messages in the currently selected folder
returned to you in sorted order, use I<ALL> as your only search
criterion.

The B<sort> method returns an array containing sequence numbers of
messages that passed the SORT IMAP client command's search criteria. If
the L<Uid> parameter is true then the array will contain message UID's.
If B<sort> is called in scalar context then a pointer to the array will
be passed, instead of the array itself. The message sequence numbers or
unique identifiers are ordered according to the sort criteria
specified. The sort criteria are nested in the order specified; that
is, items are sorted first by the first criterion, and within the first
criterion they are sorted by the second criterion, and so on.

The sort method C<$@> will clear C<$@> before attempting the I<SORT>
operation just as the L<search> method does.

=head2 see

Example:

	$imap->see(@msgs) or die "Could not see: $@\n";

The B<see> method accepts a list of one or more messages sequence
numbers, or a single reference to an array of one or more message
sequence numbers, as its argument(s). It then sets the I<\Seen> flag
for those message(s). Of course, if the L<Uid> parameter is set to a
true value then those message sequence numbers had better be unique
message id's, but then you already knew that, didn't you?

Note that specifying C<$imap-E<gt>see(@msgs)> is just a shortcut for
specifying C<$imap-E<gt>L<set_flag>("Seen",@msgs)>. 

=cut

=head2 seen

Example:

	my @seenMsgs = $imap->seen or warn "No seen msgs: $@\n";

The B<seen> method performs an IMAP SEARCH SEEN search against the
selected folder and returns an array of sequence numbers of messages
that have already been seen (ie their I<\Seen> flag is set). If the
L<Uid> parameter is true then an array of message UID's will be
returned instead. If called in scalar context than a reference to the
array (rather than the array itself) will be returned.

=cut

=head2 select

Example:

	$imap->select($folder) or die "Could not select: $@\n";

The B<select> method selects a folder and changes the object's state to
I<Selected>. It accepts one argument, which is the name of the folder
to select.

=cut

=head2 selectable

Example:

	foreach my $f ( grep($imap->selectable($_),$imap->folders ) ) {
		$imap->select($f) ;
	}

The B<selectable> method accepts one value, a folder name, and returns true
if the folder is selectable or false if it is not selectable.

=cut

=head2 sentbefore

Example:

	my @msgs = $imap->sentbefore($Rfc2060_date) 
		or warn "Could not find any msgs sent before $Rfc2060_date: $@\n";

The B<sentbefore> method works just like L<"sentsince">, below, except it
searches for messages that were sent before the date supplied as an
argument to the method.

=cut

=head2 senton

Example:

	my @msgs = $imap->senton($Rfc2060_date) 
		or warn "Could not find any messages sent on $Rfc2060_date: $@\n";

The B<senton> method works just like L<"sentsince">, below, except it searches
for messages that were sent on the exact date supplied as an argument
to the method.

=cut

=head2 sentsince

Example:

	my @msgs = $imap->sentsince($Rfc2060_date) 
		or warn "Could not find any messages sent since $Rfc2060_date: $@\n";

The B<sentsince> method accepts one argument, a date in either epoch
time format (seconds since 1/1/1970, or as output by L<time|perlfunc/time> 
and as accepted by L<localtime|perlfunc/localtime>) 
or in the I<date_text> format as defined in RFC2060 (dd-Mon-yyyy, where Mon 
is the English-language three-letter abbreviation for the month). 

It searches for items in the currently selected folder for messages
sent since the day whose date is provided as the argument. It uses the
RFC822 I<Date:> header to determine the I<sentsince> date. (Actually,
it the server that uses the I<Date:> header; this documentation just
assumes that the date is coming from the I<Date:> header because that's
what RFC2060 dictates.)

In the case of arguments supplied as a number of seconds, the returned
result list will include items sent on or after that day, regardless of
whether they arrived before the specified time on that day. The IMAP
protocol does not support searches at a granularity finer than a day,
so neither do I. On the other hand, the only thing I check for in a
I<date_text> argument is that it matches the pattern
C</\d\d-\D\D\D-\d\d\d\d/> (notice the lack of anchors), so if your
server lets you add something extra to a I<date_text> string then so
will B<Mail::IMAPClient>.

If you'd like, you can use the L<Rfc2060_date> method to convert from
epoch time (as returned by L<time|perlfunc/time>) into an RFC2060 date
specification. 

=cut

=head2 separator

Example:

	my $sepChar = $imap->separator(@args) 
		or die "Could not get separator: $@\n";

The B<separator> method returns the character used as a separator
character in folder hierarchies. On unix-based servers, this is often
but not necessarily a forward slash (/). It accepts one argument, the
name of a folder whose hierarchy's separator should be returned. If no
folder name is supplied then the separator for the INBOX is returned,
which probably is good enough.

If you want your programs to be portable from IMAP server brand X to
IMAP server brand Y, then you should never use hard-coded separator
characters to specify subfolders. (In fact, it's even more complicated
than that, since some server don't allow any subfolders at all, some
only allow subfolders under the "INBOX" folder, and some forbid
subfolders in the inbox but allow them "next" to the inbox.
Furthermore, some server implementations do not allow folders to
contain both subfolders and mail messages; other servers allow this.)

=cut

=head2 set_flag

Example:

	$imap->set_flag("Seen",@msgs) 
		or die "Could not set flag: $@\n";

The B<set_flag> method accepts the name of a flag as its first argument
and a list of one or more messages sequence numbers, or a single
reference to an array of one or more message sequence numbers, as its
next argument(s). It then sets the flag specified for those message(s).
Of course, if the L<Uid> parameter is set to a true value then those
message sequence numbers had better be unique message id's, just as
you'd expect.

Note that when specifying the flag in question, the preceding backslash
(\) is entirely optional. (For you, that is. B<Mail::IMAPClient> still
has remember to stick it in there before passing the command to the
server if the flag is one of the reserved flags specified in RFC2060.
This is in fact so important that the method checks its argument and
adds the backslash when necessary, which is why you don't have to worry
about it overly much.)

=cut

=head2 setacl

Example:

	$imap->setacl($folder,$userid,$authstring) 
		or die "Could not set acl: $@\n";

The B<setacl> method accepts three input arguments, a folder name, a
user id (or authentication identifier, to use the terminology of
RFC2086), and an access rights modification string. See RFC2086 for
more information. (This is somewhat experimental and its implementation
may change.)

=cut

=head2 since

Example:

	my @msgs = $imap->since($date) 
		or warn "Could not find any messages since $date: $@\n";

The B<since> method accepts a date in either epoch format
(seconds since 1/1/1970, or as output by L<perlfunc/time> and as
accepted by L<perlfunc/localtime>) or in the I<date_text> format as
defined in RFC2060 (dd-Mon-yyyy, where Mon is the English-language
three-letter abbreviation for the month). It searches for items in the
currently selected folder for messages whose internal dates are on or
after the day whose date is provided as the argument. It uses the
internal system date for a message to determine if that message was
sent since the given date.

In the case of arguments supplied as a number of seconds, the returned
result list will include items whose internal date is on or after that
day, regardless of whether they arrived before the specified time on
that day. 

If B<since> is called in a list context then it will return a list of 
messages meeting the I<SEARCH SINCE> criterion, or an empty list if
no messages meet the criterion.

If B<since> is called in a scalar context then it will return 
a reference to an array of messages meeting the I<SEARCH SINCE> 
criterion, or C<undef> if no messages meet the criterion.

Since B<since> is a front-end to L<search>, some of the same rules apply.
For example, the C<$@> variable will always be cleared before the I<SEARCH>
command is issued to the server, and will thus remain empty unless 
the server gives a I<BAD> or I<NO> response to the I<SEARCH> command.

=cut

=head2 size

Example:

	my $size = $imap->size($msgId) 
		or die "Could not find size of message $msgId: $@\n";

The B<size> method accepts one input argument, a sequence number (or
message UID if the L<Uid> parameter is true). It returns the size of
the message in the currently selected folder with the supplied sequence
number (or UID). The B<IMAPClient> object must be in a I<Selected>
state in order to use this method.

=cut

=head2 status

Example:

	my @rawdata = $imap->status($folder,qw/(Messages)/) 
		or die "Error obtaining status: $@\n";

The B<status> method accepts one argument, the name of a folder (or
mailbox, to use RFC2060's terminology), and returns an array containing
the results of running the IMAP STATUS client command against that
folder. If additional arguments are supplied then they are appended to
the IMAP STATUS client command string, separated from the rest of the
string and each other with spaces.

If B<status> is not called in an array context then it returns a
reference to an array rather than the array itself.

The B<status> method should not be confused with the B<Status> method
(with an uppercase 'S'), which returns information about the
B<IMAPClient> object. (See the section labeled L<"Status Methods">,
below).

=cut

=head2 store

Example:

	$imap->store(@args) or die "Could not store: $@\n";

The B<store> method accepts a message sequence number or
comma-separated list of message sequence numbers as a first argument, a
message data item name, and a value for the message data item.
Currently, data items are the word "FLAGS" followed by a space and a
list of flags (in parens). The word "FLAGS" can be modified by
prefixing it with either a "+" or a "-" (to indicate "add these flags"
or "remove these flags") and by suffixing it with ".SILENT" (which
reduces the amount of output from the server; very useful with large
message sets). Normally you won't need to call B<store> because there
are oodles of methods that will invoke store for you with the correct
arguments. Furthermore, these methods are friendlier and more flexible
with regards to how you specify your arguments. See for example L<see>,
L<deny_seeing>, L<delete_message>, and L<restore_message>. Or L<mark>,
L<unmark>, L<set_flag>, and L<unset_flag>.

=head2 subscribed

Example:

	my @subscribedFolders = $imap->subscribed 
		or warn "Could not find subscribed folders: $@\n";

The B<subscribed> method works like the B<folders> method, above,
except that the returned list (or array reference, if called in scalar
context) contains only the subscribed folders. 

Like L<folders>, you can optionally provide a prefix argument to the 
B<subscribed> method.

=head2 tag_and_run

Example:

	my @output = $imap->tag_and_run(@args) 
		or die "Could not tag_and_run: $@\n";

The B<tag_and_run> method accepts one or two arguments. The first
argument is a string containing an IMAP Client command, without a tag
but with all required arguments. The optional second argument is a
string to look for that will indicate success (without pattern
delimiters). The default is C<OK.*>. 

The B<tag_and_run> method will prefix your string (from the first
argument) with the next transaction number and run the command. It
returns an array of output lines from the command, which you are free
to parse as you see fit. Using this method instead of B<run> (above)
will free you from having to worry about handling the tags (and from
worrying about the side affects of naming your own tags).

=cut

=head2 uidnext

Example:

	my $nextUid = $imap->uidnext($folder) or die "Could not uidnext: $@\n";

The B<uidnext> method accepts one argument, the name of a folder, and
returns the numeric string that is the next available message UID for
that folder.

=head2 thread

Example: 

	my $thread = $imap->thread($algorythm, $charset, @search_args ) ;

The B<thread> method accepts zero to three arguments. The first argument is the
threading algorythm to use, generally either I<ORDEREDSUBJECT> or I<REFERENCES>.
The second argument is the character set to use, and the third argument is the
set of search arguments to use.

If the algorythm is not supplied, it defaults to I<REFERENCES> if available, or
I<ORDEREDSUBJECT> if available. If neither of these is available then the 
B<thread> method returns undef.

If the character set is not specified it will default to I<UTF-8>.

If the search arguments are not specified, the default is I<ALL>.

If B<thread> is called for an object connected to a server that does not support
the THREADS extension then the B<thread> method will return C<undef>.

The B<threads> method will issue the I<THREAD> command as defined in 
L<http://www.ietf.org/internet-drafts/draft-ietf-imapext-thread-11.txt>.
It returns an array of threads. Each element in the array is either a message
id or a reference to another array of (sub)threads.

If the L<Uid> parameter is set to a true value then the message id's returned 
in the thread structure will be message UID's. Otherwise they will be message
sequence numbers.

=head2 uidvalidity

Example:

	my $validity = $imap->uidvalidity($folder) 
		or die "Could not uidvalidity: $@\n";

The B<uidvalidity> method accepts one argument, the name of a folder,
and returns the numeric string that is the unique identifier validity
value for the folder.

=head2 unmark

Example:

	$imap->unmark(@msgs) or die "Could not unmark: $@\n";

The B<unmark> method accepts a list of one or more messages sequence
numbers, or a single reference to an array of one or more message
sequence numbers, as its argument(s). It then unsets the I<\Flagged>
flag for those message(s). Of course, if the L<Uid> parameter is set to
a true value then those message sequence numbers should really be
unique message id's.

Note that specifying C<$imap-E<gt>unmark(@msgs)> is just a shortcut for
specifying C<$imap-E<gt>unset_flag("Flagged",@msgs)>. 

Note also that the I<\Flagged> flag is just one of many possible flags.
This is a little confusing, but you'll have to get used to the idea
that among the reserved flags specified in RFC2060 is one name
I<\Flagged>. There is no specific meaning for this flag; it means
whatever the mailbox owner (or delegate) wants it to mean when it
is turned on.

=cut

=head2 unseen

Example:

	my @unread = $imap->unseen or warn "Could not find unseen msgs: $@\n";

The B<unseen> method performs an IMAP SEARCH UNSEEN search against the
selected folder and returns an array of sequence numbers of messages
that have not yet been seen (ie their I<\Seen> flag is not set). If the
L<Uid> parameter is true then an array of message UID's will be
returned instead. If called in scalar context than a pointer to the
array (rather than the array itself) will be returned.

Note that when specifying the flag in question, the preceding backslash
(\) is entirely optional.

=cut

=head2 unseen_count

Example:

	foreach my $f ($imap->folders) {
		print 	"The $f folder has ",
			$imap->unseen_count($f)||0, 
			" unseen messages.\n";		
	}

The B<unseen_count> method accepts the name of a folder as an argument
and returns the number of unseen messages in that folder. If no folder
argument is provided then it returns the number of unseen messages in
the currently selected Folder.

=head2 unset_flag

Example:

	$imap->unset_flag("\Seen",@msgs) 
		or die "Could not unset_flag: $@\n";

The B<unset_flag> method accepts the name of a flag as its first
argument and a list of one or more messages sequence numbers, or a
single reference to an array of one or more message sequence numbers,
as its next argument(s). It then unsets the flag specified for those
message(s). Of course, if the L<Uid> parameter is set to a true value
then those message sequence numbers had better be unique message id's,
just as you'd expect.

=cut

=head1 Other IMAP Client Commands and the Default Object Method

IMAP Client Commands not otherwise documented have been implemented via
an AUTOLOAD hack and use a default method.

If a program calls a method that is not defined (or inherited) by the
B<IMAPClient> module then the B<IMAPClient> module will assume that it
is an IMAP client command. It will prefix the command with the next
available transaction number (or tag value), and append to it the
space-delimited list of arguments supplied to the unimplemented method
(if any). It will then read lines of output from the imap session until
it finds a line containing the strings "OK" and "Completed", and return
an array containing all of the lines of output (or, if called in scalar
context, an array reference). If it finds "BAD" or "NO" instead of "OK"
it returns C<undef>.

Eg: 

	my @results = $imap->FOO("bar","an example","of the default");


results in:


	99 FOO bar an example of the default\r\n 

being sent to the IMAP server (assuming that 99 is the current
transaction number).

Notice that we used an uppercase method name "FOO" so as not to
conflict with future implementations of that IMAP command. If you run
your script with warnings turned on (always a good idea, at least
during testing), then you will receive warnings whenever you use a
lowercase method name that has not been implemented. An exception to
this is when you use certain common (yet unimplemented) methods that,
if ever explicitly implemented, are guaranteed to behave just like the
default method. To date, those methods are either documented in the
section labeled L<"OBJECT METHODS">, above, or listed here:

B<Mail::IMAPClient>'s default method adds enormous flexibility and
built-in extensibility but it is not psychic. It can handle almost
any extension and truthfully tell you if the server successfully 
performed your request. But it cannot predict how the command's
output should be handled, beyond returning a true value on success
and C<undef> on failure. So if you are running a command because
you want the output then you may need to parse that output yourself.
If you develop code that extends B<Mail::IMAPClient> in a way that
you feel may be useful to others then please consider donating the
code. Many of the methods in B<Mail::IMAPClient> were contributed
by other programmers such as yourself. Their contributions are listed
in the F<Changes> file as they occur.

=head2 copy($msg,$folder)

Copy a message from the currently selected folder in the the folder
whose name is in C<$folder>

=head2 subscribe($folder)

Subscribe to a folder

B<CAUTION:> Once again, remember to quote your quotes if you want
quotes to be part of the IMAP command string. 

You can also use the default method to override the behavior of
implemented IMAP methods by changing the case of the method name,
preferably to all-uppercase so as not to conflict with the Class method
and accessor method namespace. For example, if you don't want the
L<search> method's behavior (which returns a list of message numbers)
but would rather have an array of raw data returned from your L<search>
operation, you can issue the following snippet:

	my @raw = $imap->SEARCH("SUBJECT","Whatever...");

which is slightly more efficient than the equivalent:

	$imap->search("SUBJECT","Whatever...");

	my @raw = $imap->Results;

Of course you probably want the search results tucked nicely into a list
for you anyway, in which case you might as well use the L<search> method.

=cut

=head1 Parameters

There are several parameters that influence the behavior of an
B<IMAPClient> object. Each is set by specifying a named value pair
during new method invocation as follows:

	my $imap = Mail::IMAPClient->new ( parameter  => "value",
			       parameter2 => "value",
				...
	);

Parameters can also be set after an object has been instantiated by
using the parameter's eponymous accessor method like this:

	my $imap = Mail::IMAPClient->new;
	   $imap->parameter( "value");
	   $imap->parameter2("value");

The eponymous accessor methods can also be used without arguments to
obtain the current value of the parameter as follows:

	my $imap = Mail::IMAPClient->new;
           $imap->parameter( "value");
           $imap->parameter2("value");

		... 	# A whole bunch of awesome perl code, 
			# omitted for brevity


	   my $forgot  = $imap->parameter;
	   my $forgot2 = $imap->parameter2;

Note that in these examples I'm using 'parameter' and 'parameter2' as
generic parameter names. The B<IMAPClient> object doesn't actually have
parameters named 'parameter' and 'parameter2'. On the contrary, the
available parameters are:

=head2 Buffer

Example:

	$Buffer = $imap->Buffer();
	# or:
	$imap->Buffer($new_value);

The I<Buffer> parameter sets the size of a block of I/O. It is ignored
unless L<Fast_io>, below, is set to a true value (the default), or
unless you are using the L<migrate> method. It's value should be the
number of bytes to attempt to read in one I/O operation. The default
value is 4096.

When using the L<migrate> method, you can often achieve dramatic
improvements in throughput by adjusting this number upward. However,
doing so also entails a memory cost, so if set too high you risk losing
all the benefits of the L<migrate> method's chunking algorythm. Your
program can thus terminate with an "out of memory" error and you'll
have no one but yourself to blame.

Note that, as hinted above, the I<Buffer> parameter affects the
behavior of the L<migrate> method regardless of whether you have
L<Fast_io> turned on. Believe me, you don't want to go around migrating
tons of mail without using buffered I/O! 


=head2 Clear

Example:

	$Clear = $imap->Clear();
	# or:
	$imap->Clear($new_value);

The name of this parameter, for historical reasons, is somewhat
misleading. It should be named I<Wrap>, because it specifies how many
transactions are stored in the wrapped history buffer. But it didn't
always work that way; the buffer used to actually get cleared. The name
though remains the same in the interests of backwards compatibility.
Also I'm too lazy to change it.

I<Clear> specifies that the object's history buffer should be wrapped
after every I<n> transactions, where I<n> is the value specified for
the I<Clear> parameter. Calling the eponymous B<Clear> method without
an argument will return the current value of the I<Clear> parameter but
will not cause clear the history buffer to wrap. 

Setting I<Clear> to 0 turns off automatic history buffer wrapping, and
setting it to 1 turns off the history buffer facility (except for the
last transaction, which cannot be disabled without breaking the
B<IMAPClient> module). Setting I<Clear> to 0 will not cause an
immediate clearing of the history buffer; setting it to 1 (or any other
number) will (except of course for that inevitable last transaction). 

The default I<Clear> value is set to five in order to conserve memory.

=head2 Debug

Example:

	$Debug = $imap->Debug();
	# or:
	$imap->Debug($true_or_false);

Sets the debugging flag to either a true or false value. Can be
supplied with the L<new> method call or separately by calling the
B<Debug> object method. Use of this parameter is strongly recommended
when debugging scripts and required when reporting bugs.

=head2 Debug_fh

Example:

	$Debug_fh = $imap->Debug_fh();
	# or:
	$imap->Debug_fh($fileHandle);

Specifies the filehandle to which debugging information should be
printed. It can either a filehandle object reference or a filehandle
glob. The default is to print debugging info to STDERR.

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

which is the same as:

	use Mail::IMAPClient;
	use IO::File;
	# set $user, $pass, and $server here
	my $imap = Mail::IMAPClient->new(	User	=>$user, 
						Password=>$pass, 
						Server	=>$server, 
						Debug	=> "yes, please",
						Debug_fh=> IO::File->new(">debugging.output") || 
							die "Can't open debugging.output: $!\n"
	);


You can also:

	use Mail::IMAPClient;
	# set $user, $pass, and $server here
	open(DBG,">debugging.output") 
		or die "Can't open debugging.output: $!\n";
	my $imap = Mail::IMAPClient->new(	User=>$user, Password=>$pass, 
						Server=>$server, Debug=> 1,
						Debug_fh => *DBG
	);

Specifying this parameter is not very useful unless L<Debug> is set 
to a true value.

=head2 EnableServerResponseInLiteral

Example:

	$EnableServerResponseInLiteral = $imap->EnableServerResponseInLiteral();
	# or:
	$imap->EnableServerResponseInLiteral($new_value);

The I<EnableServerResponseInLiteral> parameter tells
B<Mail::IMAPClient> to expect server responses to be embedded in
literal strings. Usually literal strings contain only message data, not
server responses. I have seen at least one IMAP server implementation
though that includes the final <tag> OK response in the literal data.
If your server does this then your script will hang whenever you try to
read literal data, such as message text, or even output from the
L<folders> method if some of your folders have special characters such
as double quotes or sometimes spaces in the name.

I am pretty sure this behavior is not RFC2060 compliant so I am
dropping it by default. In fact, I encountered the problem a long time
ago when still new to IMAP and may have imagined the whole thing.
However, if your scripts hang running certain methods you may want to
at least try enabling this parameter by passing the eponymous method a
true value. 

=head2 Fast_io

Example:

	$Fast_io = $imap->Fast_io();
	# or:
	$imap->Fast_io($true_or_false);

The I<Fast_io> parameter controlls whether or not your
B<Mail::IMAPClient> object will attempt to use buffered (i.e. "Fast")
I/O. It is turned on by default. If you turn it off you will definately
slow down your program, often to a painfull degree. However, if you are
experience problems you may want to try this just to see if it helps.
If it does then that means you have found a bug and should report it
immediately (by following the instructions in the section on
L<"REPORTING BUGS">). Even if it doesn't fix the problem, testing with
both I<Fast_io> turn on and with it turned off will often aid in
identifying the source of the problem. (If it doesn't help you, it may
help me when you report it!)

Lately there have not been any bugs associated with I<Fast_io> so this
parameter may become deprecated in the future.

=head2 Folder

Example:

	$Folder = $imap->Folder();
	# or:
	$imap->Folder($new_value);

The I<Folder> parameter returns the name of the currently-selected
folder (in case you forgot). It can also be used to set the name of the
currently selected folder, which is completely unnecessary if you used
the L<select> method (or L<select>'s read-only equivalent, the
L<examine> method) to select it. 

Note that setting the I<Folder> parameter does not automatically select 
a new folder; you use the L<select> or L<examine> object methods for that. 
Generally, the I<Folder> parameter should only be queried (by using the 
no-argument form of the B<Folder> method). You will only need to set the 
I<Folder> parameter if you use some mysterious technique of your own for
selecting a folder, which you probably won't do.

=cut

=head2 Maxtemperrors

Example:

	$Maxtemperrors = $imap->Maxtemperrors();
	# or:
	$imap->Maxtemperrors($new_value);

The I<Maxtemperrors> parameter specifies the number of times a write
operation is allowed to fail on a "Resource Temporarily Available"
error. These errors can occur from time to time if the server is too
busy to empty out its read buffer (which is logically the "other end"
of the client's write buffer). By default, B<Mail::IMAPClient> will
retry 10 times, but you can adjust this behavior by setting
I<Maxtemperrors>. Note that after each temporary error, the server will
wait for a number of seconds equal to the number of consecutive
temporary errors times .25, so very high values for I<Maxtemperrors>
can slow you down in a big way if your "temporary error" is not all
that temporary.

You can set this parameter to "UNLIMITED" to ignore "Resource
Temporarily Unavailable" errors.

=head2 Password

Example:

	$Password = $imap->Password();
	# or:
	$imap->Password($new_value);

Specifies the password to use when logging into the IMAP service on the
host specified in the I<Server> parameter as the user specified in the
I<User> parameter. Can be supplied with the B<new> method call or
separately by calling the B<Password> object method.

If I<Server>, I<User>, and I<Password> are all provided to the L<new>
method, then the newly instantiated object will be connected to the
host specified in I<Server> (at either the port specified in I<Port> or
the default port 143) and then logged on as the user specified in the
I<User> parameter (using the password provided in the I<Password>
parameter). See the discussion of the L<"new"> method, below.

=head2 Peek

Example:

	$Peek = $imap->Peek();
	# or:
	$imap->Peek($true_or_false);

Setting I<Peek> to a true value will prevent the L<body_string>,
L<message_string> and L<message_to_file> methods from automatically
setting the I<\Seen> flag. Setting L<"Peek"> to 0 (zero) will force
L<"body_string">, L<"message_string">, L<"message_to_file">, and
L<"parse_headers"> to always set the I<\Seen> flag. 

The default is to set the seen flag whenever you fetch the body of a
message but not when you just fetch the headers. Passing I<undef> to
the eponymous B<Peek> method will reset the I<Peek> parameter to its
pristine, default state. 

=cut

=head2 Port

Example:

	$Port = $imap->Port();
	# or:
	$imap->Port($new_value);

Specifies the port on which the IMAP server is listening. The default
is 143, which is the standard IMAP port. Can be supplied with the
L<new> method call or separately by calling the L<Port> object method.

=head2 Server

Example:

	$Server = $imap->Server();
	# or:
	$imap->Server($hostname);

Specifies the hostname or IP address of the host running the IMAP
server. If provided as part of the L<new> method call, then the new
IMAP object will automatically be connected at the time of
instantiation. (See the L<new> method, below.) Can be supplied with the
L<new> method call or separately by calling the B<Server> object
method.

=cut

=head2 Socket

Example:

	$Socket = $imap->Socket();
	# or:
	$imap->Socket($socket_fh);

The I<Socket> method can be used to obtain the socket handle of the
current connection (say, to do I/O on the connection that is not
otherwise supported by B<Mail::IMAPClient>) or to replace the current
socket with a new handle (perhaps an SSL handle, for example). 

If you supply a socket handle yourself, either by doing something like:

	 $imap=Mail::IMAPClient->new(Socket=>$sock, User => ... );

or by doing something like:

	 $imap=Mail::IMAPClient->new(User => $user, Password => $pass, Server => $host);
	 # blah blah blah
	 $imap->Socket($ssl);

then it will be up to you to establish the connection AND to
authenticate, either via the L<login> method, or the fancier
L<authenticate>, or, since you know so much anyway, by just doing raw
I/O against the socket until you're logged in. If you do any of this
then you should also set the L<State> parameter yourself to reflect the
current state of the object (i.e. Connected, Authenticated, etc). 

=cut

=head2 Timeout

Example:

	$Timeout = $imap->Timeout();
	# or:
	$imap->Timeout($new_value);

Specifies the timeout value in seconds for reads. Specifying a true
value for I<Timeout> will prevent B<Mail::IMAPClient> from blocking in
a read.

Since timeouts are implemented via the perl L<select|perlfunc/select>
operator, the I<Timeout> parameter may be set to a fractional number of
seconds. Not supplying a I<Timeout>, or (re)setting it to zero,
disables the timeout feature.

=cut

=head2 Uid

Example:

	$Uid = $imap->Uid();
	# or:
	$imap->Uid($true_or_false);

If L<Uid> is set to a true value (i.e. 1) then the behavior of the
L<fetch>, L<search>, L<copy>, and L<store> methods (and their
derivatives) is changed so that arguments that would otherwise be
message sequence numbers are treated as message UID's and so that
return values (in the case of the L<search> method and its derivatives)
that would normally be message sequence numbers are instead message
UID's.

Internally this is implemented as a switch that, if turned on, causes
methods that would otherwise issue an IMAP FETCH, STORE, SEARCH, or
COPY client command to instead issue UID FETCH, UID STORE, UID SEARCH,
or UID COPY, respectively. The main difference between message sequence
numbers and message UID's is that, according to RFC2060, UID's must not
change during a session and should not change between sessions, and
must never be reused. Sequence numbers do not have that same guarantee
and in fact may be reused right away. 

Since foldernames also have a unique identifier (UIDVALIDITY), which is
provided when the folder is L<select>ed or L<examine>d or by doing
something like "$imap->status($folder,"UIDVALIDITY"), it is possible to
uniquely identify every message on the server, although normally you
won't need to bother.

The methods currently affected by turning on the L<Uid> flag are:

	copy 		fetch
	search 		store 
	message_string 	message_uid
	body_string 	flags
	move 		size
	parse_headers	thread

Note that if for some reason you only want the L<Uid> parameter turned
on for one command, then you can choose between the following two
snippets, which are equivalent:

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

In the second example, we used the default method to issue the UID IMAP
Client command, being careful to use an all-uppercase method name so as
not to inadvertently call the L<Uid> accessor method. Then we parsed
out the message UIDs manually, since we don't have the benefit of the
built-in L<search> method doing it for us.

Please be very careful when turning the L<Uid> parameter on and off
throughout a script. If you loose track of whether you've got the
L<Uid> parameter turned on you might do something sad, like deleting
the wrong message. Remember, like all eponymous accessor methods, the
B<Uid> method without arguments will return the current value for the
L<Uid> parameter, so do yourself a favor and check. The safest approach
is probably to turn it on at the beginning (or just let it default to
being on) and then leave it on. (Remember that leaving it turned off
can lead to problems if changes to a folder's contents cause
resequencing.) 

By default, the L<Uid> parameter is turned on.

=head2 User

Example:

	$User = $imap->User();
	# or:
	$imap->User($userid);

Specifies the userid to use when logging into the IMAP service. Can be
supplied with the L<new> method call or separately by calling the
B<User> object method.

Parameters can be set during L<new> method invocation by passing named
parameter/value pairs to the method, or later by calling the
parameter's eponymous object method.

=cut


=head1 Status Methods


There are several object methods that return the status of the object.
They can be used at any time to check the status of an B<IMAPClient>
object, but are particularly useful for determining the cause of
failure when a connection and login are attempted as part of a single
L<new> method invocation. The status methods are:

=head2 Escaped_results

Example:
	my @results = $imap->Escaped_results ;

The B<Escaped_results> method is almost identical to the B<History>
method. Unlike the B<History> method, however, server output
transmitted literally will be wrapped in double quotes, with all of the
parentheses, double quotes, backslashes, newlines, and carrage returns
escaped. If called in a scalar context, B<Escaped_results> returns an
array reference rather than an array.

B<Escaped_results> is useful if you are retrieving output and
processing it manually, and you are depending on the above special
characters to delimit the data. It is not useful when retrieving
message contents; use B<message_string> or B<body_string> for that.

=head2 History

Example:

	my @history = $imap->History;

The B<History> method is almost identical to the L<Results> method.
Unlike the L<Results> method, however, the IMAP command that was issued
to create the results being returned is not included in the returned
results. If called in a scalar context, B<History> returns an array
reference rather than an array.

=head2 IsUnconnected

returns a true value if the object is currently in an L<Unconnected>
state.

=head2 IsConnected

returns a true value if the object is currently in either a
L<Connected>, L<Authenticated>, or L<Selected> state.

=head2 IsAuthenticated

returns a true value if the object is currently in either an
L<Authenticated> or L<Selected> state.

=head2 IsSelected

returns a true value if the object is currently in a L<Selected> state.

=head2 LastError

Internally B<LastError> is implemented just like a parameter (as
described in L<"Parameters">, above). There is a I<LastError> attribute
and an eponymous accessor method which returns the I<LastError> text
string describing the last error condition encountered by the server. 

Note that some errors are more serious than others, so I<LastError>'s
value is only meaningful if you encounter an error condition that you
don't like. For example, if you use the L<exists> method to see if a
folder exists and the folder does not exist, then an error message will
be recorded in I<LastError> even though this is not a particularly
serious error. On the other hand, if you didn't use L<exists> and just
tried to L<select> a non-existing folder, then L<select> would return
C<undef> after setting I<LastError> to something like C<NO SELECT
failed: Can't open mailbox "mailbox": no such mailbox>. At this point
it would be useful to print out the contents of I<LastError> as you
L<die|perlfunc/die>.

=head2 LastIMAPCommand

New in version 2.0.4, B<LastIMAPCommand> returns the exact IMAP command
string to be sent to the server. Useful mainly in constructing error
messages when L<LastError> just isn't enough.

=head2 Report

The B<Report> method returns an array containing a history of the IMAP
session up to the point that B<Report> was called. It is primarily
meant to assist in debugging but can also be used to retrieve raw
output for manual parsing. The value of the L<Clear> parameter controls
how many transactions are in the report. (See the discussion of
L<Clear> in L<"Parameters">, above.)

=cut

=head2 Results

The B<Results> method returns an array containing the results of one
IMAP client command. It accepts one argument, the transaction number of
the command whose results are to be returned. If transaction number is
unspecified then B<Results> returns the results of the last IMAP client
command issued. If called in a scalar context, B<Results> returns an
array reference rather than an array.

=cut

=head2 State 

The B<State> method returns a numerical value that indicates the
current status of the B<IMAPClient> object. If invoked with an
argument, it will set the object's state to that value. If invoked
without an argument, it behaves just like L<Status>, below. 

Normally you will not have to invoke this function. An exception is if
you are bypassing the B<Mail::IMAPClient> module's L<connect> and/or
L<login> modules to set up your own connection (say, for example, over
a secure socket), in which case you must manually do what the
L<connect> and L<login> methods would otherwise do for you.

=head2 Status 

The B<Status> method returns a numerical value that indicates the
current status of the B<IMAPClient> object. (Not to be confused with
the L<status> method, all lower-case, which is the implementation of
the I<STATUS> IMAP client command.)

=head2 Transaction 

The B<Transaction> method returns the tag value (or transaction number)
of the last IMAP client command.

=head1 Undocumented Methods and Subroutines

There are two types of undocumented subroutines and methods. The first
are methods that are not documented because they don't exist, even
though they work just fine. Some of my favorite B<Mail::IMAPClient>
methods don't exist but I use them all the time anyway. You can too,
assuming you have your copy of RFC2060 and its extension RFC's handy.
(By the way, you do have them handy because I gave them to you. They're
bundled with the B<Mail::IMAPClient> distribution in the F<docs/>
subdirectory.) You should feel free to use any of these undocumented
methods.

These undocumented methods all use what this document refers to as the
"default method". See L<Other IMAP Client Commands and the Default
Object Method>, above, for more information on the default method.

There are also some undocumented methods and subroutines that actually
do exist. Don't use these! If they aren't documented it's for a reason.
They are either experimental, or intended for use by other
B<Mail::IMAPClient> methods only, or deprecated, or broken, or all or
none of the above. In no cases can you write programs that use these
methods and assume that these programs will work with the next version
of B<Mail::IMAPClient>. I never try to make these undocumented methods
and subroutines backwards compatible because they aren't part of the
documented API. 

Occasionally I will add a method and forget to document it; in that
case it's a bug and you should report it. (See L<"REPORTING BUGS">,
below.) It is sometimes hard to tell the difference; if in doubt you
may submit a bug report and see what happens! However, don't bothering
submitting bug reports for missing documentation for any method or
subroutine that begins with an underscore (_) character. These methods
are always private and will never be part of the documented interface. 

=head1 REPORTING BUGS

Please feel free to e-mail the author at the below address if you
encounter any strange behaviors. Don't worry about hurting my 
feelings or sounding like a whiner or anything like that; 
if there's a problem with this module I'd like to hear about it. 
However, I probably won't be able to do much about it if you don't 
include enough information, so please read and follow these
instructions carefully:

When reporting a bug, please be sure to include the following:

- As much information about your environment as possible. I especially
need to know which version of Mail::IMAPClient you are running and the
type/version of IMAP server to which you are connecting. Your OS and
perl verions would be helpful too.

- As detailed a description of the problem as possible. (What are you
doing? What happens? Have you found a work-around?)

- An example script that demonstrates the problem (preferably with as
few lines of code as possible!) and which calls the Mail::IMAPClient's
B<new> method with the I<Debug> parameter set to "1".

- Output from the example script when it's running with the Debug
parameter turned on. You can edit the output to remove (or preferably
to "X" out) sensitive data, such as hostnames, user names, and
passwords, but PLEASE do not remove the text that identifies the TYPE
of IMAP server to which you are connecting. Note that in most versions
of B<Mail::IMAPClient>, debugging does not print out the user or
password from the login command line. However, if you use some other
means of authenticating then you may need to edit the debugging output
with an eye to security.

- If something worked in a previous release and doesn't work now,
please tell me which release did work. You don't have to test every
intervening release; just let me know it worked in version x but
doesn't work in version (x+n) or whatever.

- Don't be surprised if I come back asking for a trace of the problem.
To provide this, you should create a file called I<.perldb> in your
current working directory and include the following line of text in
that file:

C<&parse_options("NonStop=1 LineInfo=mail_imapclient_db.out");>

For your debugging convenience, a sample .perldb file, which was
randomly assigned the name F<sample.perldb>, is provided in the
distribution.

Next, without changing your working directory, debug the example script
like this: C<perl -d example_script.pl [ args ]>

Note that in these examples, the script that demonstrates your problem
is named "example_script.pl" and the trace output will be saved in
"mail_imapclient_db.out". You should change these to suite your needs.

=head1 AUTHOR

	David J. Kernen
	The Kernen Consulting Group, Inc
	david.kernen@kernengroup.com

=cut

=head1 COPYRIGHT

                       Copyright 1999, 2000 The Kernen Group, Inc.
                            All rights reserved.

This program is free software; you can redistribute it and/or modify it
under the terms of either:

=over 4

=item a) the "Artistic License" which comes with this Kit, or

=item b) the GNU General Public License as published by the Free Software Foundation; either 
version 1, or (at your option) any later version.

=back

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See either the GNU
General Public License or the Artistic License for more details.

=cut

my $not_void = 1;
