package Mail::IMAPClient;

# $Id: IMAPClient.pm,v 19991216.8 2000/01/12 18:58:00 dkernen Exp $

$Mail::IMAPClient::VERSION = '1.08a';
$Mail::IMAPClient::VERSION = '1.08a';  	# do it twice to make sure it takes

use Fcntl qw(:DEFAULT);
use Socket;
use IO::Socket;

#print "Found Fcntl in $INC{'Fcntl.pm'}\n";
#Fcntl->import;

=head1 NAME 

Mail::IMAPClient - An IMAP Client API

=cut


sub Unconnected 	{ return 0 ; }		# Object not connected
sub Connected 		{ return 1 ; } 		# connected; not logged in
sub Authenticated 	{ return 2 ; }		# logged in; no mailbox selected
sub Selected 		{ return 3 ; }		# mailbox selected

# the following for loop sets up eponymous accessor methods for 
# the object's parameters:

{
 for my $datum (
		qw( 	State Port Server Folder Fast_io
			User Password Socket Timeout
			Debug LastError Count Uid
		)
 ) {
	no strict 'refs';
        *$datum = sub {
                if (defined($_[1])) {
                        return $_[0]->{$datum} = $_[1] ;
                } else {
                        return $_[0]->{$datum};
                }
        };
 }
}

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

# read-only access to the transaction number:
sub Transaction { shift->Count };

# the constructor:
sub new {
	my $class 	= shift;
	my $self  	= {};

	bless $self, ref($class)||$class;

	$self->{Count} = 1;
	$self->State(Unconnected);
	if (@_) {
		my %conf = @_;
		while (my($k,$v)=each %conf) { 
			$k = ucfirst lc($k) ; 
			$self->$k($v);
		}
	}	
	$self->Clear(5) unless exists $self->{'Clear'};
	return $self->connect if $self->Server;
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
	my ($code, $output);
        $output = "";

        until ( ($code) = $output =~ /^\*\s+(OK|BAD|NO)/i ) {

                $output = $self->_read_line;
                for my $o (split(/\r?\n/,$output)) {
                        $self->_record(0,"$o\r\n");
                }
                if ($output =~ /^\*\s+(BYE|NO)/) {
                        $self->State(Unconnected);
                        return undef ;
                }
        }

	$self->State(Connected);
	if ($self->User and $self->Password) {
		return $self->login ;
	} else {
		return $self;	
	}
}
	

sub login {
	my $self = shift;
	my $string = "Login " . $self->User . " " . $self->Massage($self->Password) ;
	$self->_imap_command($string) 
		and $self->State(Authenticated);
	# $self->folders and $self->separator unless $self->NoAutoList;
	return ( ($self->State eq Authenticated) ? $self : undef);
}

sub separator {
	my $self = shift;
	my $target = shift ; defined($target) or $target = "INBOX";
	$target = 'INBOX' if $target =~ /inbox/i;

	# The fact that the response might end with {123} doesn't really matter here:

	unless (exists $self->{"$target${;}SEPARATOR"}) {
		my $list = (grep(/^\*\s+LIST\s+/, $self->list(undef,$target) ))[0] || qq("/");
		my $s = (split(/\s+/,$list))[3];
		$self->{"$target${;}SEPARATOR"} = $s eq 'NIL' ? 'NIL' : substr($s, 1,length($s)-2) ;
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
				$self->{"History"}{$self->Count}	;
}


sub deleteacl {
	my $self = shift;
	my ($target, $user ) = @_;
	$target 	  = $self->Massage($target);
	$user		  =~ s/^"(.*)"$/$1/;
	$user 	  	  =~ s/"/\\"/g;
	my $string 	=  qq(DELETEACL $target "$user");
	$self->_imap_command($string)  or return undef;

	return wantarray ? 	$self->History($self->Count) 		: 
				$self->{"History"}{$self->Count}	;
}

sub setacl {
	my $self = shift;
	my ($target, $user, $acl) = @_;
	$user = $self->User unless defined($user);
	$target = $self->Folder unless defined($target);
	$target 	  = $self->Massage($target);
	$user		  =~ s/^"(.*)"$/$1/;
	$user 	  	  =~ s/"/\\"/g;
	$acl		  =~ s/^"(.*)"$/$1/;
	$acl 	  	  =~ s/"/\\"/g;
	my $string 	=  qq(SETACL $target "$user" "$acl");
	$self->_imap_command($string)  or return undef;
	return wantarray ? 	$self->History($self->Count) 		: 
				$self->{"History"}{$self->Count}	;
}

sub getacl {
	my $self = shift;
	my ($target) = @_;
	$target = $self->Folder unless defined($target);
	$target 	  = $self->Massage($target);
	my $string 	=  qq(GETACL $target);
	$self->_imap_command($string)  or return undef;
	return wantarray ? 	$self->History($self->Count) 		: 
				$self->{"History"}{$self->Count}	;
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

	$target = $self->Massage($target);

	my $string 	=  qq/SELECT $target/;

	my $old = $self->Folder;

	if ($self->_imap_command($string) and $self->State(Selected)) {
		$self->Folder($target);
		return $old||$self;
	} else { 
		return undef;
	}
}

sub flags {
	my $self = shift;
	my $msg  = shift;
	$self->fetch($msg,"FLAGS");
	my $line = ($self->Results)[-2];
        my($flags) = $line =~ /FLAGS\s*\(([^)]*)\)/;
        my @flags = split(/\s+/,$flags);
	return wantarray ? @flags : \@flags ;
}

sub message_string {
	my $self = shift;
	my $msg  = shift;
	my @head = $self->fetch($msg,"RFC822.HEADER");
	my @torso = $self->fetch($msg,"RFC822.TEXT");
	shift @head and pop @head and pop @head;
	shift @torso and pop @torso and pop @torso;
	return join("",@head) . join("",@torso);
}

sub message_uid {
	my $self = shift;
	my $msg  = shift;
	my @uid = $self->fetch($msg,"UID");
	my $uid;
	while ( my $u = shift @uid and !$uid) {
		($uid) = $u =~ /\(UID\s+(\d+)\)\r?$/;
	}
	return $uid;
}

sub body_string {
	my $self = shift;
	my $msg  = shift;
	my @torso = $self->fetch($msg,"RFC822.TEXT");
	shift @torso and pop @torso and pop @torso;
	return join("",@torso);
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

# _{name} methods are undocumented and meant to be private.

# _imap_command runs a command, inserting the correct tag
# and <CR><LF> and whatnot.
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

	$self->_record($count,"$string\r\n");

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError( "Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	$output = "";

	until ( ($code) = $output =~ /^$count (OK|BAD|NO|$good)/m ) {
		# print "Line +$output \t not '$count OK or BAD or NO or $good'\n" 
		#	if $self->Debug;
		$output = $self->_read_line;	
		for my $o (split(/\r?\n/,$output)) { 
			$self->_record($count,"$o\r\n");
		}
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

	$self->LastError("$line") if $line =~ /^\S+\s+(BAD|NO)/;

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
	return	syswrite(	
				$self->Socket, 
				$string, 
				length($string) 
	) ;
}

#_read_line reads from the socket:
sub _read_line {
	
	my $self 	= shift;	
	my $sh		= $self->Socket;
	
	my $buffer	= ""; 
	my $count	= ""; $count = 0;
	my $rvec 	= my $ready = my $errors = 0; 
	my $timeout	= $self->Timeout;

	my $readlen 	= 1;
	my $fcntl 	= '';
	my $flags 	= '0';

	if ( $self->Fast_io and $fcntl=fcntl($sh, F_GETFL, $flags) ) {
		# print STDERR 
		# "Setfl = ",F_SETFL," and GETFL = ",F_GETFL," and NONBLOCK = ",O_NONBLOCK,"\n";
		# print STDERR "Fcntl flag is now $fcntl\n";
		my $newflags = $fcntl;
		$newflags |= O_NONBLOCK;
		fcntl($sh, F_SETFL, $newflags) and $readlen = 4096;
	}

	until ($buffer =~ /\r?\n$/ ) {
		if ($timeout) {
			vec($rvec, fileno($self->Socket), 1) = 1;
			CORE::select( $ready = $rvec, undef, $errors = $rvec, $timeout) ;
			unless ( vec ( $ready, fileno($self->Socket), 1 ) ) {
				$self->LastError("Tag " . $self->Transaction . 
					": Timeout waiting for data from server\n");	
				fcntl($sh, F_SETFL, $fcntl) 
					if $self->Fast_io and defined($fcntl);
				return undef;
			}
		}
		my $newcount = $count;
		my $oldW = $^W;
		$^W = 0;
		$count += sysread(
					$sh,
					$buffer,
					$readlen,
					$newcount
		) ;
		$^W = $oldW;
		# print "Read so far: $buffer\n" if $self->Debug;

	}
	fcntl($sh, F_SETFL, $fcntl) if $self->Fast_io and defined($fcntl);
	if ( $buffer =~ /\{(\d+)\}\r\n$/ ) {
		# my $len = $1 + 2;
		my $newcount = 0;
		if ($timeout) {
			vec($rvec, fileno($self->Socket), 1) = 1;
			unless ( CORE::select( $ready = $rvec, undef, $errors = $rvec, $timeout) ) {
				$self->LastError("Tag " . $self->Transaction . 
					": Timeout waiting for literal data from server\n");	
				return undef;
			}
		}
		$newcount += sysread($sh,$buffer,$len-$newcount, 
						$count+$newcount)
			until $newcount >= $len;
		sysread($sh,$buffer,2,$count+$newcount) unless $buffer =~ /\r\n$/;
	}
	print "Read: $buffer\n" if $self->Debug;
	return defined($buffer) ? $buffer : undef ;
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
	$self->{Folders} = undef;
	$self->Socket->close ; $self->Socket(undef);
	return $self;
}

sub folders {
        my $self = shift;
	my $what = shift ;
        return wantarray ?      @{$self->{Folders}} :
                                $self->{Folders} 
                if ref($self->{Folders}) and !$what;
	
        my @folders ;  
	my $list = $self->list(undef,( $what? "$what" . $self->separator($what) . "*" : undef ) );
	
	for (my $m = 0; $m < scalar(@$list); $m++ ) {
	
		if ($list->[$m]  =~ s/(\{\d+\})\r\n$// ) {
			$list->[$m] .= '\FOLDER LITERAL::' . $list->[$m+1];
			$list->[$m+1] = "";	
		}
			
		
		push @folders, $1||$2 
			if $list->[$m] =~
                        /       ^\*\s+LIST              # * LIST
                                \s+\([^\)]*\)\s+            # (Flags)
                                "[^"]*"\s+              # "delimiter"
                                (?:"([^"]*)"|(.*))\r\n$  # Name or "Folder name"
                        /x;

        } 

        for my $f (@folders) { $f =~ s/^\\FOLDER LITERAL:://;}

        $self->{Folders} = \@folders;

        return wantarray ? @folders : \@folders ;
}


sub exists {
	my ($self,$what) = (shift,shift);
	return $self if $self->STATUS($self->Massage($what),"(MESSAGES)");
	return undef;
}
	
	
sub fetch {

	my $self = shift;
	$self->_imap_command( ( $self->Uid ? "UID " : "" ) .
				"FETCH " . (join(" ",@_)||'ALL')
	) 	 					or return undef;
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;

}
	
sub AUTOLOAD {

	my $self = shift;
	return undef if $Mail::IMAPClient::AUTOLOAD =~ /DESTROY$/;
	delete $self->{Folders}  ;
	my $cmd = $Mail::IMAPClient::AUTOLOAD =~ s/.*:://;
	if (scalar(@_)) {
		my @a = @_;
		if (	
			$Mail::IMAPClient::AUTOLOAD =~ 
				/^(?:subscribe|create|delete|lsub|search|myrights)$/i
		) {
			$a[-1] = $self->Massage($a[-1]) ;
		} elsif (
			$Mail::IMAPClient::AUTOLOAD =~ /^(?:store|copy)$/i
		) {
			$Mail::IMAPClient::AUTOLOAD = "UID $Mail::IMAPClient::AUTOLOAD"
				if $self->Uid;
		}
		print "Running: $Mail::IMAPClient::AUTOLOAD " . join(" ",@a) ,"\n" if $self->Debug;
		$self->_imap_command(qq/$Mail::IMAPClient::AUTOLOAD / . join(" ",@a) )  or return undef;
	} else {
		$self->_imap_command(qq/$Mail::IMAPClient::AUTOLOAD/) or return undef;
	}
	return wantarray ? 	$self->History($self->Count) 	: 
				$self->{"History"}{$self->Count}	;

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
				$self->{"History"}{$self->Count}	;

}

sub parse_headers {

	my($self,$msg,@fields) = @_;
	my $string; my $field;

	if ($fields[0] 	=~ 	/^[Aa][Ll]{2}$/ 	) { 

		$string = 	"$msg rfc822.header" 	; 
	} else {
		$string	= 	"$msg body[header.fields (" 	. 
				join(" ",@fields) 		. ')]' ;
	}
	my @raw=$self->fetch(	$string	) or return undef;
	
	my $h = {};

        for my $header (@raw) {
                next if $header =~ /^\*/;
                next if $header =~ /^\s+$/;
                # ( for vi
                last if $header =~ /^\)/;
                my $hdr = $header;
                chomp $hdr;
                $hdr =~ s/\r$//;   
                if ($hdr =~ s/^(\S+): //) { 
                        $field = $1 ;
                        push @{$h->{$field}} , $hdr ;
                } else {
                        $h->{$field}[-1] .= $hdr if ref($h->{$field}) eq 'ARRAY';
                }
        }

	return $h;
}

sub recent_count {
	my ($self, $folder) = (shift, shift);

	$self->status($folder, 'RECENT') or return undef;

	chomp(my $r = ( grep { s/\*\s+STATUS\s+.*\(RECENT\s+(\d+)\)/$1/ }
			$self->History($self->Transaction)
	)[0]);

	$r =~ s/\D//g;

	return $r;
}

sub message_count {
	
	my ($self, $folder) = (shift, shift);
	
	$self->status($folder, 'MESSAGES') or return undef;

	chomp(my $m =	   (	grep {s/\*\s+STATUS\s+.*\(MESSAGES\s+(\d+)\).*$/$1/ }
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

		$self->_imap_command( ($self->Uid ? "UID " : "") . "SEARCH $datum")
			 or return wantarray ? @hits : \@hits ;
		my @results =  $self->History($self->Count)     ;

		for my $r (@results) {

		       chomp $r;
		       $r =~ s/\r$//;
		       $r =~ s/^.*SEARCH\s+//;
		       push @hits, grep(/\d/,(split(/\s+/,$r)));

		}

		return wantarray ? @hits : \@hits;


        };
}
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
	local($^W = 0);
	eval {
		$self->logout if $self->IsConnected; 
		$self->Socket->close if ref($self->Socket);
	}
}


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
	my @msgs;
	for my $arg (@_) {
		if (ref($arg) eq 'ARRAY') {
			push @msgs , @{$arg};
		} else {
			push @msgs , split(/,/,$arg);
		}
	}
	

	$self->store(join(',',@msgs),'+FLAGS.SILENT','(\Deleted)') and $count++;

	return $count;
}

sub uidvalidity {

	my $self = shift; my $folder = $self->Massage(shift);

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

sub is_parent {
	my ($self, $folder) = (shift, shift);
        my $list = $self->list(undef, $folder);
	my $line;

        for (my $m = 0; $m < scalar(@$list); $m++ ) {
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
	my($f) = $line =~ /^\*\s+LIST\s+\(([^\)]*)\)/ if $line;
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
	my $text = join("\n",@_);
	$text =~ s/\n/\r\n/g;
        my $clear = $self->Clear;

        $self->Clear($clear)
                if $self->Count >= $clear and $clear > 0;

	my $count 	= $self->Count($self->Count+1);


        my $string = "$count APPEND $folder {" . length($text)  . "}\r\n" ;

        $self->_record($count,"$string\r\n");

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output) = ("","");	
	
	until ( ($code) = $output =~ /(^\+|^\d+\sNO|^\d+\sBAD)/) {
		$output = $self->_read_line;	
		$self->_record($count,$output);
		if ($output =~ /^\*\s+BYE/) {
			$self->State(Unconnected);
			return undef ;
		} elsif ( $output =~ /^\d+(NO|BAD)/ ) {
			return undef;
		}
	}	
	
        if ( $output =~ /^\d+\s+(BAD|NO)/ ) { 
                $output =~ s/^\d+\s+//;
                $self->LastError("Error trying to append: $output\n");
                return undef;
        }

	$self->_record($count,$text);
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
	if ($code !~ /^OK/) {
		return undef;
	}

	my($uid) = $output =~ m#\s+(\d+)\]#;

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

        $self->_record($count,"$string\r\n");

	my $feedback = $self->_send_line("$string");

	unless ($feedback) {
		$self->LastError("Error sending '$string' to IMAP: $!\n");
		return undef;
	}

	my ($code, $output);	
	
	until ( ($code) = $output =~ /^\+ (.*)\+$/) {
		$output = $self->_read_line;	
		$self->_record($count,$output);
		if ($output =~ /^\*\s+BYE/) {
			$self->State(Unconnected);
			return undef ;
		}
	}	
	
        return undef if $code =~ /^BAD|^NO/ ;

	my $reply = $response->($code) ;
        $feedback = $self->_send_line($response->($code));

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

	$target = $self->Massage($target);

	my $uids = $self->copy($target, map { ref($_) ? @{$_} : $_ } @msgs) or return undef;

	$self->delete_message(@msgs);
	
	return $uids;
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

# Status Routines:

sub Status 		{ return $_[0]->State 				;	}
sub IsUnconnected 	{ return ($_[0]->State == Unconnected)	? 1 : 0 ; 	}
sub IsConnected 	{ return ($_[0]->State >= Connected) 	? 1 : 0 ; 	}
sub IsAuthenticated 	{ return ($_[0]->State >= Authenticated)? 1 : 0 ; 	}
sub IsSelected 		{ return ($_[0]->State == Selected) 	? 1 : 0 ; 	}		

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

=head2 Class Methods

There are a couple of methods that can be invoked as class methods. Generally they can be 
invoked as an object method as well, as a convenience to the programmer. (That is, as a 
convenience to the programmer who wrote this module, as well as the programmers using it. 
It's easier I<not> to enforce a class method's classiness.) Note that if the B<new> method 
is called as an object method, the object returned is identical to what have would been 
returned if B<new> had been called as a class method. It doesn't give you a copy of the original 
object or anything like that.

=over 4

=item new

The B<new> method creates a new instance of an B<IMAPClient> object. If the I<Server> parameter
is passed as an argument to B<new>, then B<new> will implicitly call the B<connect> method. If
the I<Server> parameter is not supplied then the B<IMAPClient> object is created in the 
I<Unconnected> state.

=back

=cut

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
an argument, and returns that line with all carriage returns removed. If the input argument
had no carriage returns then it is returned unchanged. 

B<Strip_cr> does not remove new line characters.

=cut

=item Rfc822_date

The B<Rfc822_date> method accepts one input argument, a number of seconds since
the epoch date. It returns an RFC822 compliant date string for that date
(without the 'Date:' prefix). Useful for putting dates in messagestrings before
calling <append>.

=cut

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

=item Timeout

Specifies the timeout value in seconds for reads. Specifying a true value for I<Timeout> 
will prevent B<Mail::IMAPClient> from blocking in a read.

Since timeouts are implemented via the perl B<select> operator, the I<Timeout> parameter
may be set to a fractional number of seconds. Not supplying a I<Timeout>, or (re)setting it to 
zero, disables the timeout feature.

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

=item Uid

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

Note that if for some reason you only want the I<Uid> parameter turned on for one command, then
you can choose between the following two snippets, which are equivalent:

Example 1:

	$imap->Uid(1);
	my @hits = $imap->search('SUBJECT',"Just a silly test");
	$imap->Uid(0);

Example 2:

	my @hits 
       	foreach $r ($imap->uid("SEARCH","SUBJECT","Just a silly test") {
	       chomp $r;
	       $r =~ s/\r$//;
	       $r =~ s/^\*\s+SEARCH\s+// or next;
	       push @hits, grep(/\d/,(split(/\s+/,$r)));
	}

In the second example, we used the default method to issue the UID IMAP Client command, being 
careful to use different case so as not to inadvertently call the I<Uid> accessor method. Then
we parsed out the message UID's manually, since we don't have the benefit of the built-in 
B<search> method doing it for us.
	
Please be very careful when turning the I<Uid> parameter on and off throughout a script. If you loose
track of whether you've got the I<Uid> parameter turned on you might do something sad, like deleting
the wrong message. Remember, like all eponymous accessor methods, the B<Uid> method without arguments
will return the current value for the I<Uid> parameter, so do yourself a favor and check. The safest
approach is probably to turn it on at the beginning and then leave it on. (Remember that leaving it
turned off can lead to problems if changes to a folder's contents cause resequencing.) 

=back

Parameters can be set during B<new> method invocation by passing named parameter/value pairs
to the method, or later by calling the parameter's eponymous object method.

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

=item append

The B<append> method adds a message to the specified folder. It takes two arguments, the
name of the folder to append the message to, and the text of the message (including headers).
Additional arguments are added to the message text, separated with a newline.

The B<append> method returns the UID of the new message (a true value) if successful, 
or undef if not, if the IMAP server has the UIDPLUS capability.

=cut

=item authenticate

The B<authenticate> method accepts two arguments, an authentication type to be used (ie CRAM-MD5)
and a code or subroutine reference to execute to obtain a response. The B<authenticate> assumes that
the authentication type specified in the first argument follows a challenge-response flow. The 
B<authenticate> method issues the IMAP Client AUTHENTICATE command and receives a challenge from the
server. That challenge (minus any tag prefix or enclosing '+' characters but still base64 encoding)
is passed as the only argument to the code or subroutine referenced in the second argument. The return
value from the 2nd argument's code is written to the server as is, except that a <CR><NL> sequence is
appended if neccessary.

=cut

=item body_string

The B<body_string> method accepts a message sequence number (or a message UID, if the I<Uid> parameter 
is set to true) as an argument a returns the message body as a string. The returned value contains 
the entire message in one scalar variable, without the message headers.

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

=item copy

The B<copy> method requires a folder name as the first argument, and a list of one or more messages
sequence numbers (or messages UID's, if the I<UID> parameter is set to a true value). The message 
sequence numbers or UID's should refer to messages in the currenly selected folder. Those messages 
will be copies into the folder named in the first argument to the B<copy> method.

The B<copy> method returns undef on failure and a true value if successful. If the server to which 
the current Mail::IMAPClient object is connected supports the UIDPLUS capability then the true value
returned by B<copy> will be a comma separated list of UID's, which are the UID's of the newly copied 
messages in the target folder. 

=cut

=item delete_message

The B<delete_message> method accepts a list of arguments. If the I<Uid> parameter is not set to 
a true value, then each item in the list should be either: 

=item a message sequence number,

=item a comma-separated list of message sequence numbers, 

=item a reference to an array of message sequence numbers, or

If the I<Uid> parameter is set to a true value, then each item in the list should be either: 

=item a message UID, 

=item a comma-separated list of UID's, or 

=item a reference to an array of message UID's.

The messages identified by the sequence numbers or UID's will be deleted. B<delete_message> returns 
the number of messages it was told to delete. However, since the delete is done by issuing the 
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

B<NOTE SOME MORE:> In the grand tradition of the IMAP protocol, deleting a message doesn't actually
delete the message. Really. If you want to make sure the message has been deleted, you need to expunge
the folder (via the B<expunge> method, which is implemented via the default method).

B<See also:> The B<delete> method, to delete a folder, and B<expunge> and B<close> in RFC2060.

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
If the I<Uid> parameter is set to a true value then the first argument will be treated as
a UID or list of UID's, which means that the UID FETCH IMAP client command will be run instead
of FETCH. (It would really be a good idea at this point to review RFC2060.) 

=cut

=item flags

The B<flags> method implements the FETCH IMAP client command to list a single message's flags.
It accepts one argument, a message sequence number (or a message UID, if the I<Uid> parameter is
true), and returns an array (or a reference to an array, if called in scalar context) listing the 
flags that have been set. Flag names are provided with leading backslashes, if any. 

=cut

=item folders

The B<folders> method returns an array listing the available folders. It will only be 
successful if the object is in the B<Authenticated> or B<Selected> states.

=cut

=item has_capability

Returns true if the IMAP server to which the B<IMAPClient> object is connected has the capability
specified as an argument to B<has_capability>.

=item is_parent

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

The B<message_count> method accepts the name of a folder as an argument and returns the number
of messages in that folder. Internally, it invokes the B<status> method (see above) and 
parses out the results to obtain the number of messages. 

=cut

=item message_string

The B<message_string> method accepts a message sequence number (or message UID if I<Uid> is true)
as an argument a returns the message as a string. The returned value contains the entire message 
in one scalar variable, including the message headers.

=cut

=item message_uid

The B<message_uid> method accepts a message sequence number (or message UID if I<Uid> is true)
as an argument and returns the message's UID. Yes, if I<Uid> is true then it will use the IMAP 
UID FETCH UID client command to obtain and return the very same argument you supplied.  This is an 
IMAP feature so don't complain to me about it.

=cut

=item move

The B<move> method moves messages from the currently selected folder to the folder specified
in the first argument to B<move>.  If the I<Uid> parameter is not true, then the rest of the arguments 
should be either:

=over 8

=item a message sequence number,

=item a comma-separated list of message sequence numbers, or

=item a reference to an array of message sequence numbers.

If the I<Uid> parameter is true, then the arguments should be:

=item a message UID,

=item a comma-separated list of message UID's, or

=item a reference to an array of message UID's.

=back

If the target folder does not exist then it will be created.

If move is sucessful, then it returns a true value. Furthermore, if the B<Mail::IMAPClient> 
object is connected to a server that has the UIDPLUS capability, then the true value will 
be the list of UID's for the newly copied messages. The list will be in the order in which the
messages were moved. (Since B<move> uses the copy method, the messages will be moved in numerical
order.)

If the move is not successful then B<move> returns undef.

=cut

=item parse_headers 

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

If the I<Uid> parameter is true then the first argument will be treated as a message UID.

Currently, specifying a range of message numbers as the first argument is not supported. 

=cut

=item recent

The B<recent> method performs an IMAP SEARCH RECENT search against the selected folder and returns
an array of sequence numbers (or UID's, if the I<Uid> parameter is true) of messages that are recent.

=cut

=item recent_count

The B<recent_count> method accepts as an argument a folder name. It returns the number of recent 
messages in the folder (as returned by the IMAP client command "STATUS folder RECENT"), or undef 
in the case of an error. The B<recent_count> method was contributed by Rob Deker (deker@ikimbo.com).

=cut

=item rename

The B<rename> method accepts two arguments: the name of an existing folder, and a new name for
the folder. The existing folder will be renamed to the new name using the RENAME IMAP client 
command. B<rename> will return a true value if successful, or undef if unsuccessful.

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
SEARCH IMAP client command's search criteria. If the I<Uid> parameter is true then the array
will contain message UID's. If B<search> is called in scalar context then a pointer to the array
will be passed, instead of the array itself.

=cut

=item seen

The B<seen> method performs an IMAP SEARCH SEEN search against the selected folder and returns
an array of sequence numbers of messages that have already been seen (ie their SEEN flag is set).
If the I<Uid> parameter is true then an array of message UID's will be returned instead. If called
in scalar context than a pointer to the array (rather than the array itself) will be returned.

=cut

=item select

The B<select> method selects a folder and changes the object's state to "Selected".
It accepts one argument, which is the name of the folder to select.

=cut

=item separator

The B<separator> method returns the character used as a separator character in 
folder hierarchies. On unix-based servers, this is often a forward slash (/). It accepts one
argument, the name of a folder whose hierarchy's separator should be returned. If no folder
name is supplied then the separator for the INBOX is returned, which probably is good enough.

=cut

=item setacl

The B<setacl> method accepts three input arguments, a folder name, a user id (or authentication
identifier, to use the terminology of RFC2086), and an access rights modification string. See
RFC2086 for more information.

=cut

=item size

The B<size> method accepts one input argument, a sequence number (or message UID if the I<Uid> parameter
is true). It returns the size of the message in the currently selected folder with the supplied 
sequence number (or UID).  The B<IMAPClient> object must be in a I<Selected> state in order to use 
this method.

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

=item uidnext

The B<uidnext> method accepts one argument, the name of a folder, and returns the numeric string
that is the next available message UID for that folder.

=item uidvalidity

The B<uidvalidity> method accepts one argument, the name of a folder, and returns the numeric string
that is the unique identifier validity value for the folder.

=item unseen

The B<unseen> method performs an IMAP SEARCH UNSEEN search against the selected folder and returns
an array of sequence numbers of messages that have not yet been seen (ie their SEEN flag is not set).
If the I<Uid> parameter is true then an array of message UID's will be returned instead. If called
in scalar context than a pointer to the array (rather than the array itself) will be returned.

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

You can also use the default method to override the behavior of implemented IMAP methods by
changing the case of the method name, preferably to all-uppercase so as not to conflict with
the Class method and accessor method namespace. For example, if you don't want the B<search>
method's behavior (which returns a list of message numbers) but would rather have an array
of raw data returned from your B<search> operator, you can issue the following snippet:
	
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

b) the GNU General Public License as published by the Free Software Foundation; either 
version 1, or (at your option) any later version.

=back

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See either
the GNU General Public License or the Artistic License for more details.

=cut

my $not_void_context = '0 but true'; 		# return true value

# $Log: IMAPClient.pm,v $
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

