package Mail::IMAPClient::BodyStructure;
#$Id$
use Parse::RecDescent;
use Mail::IMAPClient;
#use Data::Dumper;

$Mail::IMAPClient::BodyStructure::VERSION = '0.0.2';
# Do it once more to show we mean it!
$Mail::IMAPClient::BodyStructure::VERSION = '0.0.2'; 

my $rules = <<'END_RULES';	# Don't mess with the following here doc unless you know what you're doing
	# Directives
	# (none)
	# Atoms
	TEXT:		/^"TEXT"|^TEXT/i 	{ $return = "TEXT" }
	MESSAGE:	/^"MESSAGE"|^MESSAGE/i 	{ $return = "MESSAGE" 	}
	RFC822:		/^"RFC822"|^RFC822/i  	{ $return = "RFC822" 	}
	NIL:		/^NIL/i			{ $return = "NIL" 	}
	NUMBER:		/^(\d+)/		{ $return = $item[1]; $return||defined($return);}

        STRING:         /^"((?:[^"\\]|\\.)*)"/ | /^([^ \(\)]+)/ 
                        {       $item{STRING} =~ s/^"(.*)"$/$1/;
                                $return = $item{STRING} || $item{__PATTERN1__} ;
                                $return||defined($return);
                        }

	BARESTRING:	/^[^(]+\s+(?=\()/	
			{ $return = $item[1] ; $return||defined($return);}

	textlines:	NIL | NUMBER	{ $return = $item[1] || $item[2]; $return||defined($return); }
	rfc822message:  MESSAGE RFC822 	{ $return = "MESSAGE RFC822" }
	key:		STRING		{ $return = $item{STRING} ; $return||defined($return);}
	value:		NIL | NUMBER | STRING | "(" kvpair(s) ")"  
			{ 	$return = $item{NIL} 		|| 
				$item{NUMBER} 			|| 
				$item{STRING} 			|| 
				{ map { (%$_) } @{$item{kvpair}} } ;
				$return||defined($return);
			}
	kvpair:		key value 	
			{ $return = { $item{key} => $item{value} }; $return||defined($return);}
	bodytype:	STRING		
			{ $return = $item{STRING} ; $return||defined($return);}
	bodysubtype:	NIL | STRING	
			{ $return = $item{NIL}||$item{STRING} ; $return||defined($return);}
	bodyparms:	NIL |  '(' kvpair(s) ')' 		
			{
                          $return = $item{NIL} || 
                                    { map { (%$_) } @{$item{kvpair}} };
                          $return || defined($return); 
                        }
	bodydisp:	NIL |  '(' kvpair(s) ')'		
			{
                          $return = $item{NIL} || 
                                    { map { (%$_) } @{$item{kvpair}} };
                          $return || defined($return); 
                        }
	bodyid:		NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ; $return||defined($return);}
	bodydesc:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ; $return||defined($return);}
	bodyenc:	NIL | STRING |  '(' kvpair(s) ')'
		{
			$return = $item{NIL} 		|| 
				  $item{STRING} 	||
				  { map { (%$_) } @{$item{kvpair}} };
			$return||defined($return);
		}
	bodysize:	NIL | NUMBER		
			{ $return = $item{NIL} || $item{NUMBER} ;$return||defined($return);}
	bodyMD5:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	bodylang:	NIL | STRING | "(" STRING(s) ")"
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	personalname:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	sourceroute:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	mailboxname:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	hostname:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}
	addressstruct: "(" personalname sourceroute mailboxname hostname ")"
			{ $return = {
				personalname => $item{personalname} ,	
				sourceroute  => $item{sourceroute} ,	
				mailboxname  => $item{mailboxname} ,	
				hostname     => $item{hostname} ,	
			  }
			}
	subject:	NIL | STRING 
			{ 
				$return = $item{NIL} || $item{STRING} ;
				$return||defined($return);
			}
	inreplyto:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}

	messageid:	NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}

	date:		NIL | STRING		
			{ $return = $item{NIL} || $item{STRING} ;$return||defined($return);}

	cc:		NIL | "(" addressstruct(s) ")" 		
			{ $return = $item{NIL} || $item{addressstruct} }

	bcc:		NIL | "(" addressstruct(s) ")" 		
			{ $return = $item{NIL} || $item{addressstruct} }

	from:		NIL | "(" addressstruct(s) ")"		
			{ $return = $item{NIL} || $item{addressstruct} }

	replyto:	NIL | "(" addressstruct(s) ")"		
			{ $return = $item{NIL} || $item{addressstruct} }

	sender:		NIL | "(" addressstruct(s) ")"		
			{ $return = $item{NIL} || $item{addressstruct} }

	to:		NIL | "(" addressstruct(s) ")"		
			{ $return = $item{NIL} || $item{addressstruct} }

	basicfields: 	bodysubtype bodyparms bodyid(?) bodydesc(?) bodyenc(?) bodysize(?) 
		{ $return = { 
			bodysubtype 	=> $item{bodysubtype} ,
			bodyparms 	=> $item{bodyparms} ,
			bodyid 		=> $item{bodyid} ,
			bodydesc 	=> $item{bodydesc} ,
			bodyenc 	=> $item{bodyenc} ,
			bodysize 	=> $item{bodysize} ,
		  };
		  $return;
		}

	textmessage: 	TEXT <commit> basicfields textlines(?) bodyMD5(?) bodydisp(?) bodylang(?) 
		{ 
		  $return = $item{basicfields}||{};
		  $return->{bodytype} = 'TEXT';
		  foreach my $what (qw/textlines bodyMD5 bodydisp bodylang/) {
			$return->{$what} = $item{$what};
		  }
		  $return||defined($return);
	        }

	othertypemessage: bodytype basicfields bodyparms(?) bodydisp(?) bodylang(?)
		{ $return = {}; 
		  foreach my $what (qw/bodytype bodyparms bodydisp bodylang/) {
			$return->{$what} = $item{$what};
		  }
		  while ( my($k,$v) = each %{$item{basicfields}} ) { $return->{$k} = $v }
		  $return||defined($return);
		}

	envelopestruct:	"(" date subject from sender replyto to cc bcc inreplyto messageid ")" 
		{ $return = {}; 
		  foreach my $what (qw/date subject from sender replyto to cc bcc inreplyto messageid/) {
			$return->{$what} = $item{$what};
		  }
		  $return||defined($return);
		}

	messagerfc822message: 
			rfc822message <commit> bodyparms bodyid bodydesc bodyenc bodysize 
			envelopestruct bodystructure textlines
			bodyMD5(?) bodydisp(?) bodylang(?) 
		{ 
		  $return = {}; 
		  $return->{bodytype} 	= "MESSAGE" ; 
		  $return->{bodysubtype}= "RFC822" ;
		  foreach my $what (qw/	bodyparms bodyid bodydesc bodyenc bodysize 
					envelopestruct bodystructure textlines
					bodyMD5 bodydisp bodylang
		  		     /
		  ) {
			$return->{$what} = $item{$what};
		  }
		  while ( my($k,$v) = each %{$item{basicfields}} ) { $return->{$k} = $v }
		  $return||defined($return);
		}

	subpart:	"(" part ")"  	
		{ 
			$return = $item{part} ; 
			$return||defined($return);
		}


	part:		subpart(s) <commit> basicfields 
				bodyparms(?) bodydisp(?) bodylang(?) 	
		{
			$return = bless($item{basicfields}, "Mail::IMAPClient::BodyStructure");
			$return->{bodytype} = "MULTIPART";
			$return->{bodystructure} = $item{subpart};
			foreach my $b (qw/bodyparms bodydisp bodylang/) { 
				$return->{$b} = $item{$b};
			}
			$return||defined($return) ;
		}
			| 	textmessage <commit> 				
		{
			$return = bless $item{textmessage}, "Mail::IMAPClient::BodyStructure";
			$return||defined($return);
		}
			| 	messagerfc822message <commit> 		
		{
			$return = bless $item{messagerfc822message}, "Mail::IMAPClient::BodyStructure";
			$return||defined($return);
		}
			| 	othertypemessage
		{
			$return = bless $item{othertypemessage}, "Mail::IMAPClient::BodyStructure";
			$return||defined($return);
		}

	bodystructure: 	 "(" part(s) ")"
			{
				$return = $item{part} ;
				$return||defined($return);
			}

	start:		/.*\(.*BODYSTRUCTURE \(/i part(s)  /\)\)\r?\n?/
			{
				$return = $item{part} ;				
				$return||defined($return);
			}
				
END_RULES

my $parser = Parse::RecDescent->new($rules) 
	or die "Cannot parse rules: $@" and return undef;

sub new {
	my $class = shift;
	my $bodystructure = shift;
	
	my $values = $parser->start($bodystructure);
	return $values->[0];
}

sub _get_thingy {
	my $thingy = shift;
	my $object = shift||(ref($thingy)?$thingy:undef);
	unless ( defined($object) and ref($object) ) {
		$@ = "No argument passed to $thingy method." 	;
		$^W and print STDERR "$@\n" ;
		return undef;
	}
	unless ( defined($object->{$thingy}) ) {
		$@ = 	ref($object) 					.
			" $object does not have " 			. 
			( $thingy =~ /^[aeiou]/i ? "an " : "a " ) 	.
			"${thingy}.";  
		$^W and print STDERR "$@\n" ;
		return undef;
	}
	if ( $thingy eq 'bodytype' and ref($object->{bodytype}) )	{ return "MULTIPART" }
	return Unwrapped($object->{$thingy});

}

BEGIN {
 foreach my $datum (qw/	bodytype bodysubtype 	bodyparms 	bodydisp bodyid
			bodydesc bodyenc 	bodysize 	bodylang 
			envelopestruct  	textlines
		   /
 ) {
        no strict 'refs';
        *$datum = sub { _get_thingy($datum, @_); };
 }

}

sub parts {
	my $self = shift;

	my @parts = ();
	if ($self->bodytype() ne 'MULTIPART' && $self->bodytype() ne 'MESSAGE') {
		@parts = ( 1 );
	}
	else {
		foreach my $p ($self->bodystructure()) {
			if ($p->bodytype() eq 'MULTIPART') {
				push @parts, $p->parts();
			}
			elsif ($p->bodytype() eq 'MESSAGE') {
				push @parts, $p->parts();
			}
			else {
				push @parts, $p->id();
			}
		}
	}
	return wantarray ? @parts : \@parts;
}

sub bodystructure {
	my $self = shift;
	my @parts = ();
	my $partno = 0;

        my $prefix = $self->{prefix} || "";

	if (exists $self->{bodystructure} && ref($self->{bodystructure}) =~ /^ARRAY/) {
		my $bs = $self->{bodystructure};
		if (scalar @$bs == 1 && $bs->[0]{bodytype} eq 'MULTIPART') {
			$bs->[0]{prefix} = $prefix;
                }
        	$prefix = "$prefix." if $prefix;
		foreach my $p ( @{$self->{bodystructure}}  ) {
			$partno++;
			if (! exists $p->{prefix} ) {
                        	$p->{id} = "$prefix$partno";
				$p->{prefix} = "$prefix$partno";
			}
			push @parts, $p;	
		}
	}
	return wantarray ? @parts : \@parts;
}

sub id {
	my $self = shift;
	
	return $self->{id} if exists $self->{id};

	if ($self->{bodytype} eq 'MULTIPART') {
		return undef;
	} else {
		return $self->{id} = 1;
	}
}

sub Unwrapped {
	my $unescape = Mail::IMAPClient::Unescape(@_);
	$unescape =~ s/^"(.*)"$/$1/;
	return $unescape;
}






1;
__END__

=head1 NAME

Mail::IMAPClient::BodyStructure - Perl extension to Mail::IMAPClient to facilitate the parsing of
server responses to the FETCH BODYSTRUCTURE IMAP client command.

=head1 SYNOPSIS

  use Mail::IMAPClient::BodyStructure;
  use Mail::IMAPClient;

  my $imap = Mail::IMAPClient->new(Server=>$serv,User=>$usr,Password=>$pwd);
  $imap->select("INBOX") or die "cannot select the inbox for $usr: $@\n";

  my @recent = $imap->search("recent");

  foreach my $new (@recent) {

	my $struct = Mail::IMAPClient::BodyStructure->new($imap->fetch($new,"bodystructure"));

	print	"Msg $new (Content-type: ",$struct->bodytype,"/",$struct->bodysubtype,
        	") contains these parts:\n\t",join("\n\t",$struct->parts),"\n\n";


  }


  

=head1 DESCRIPTION

This extension will parse the result of an IMAP FETCH BODYSTRUCTURE command into a perl 
data structure. It also provides helper methods that will help you pull information out 
of the data structure.

Use of this extension requires Parse::RecDescent. If you don't have Parse::RecDescent 
then you must either get it or refrain from using this module.

=head2 EXPORT

There are no restrictions on exporting this module out of the US. (Oh, did you want to know 
what variables are exported by default or exportable upon request? There aren't any.)

=head1 Class Methods

The following class method is available:

=head2 new

This class method is the constructor method for instantiating new Mail::IMAPClient::BodyStructure 
objects. The B<new> method accepts one argument, a string containing a server response to a 
FETCH BODYSTRUCTURE directive.  Only one message's body structure should be described in this 
string, although that message may contain an arbitrary number of parts.

If you know the messages sequence number or unique ID (UID) but haven't got its body structure, 
and you want to get the body structure and parse it into a B<Mail::IMAPClient::BodyStructure> object, then you might as well save yourself some work and use B<Mail::IMAPClient>'s B<get_bodystructure> 
method, which accepts a message sequence number (or UID if I<Uid> is true) and returns a 
B<Mail::IMAPClient::BodyStructure> object. It's functionally equivalent to issuing the 
FETCH BODYSTRUCTURE IMAP client command and then passing the results to 
B<Mail::IMAPClient::BodyStructure>'s B<new> method but it does those things in one simple 
method call.

=head1 Object Methods

The following object methods are available:

=head2 bodytype

The B<bodytype> object method requires no arguments.  
It returns the bodytype for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut

=head2 bodysubtype

The B<bodysubtype> object method requires no arguments.  
It returns the bodysubtype for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut



=head2 bodyparms

The B<bodyparms> object method requires no arguments.  
It returns the bodyparms for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodydisp

The B<bodydisp> object method requires no arguments.  
It returns the bodydisp for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodyid

The B<bodyid> object method requires no arguments.  
It returns the bodyid for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodydesc

The B<bodydesc> object method requires no arguments.  
It returns the bodydesc for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodyenc

The B<bodyenc> object method requires no arguments.  
It returns the bodyenc for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut



=head2 bodysize

The B<bodysize> object method requires no arguments.  
It returns the bodysize for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodylang

The B<bodylang> object method requires no arguments.  
It returns the bodylang for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 bodystructure

The B<bodystructure> object method requires no arguments.  
It returns the bodystructure for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 envelopestruct

The B<envelopestruct> object method requires no arguments.  
It returns the envelopestruct for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


	
=head2 textlines

The B<textlines> object method requires no arguments.  
It returns the textlines for the message whose structure is described by the calling 
B<Mail::IMAPClient::Bodystructure> object.

=cut


=head1 AUTHOR

David J. Kernen

=head1 SEE ALSO

perl(1), Mail::IMAPClient, and RFC2060.

=cut


# History: 
# $Log$
