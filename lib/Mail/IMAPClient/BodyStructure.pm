# Copyrights 2008.
#  For other contributors see Changes.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 1.04.
use warnings;
use strict;

package Mail::IMAPClient::BodyStructure;
use vars '$VERSION';
$VERSION = '3.07';

use Mail::IMAPClient::BodyStructure::Parse;

# my has file scope, not limited to package!
my $parser = Mail::IMAPClient::BodyStructure::Parse->new
   or die "Cannot parse rules: $@\n"
        . "Try remaking Mail::IMAPClient::BodyStructure::Parse.\n";

sub new
{   my $class = shift;
    my $bodystructure = shift;

    my $self  = $parser->start($bodystructure)
       or return undef;

    $self->{_prefix} = "";
    $self->{_id}     = exists $self->{bodystructure} ? 'HEAD' : 1;
    $self->{_top}    = 1;

    bless $self, ref($class)||$class;
}

sub _get_thingy
{   my $thingy = shift;
    my $object = shift || (ref $thingy ? $thingy : undef);

    unless ($object && ref $object)
    {   warn $@ = "No argument passed to $thingy method.";
        return undef;
    }

    unless(UNIVERSAL::isa($object, 'HASH') && exists $object->{$thingy})
    {   my $a = $thingy =~ /^[aeiou]/i ? 'an' : 'a';
        my $has = ref $object eq 'HASH' ? join(", ",keys %$object) : '';
        warn $@ = ref($object)." $object does not have $a $thingy. "
                . ($has ? "It has $has" : '');
        return undef;
    }

    my $value = $object->{$thingy};
    $value    =~ s/\\ ( [\\\(\)"\x0d\x0a] )/$1/gx;
    $value    =~ s/^"(.*)"$/$1/;
    $value;
}

BEGIN
{   no strict 'refs';
    foreach my $datum (
       qw/ bodytype bodysubtype bodyparms bodydisp bodyid bodydesc bodyenc
        bodysize bodylang envelopestruct textlines / )
    {   *$datum = sub { _get_thingy($datum, @_) };
    }
}

sub parts
{   my $self = shift;
    return wantarray ? @{$self->{PartsList}} : $self->{PartsList}
        if exists $self->{PartsList};

    my @parts;
    $self->{PartsList} = \@parts;

    unless(exists $self->{bodystructure})
    {   $self->{PartsIndex}{1} = $self;
        @parts = ("HEAD", 1);
        return wantarray ? @parts : \@parts;
    }

    foreach my $p ($self->bodystructure)
    {   my $id = $p->id;
        push @parts, $id;
        $self->{PartsIndex}{$id} = $p ;
        my $type = uc $p->bodytype || '';

        push @parts, "$id.HEAD"
            if $type eq 'MESSAGE';
    }

    wantarray ? @parts : \@parts;
}

sub bodystructure
{   my $self   = shift;
    my $partno = 0;
    my @parts;

    if($self->{_top})
    {   $self->{_id}     ||= "HEAD";
        $self->{_prefix} ||= "HEAD";
        $partno = 0;
        foreach my $b ( @{$self->{bodystructure}} )
        {   $b->{_id}     = ++$partno;
            $b->{_prefix} = $partno;
            push @parts, $b, $b->bodystructure;
        }
        return wantarray ? @parts : \@parts;
    }

    my $prefix = $self->{_prefix} || "";
    $prefix    =~ s/\.?$/./;

    foreach my $p ( @{$self->{bodystructure}} )
    {   $partno++;
        $p->{_prefix} = "$prefix$partno";
        $p->{_id}   ||= "$prefix$partno";
        push @parts, $p, $p->{bodystructure} ? $p->bodystructure : ();
    }

    wantarray ? @parts : \@parts;
}

sub id
{   my $self = shift;
    return $self->{_id}
        if exists $self->{_id};

    return "HEAD"
        if $self->{_top};

    if ($self->{bodytype} eq 'MULTIPART')
    {   my $p = $self->{_id} || $self->{_prefix};
        $p =~ s/\.$//;
        return $p;
    }
    else
    {   return $self->{_id}  ||= 1;
    }
}

package Mail::IMAPClient::BodyStructure::Part;
use vars '$VERSION';
$VERSION = '3.07';

our @ISA = qw/Mail::IMAPClient::BodyStructure/;

package Mail::IMAPClient::BodyStructure::Envelope;
use vars '$VERSION';
$VERSION = '3.07';

our @ISA = qw/Mail::IMAPClient::BodyStructure/;

sub new
{   my ($class, $envelope) = @_;
    $parser->envelope($envelope);
}

sub from_addresses    { shift->_addresses(from    => 1) }
sub sender_addresses  { shift->_addresses(sender  => 1) }
sub replyto_addresses { shift->_addresses(replyto => 1) }
sub to_addresses      { shift->_addresses(to      => 0) }
sub cc_addresses      { shift->_addresses(cc      => 0) }
sub bcc_addresses     { shift->_addresses(bcc     => 0) }

sub _addresses($$$)
{   my ($self, $name, $isSender) = @_;
    ref $self->{$name} eq 'ARRAY'
        or return ();

    my @list;
    foreach ( @{$self->{$name}} )
    {   my $pn   = $_->personalname;
        my $name = $pn && $pn ne 'NIL' ? "$pn " : '';
        push @list, $pn. '<'.$_->mailboxname .'@'.  $_->hostname.'>';
    }

      wantarray ? @list
    : $isSender ? $list[0]
    :             \@list;
}

BEGIN
{   no strict 'refs';
    for my $datum ( qw(subject inreplyto from messageid bcc date
                       replyto to sender cc))
    {  *$datum = sub { @_ > 1 ? $_[0]->{$datum} = $_[1] : $_[0]->{$datum} }
    }
}


package Mail::IMAPClient::BodyStructure::Address;
use vars '$VERSION';
$VERSION = '3.07';

our @ISA = qw/Mail::IMAPClient::BodyStructure/;

for my $datum ( qw(personalname mailboxname hostname sourcename) )
{   no strict 'refs';
    *$datum = sub { shift->{$datum}; };
}

1;

__END__


