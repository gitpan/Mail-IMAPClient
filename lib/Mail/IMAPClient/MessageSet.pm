# Copyrights 2008.
#  For other contributors see Changes.
# See the manual pages for details on the licensing terms.
# Pod stripped from pm file by OODoc 1.04.
use warnings;
use strict;

package Mail::IMAPClient::MessageSet;
use vars '$VERSION';
$VERSION = '3.07';



use overload
    '""'     => "str"
  , '.='     => sub {$_[0]->cat($_[1])}
  , '+='     => sub {$_[0]->cat($_[1])}
  , '-='     => sub {$_[0]->rem($_[1])}
  , '@{}'    => "unfold"
  , fallback => 1;

sub new
{   my $class = shift;
    my $range = $class->range(@_);
    bless \$range, $class;
}

sub str { overload::StrVal( ${$_[0]} ) }

sub _unfold_range($)
# {   my $x = shift; return if $x =~ m/[^0-9,:]$/; $x =~ s/\:/../g; eval $x; }
{   map { /(\d+)\s*\:\s*(\d+)/ ? ($1..$2) : $_ }
        split /\,/, shift;
}

sub rem
{   my $self   = shift;
    my %delete = map { ($_ => 1) } map { _unfold_range $_ } @_;
    $$self     = $self->range(grep {not $delete{$_}} $self->unfold);
    $self;
}

sub cat
{   my $self = shift;
    $$self = $self->range($$self, @_);
    $self;
}

sub range
{   my $self = shift;

    my @msgs;
    foreach my $m (@_)
    {   defined $m && length $m
            or next;

        foreach my $mm (ref $m eq 'ARRAY' ? @$m : $m)
        {   push @msgs, _unfold_range $mm;
        }
    }

    @msgs
        or return undef;

    @msgs = sort {$a <=> $b} @msgs;
    my $low = my $high = shift @msgs;

    my @ranges;
    foreach my $m (@msgs)
    {   next if $m == $high; # double

        if($m == $high + 1) { $high = $m }
        else
        {   push @ranges, $low == $high ? $low : "$low:$high";
            $low = $high = $m;
        }
    }

    push @ranges, $low == $high ? $low : "$low:$high" ;
    join ",", @ranges;
}

sub unfold
{   my $self = shift;
    wantarray ? ( _unfold_range $$self ) : [ _unfold_range $$self ];
}


1;
