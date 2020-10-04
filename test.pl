use v5.18;
use strict;
use JSON::PP;
use warnings;
use feature qw(
    say
);

sub blorg {
    say shift;
    return undef;
}

blorg(9932.2);
blorg("foo" . "bar");