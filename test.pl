use v5.18;
use strict;
use JSON::PP;
use warnings;
use feature qw(
    say
);

BEGIN {
    warn "omg";
}

my $bar = rand();
my $array_ref = ['a', "blorg $bar", qr/match/];
my $hash_ref = { $bar => 5 };
my %hash = ('what' => "is this");
my @list = ($array_ref, $array_ref, $hash_ref, %hash);
my $var_not_named_scalar = 'abc';

sub blorg {
    my $arg = shift();
    say shift;
    shift();
    return $arg + 53;
}

blorg(9932.2);
blorg("foo" . "bar");