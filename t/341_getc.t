# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..1\n";

my $__FILE__ = __FILE__;

if ($] < 5.006) {
    print qq{ok - 1 # SKIP $^X $] $__FILE__\n};
    exit;
}

open(FILE,">$__FILE__.txt") || die;
print FILE <DATA>;
close(FILE);
open(my $fh,"<$__FILE__.txt") || die;

my @getc = ();
while (my $c = Char::UTF2::getc($fh)) {
    last if $c eq "\n";
    push @getc, $c;
}
close($fh);
unlink("$__FILE__.txt");

my $result = join('', map {"($_)"} @getc);

if ($result eq '(1)(2)(ｱ)(ｲ)(あ)(い)') {
    print "ok - 1 $^X $__FILE__ 12ｱｲあい --> $result.\n";
}
else {
    print "not ok - 1 $^X $__FILE__ 12ｱｲあい --> $result.\n";
}

__END__
12ｱｲあい
