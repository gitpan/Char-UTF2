# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..2\n";

my $__FILE__ = __FILE__;

if (length('あいうえお') == 15) {
    print qq{ok - 1 length('あいうえお') == 15 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 length('あいうえお') == 15 $^X $__FILE__\n};
}

if (Char::UTF2::length('あいうえお') == 5) {
    print qq{ok - 2 Char::UTF2::length('あいうえお') == 5 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 Char::UTF2::length('あいうえお') == 5 $^X $__FILE__\n};
}

__END__
