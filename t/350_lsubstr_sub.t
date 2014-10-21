# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

print "1..4\n";

my $__FILE__ = __FILE__;

if ($] < 5.014) {
    for my $tno (1..4) {
        print "ok - $tno # SKIP $^X $__FILE__\n";
    }
    exit;
}

if (open(TEST,">@{[__FILE__]}.t")) {
    print TEST <DATA>;
    close(TEST);
    system(qq{$^X @{[__FILE__]}.t});
    unlink("@{[__FILE__]}.t");
    unlink("@{[__FILE__]}.t.e");
}

__END__
use Char::UTF2;

my $__FILE__ = __FILE__;

$var = '0123456789';
Char::UTF2::substr($var,4,2) =~ s/[0-9]+/KK/;
if ($var eq '0123KK6789') {
    print "ok - 1 $^X $__FILE__\n";
}
else {
    print "not ok - 1 $^X $__FILE__\n";
}

$var = '0123456789';
Char::UTF2::substr($var,4,3) =~ s/[0-9]/H/g;
if ($var eq '0123HHH789') {
    print "ok - 2 $^X $__FILE__\n";
}
else {
    print "not ok - 2 $^X $__FILE__\n";
}

$var = '0123456789';
my $var2 = Char::UTF2::substr($var,4,4) =~ s/[0-9]/I/r;
if (($var2 eq 'I567') and ($var eq '0123456789')) {
    print "ok - 3 $^X $__FILE__\n";
}
else {
    print "not ok - 3 $^X $__FILE__\n";
}

$var = '0123456789';
$var2 = Char::UTF2::substr($var,4,5) =~ s/[0-9]/I/gr;
if (($var2 eq 'IIIII') and ($var eq '0123456789')) {
    print "ok - 4 $^X $__FILE__\n";
}
else {
    print "not ok - 4 $^X $__FILE__\n";
}

__END__
