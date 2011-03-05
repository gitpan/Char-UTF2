# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

print "1..1\n";

my $__FILE__ = __FILE__;

my $null = '/dev/null';
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    $null = 'NUL';
}

my $script = __FILE__ . '.pl';
open(TEST,">$script") || die "Can't open file: $script\n";
print TEST <DATA>;
close(TEST);

if (system(qq{$^X $script 2>$null}) != 0) {
    print "ok - 1 $^X $__FILE__ die ('-' =~ /あ[い-あ]/).\n";
}
else {
    print "not ok - 1 $^X $__FILE__ die ('-' =~ /あ[い-あ]/).\n";
}

__END__
# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;

'-' =~ /(あ[い-あ])/;

exit 0;
