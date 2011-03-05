# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

my $__FILE__ = __FILE__;

use Char::UTF2;
print "1..1\n";

if (1) {
    print "ok - 1 # SKIP $^X $0\n";
    exit;
}

open(FILE,'>F機能') || die "Can't open file: F機能\n";
print FILE "1\n";
close(FILE);

# glob
@glob = glob('./*');
if (grep(/F機能/,@glob)) {
    print "ok - 1 glob $^X $__FILE__\n";
}
else {
    print "not ok - 1 glob: ", (map {"($_)"} @glob), ": $! $^X $__FILE__\n";
}

unlink('F機能');

__END__
