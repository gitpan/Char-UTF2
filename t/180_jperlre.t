# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..1\n";

my $__FILE__ = __FILE__;

eval q< '-' =~ /((*)い)/ >;
if ($@) {
    print "ok - 1 $^X $__FILE__ die ('-' =~ /(*)い/).\n";
}
else {
    print "not ok - 1 $^X $__FILE__ die ('-' =~ /(*)い/).\n";
}

__END__
