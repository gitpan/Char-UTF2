# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

print "1..2\n";

my $__FILE__ = __FILE__;

if ($^O eq 'MacOS') {
    print "ok - 1 # SKIP $^X $__FILE__\n";
    print "ok - 2 # SKIP $^X $__FILE__\n";
    exit;
}

my $null = '';
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    if ($ENV{'COMSPEC'} =~ / \\COMMAND\.COM \z/oxmsi) {
        $null = '';
    }
    else {
        $null = '2>NUL';
    }
}
else{
    $null = '2>/dev/null';
}

my $script = __FILE__ . '.pl';

open(TEST,">$script") || die "Can't open file: $script\n";
print TEST <<'END';
use Char::UTF2;
'-' =~ /(あ[あ-い])/;
print "PASS\n";
END
close(TEST);
eval {
    $result = qx{$^X $script $null};
};
if ($result =~ /PASS/) {
    print "ok - 1 $^X $__FILE__ die ('-' =~ /あ[あ-い]/).\n";
}
else {
    print "not ok - 1 $^X $__FILE__ die ('-' =~ /あ[あ-い]/).\n";
}
unlink("$script");
unlink("$script.e");

open(TEST,">$script") || die "Can't open file: $script\n";
print TEST <<'END';
use Char::UTF2;
'-' =~ /(あ[い-あ])/;
print "PASS\n";
END
close(TEST);
eval {
    $result = qx{$^X $script $null};
};
if ($result !~ /PASS/) {
    print "ok - 2 $^X $__FILE__ die ('-' =~ /あ[い-あ]/).\n";
}
else {
    print "not ok - 2 $^X $__FILE__ die ('-' =~ /あ[い-あ]/).\n";
}
unlink("$script");
unlink("$script.e");

__END__

