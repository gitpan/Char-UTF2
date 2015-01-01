# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;

print "1..12\n";

# Char::UTF2::eval q{...} has Char::UTF2::eval "..."
if (Char::UTF2::eval q{ Char::UTF2::eval " if ('□●' !~ /[◆]/) { return 1 } else { return 0 } " }) {
    print qq{ok - 1 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 1 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval qq{...}
if (Char::UTF2::eval q{ Char::UTF2::eval qq{ if ('□●' !~ /[◆]/) { return 1 } else { return 0 } } }) {
    print qq{ok - 2 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 2 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval '...'
if (Char::UTF2::eval q{ Char::UTF2::eval ' if ("□●" !~ /[◆]/) { return 1 } else { return 0 } ' }) {
    print qq{ok - 3 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 3 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval q{...}
if (Char::UTF2::eval q{ Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return 1 } else { return 0 } } }) {
    print qq{ok - 4 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 4 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval $var
my $var = q{ if ('□●' !~ /[◆]/) { return 1 } else { return 0 } };
if (Char::UTF2::eval q{ Char::UTF2::eval $var }) {
    print qq{ok - 5 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 5 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval (omit)
$_ = "if ('□●' !~ /[◆]/) { return 1 } else { return 0 }";
if (Char::UTF2::eval q{ Char::UTF2::eval }) {
    print qq{ok - 6 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 6 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has Char::UTF2::eval {...}
if (Char::UTF2::eval q{ Char::UTF2::eval { if ('□●' !~ /[◆]/) { return 1 } else { return 0 } } }) {
    print qq{ok - 7 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 7 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has "..."
if (Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return "1" } else { return "0" } }) {
    print qq{ok - 8 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 8 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has qq{...}
if (Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return qq{1} } else { return qq{0} } }) {
    print qq{ok - 9 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 9 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has '...'
if (Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return '1' } else { return '0' } }) {
    print qq{ok - 10 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 10 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has q{...}
if (Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return q{1} } else { return q{0} } }) {
    print qq{ok - 11 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 11 $^X @{[__FILE__]}\n};
}

# Char::UTF2::eval q{...} has $var
my $var1 = 1;
my $var0 = 0;
if (Char::UTF2::eval q{ if ('□●' !~ /[◆]/) { return $var1 } else { return $var0 } }) {
    print qq{ok - 12 $^X @{[__FILE__]}\n};
}
else {
    print qq{not ok - 12 $^X @{[__FILE__]}\n};
}

__END__
