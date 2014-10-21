# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..30\n";

my $__FILE__ = __FILE__;

if (fc('ABCDEF') eq fc('abcdef')) {
    print qq{ok - 1 fc('ABCDEF') eq fc('abcdef')\n};
}
else {
    print qq{not ok - 1 fc('ABCDEF') eq fc('abcdef')\n};
}

if (fc('アイウエオ') eq 'アイウエオ') {
    print qq{ok - 2 fc('アイウエオ') eq 'アイウエオ'\n};
}
else {
    print qq{not ok - 2 fc('アイウエオ') eq 'アイウエオ'\n};
}

if ("\FABCDEF\E" eq "\Fabcdef\E") {
    print qq{ok - 3 "\\FABCDEF\\E" eq "\\Fabcdef\\E"\n};
}
else {
    print qq{not ok - 3 "\\FABCDEF\\E" eq "\\Fabcdef\\E"\n};
}

if ("\Fアイウエオ\E" eq "アイウエオ") {
    print qq{ok - 4 "\\Fアイウエオ\\E" eq "アイウエオ"\n};
}
else {
    print qq{not ok - 4 "\\Fアイウエオ\\E" eq "アイウエオ"\n};
}

if ("\FABCDEF\E" =~ /\Fabcdef\E/) {
    print qq{ok - 5 "\\FABCDEF\\E" =~ /\\Fabcdef\\E/\n};
}
else {
    print qq{not ok - 5 "\\FABCDEF\\E" =~ /\\Fabcdef\\E/\n};
}

if ("\Fabcdef\E" =~ /\FABCDEF\E/) {
    print qq{ok - 6 "\\Fabcdef\\E" =~ /\\FABCDEF\\E/\n};
}
else {
    print qq{not ok - 6 "\\Fabcdef\\E" =~ /\\FABCDEF\\E/\n};
}

if ("\FABCDEF\E" =~ /\Fabcdef\E/i) {
    print qq{ok - 7 "\\FABCDEF\\E" =~ /\\Fabcdef\\E/i\n};
}
else {
    print qq{not ok - 7 "\\FABCDEF\\E" =~ /\\Fabcdef\\E/i\n};
}

if ("\Fabcdef\E" =~ /\FABCDEF\E/i) {
    print qq{ok - 8 "\\Fabcdef\\E" =~ /\\FABCDEF\\E/i\n};
}
else {
    print qq{not ok - 8 "\\Fabcdef\\E" =~ /\\FABCDEF\\E/i\n};
}

my %fc = (
    "\x41"             => "\x61",                     # LATIN CAPITAL LETTER A
    "\x42"             => "\x62",                     # LATIN CAPITAL LETTER B
    "\x43"             => "\x63",                     # LATIN CAPITAL LETTER C
    "\x44"             => "\x64",                     # LATIN CAPITAL LETTER D
    "\x45"             => "\x65",                     # LATIN CAPITAL LETTER E
    "\x46"             => "\x66",                     # LATIN CAPITAL LETTER F
    "\x47"             => "\x67",                     # LATIN CAPITAL LETTER G
    "\x48"             => "\x68",                     # LATIN CAPITAL LETTER H
    "\x49"             => "\x69",                     # LATIN CAPITAL LETTER I
    "\x4A"             => "\x6A",                     # LATIN CAPITAL LETTER J
    "\x4B"             => "\x6B",                     # LATIN CAPITAL LETTER K
    "\x4C"             => "\x6C",                     # LATIN CAPITAL LETTER L
    "\x4D"             => "\x6D",                     # LATIN CAPITAL LETTER M
    "\x4E"             => "\x6E",                     # LATIN CAPITAL LETTER N
    "\x4F"             => "\x6F",                     # LATIN CAPITAL LETTER O
    "\x50"             => "\x70",                     # LATIN CAPITAL LETTER P
    "\x51"             => "\x71",                     # LATIN CAPITAL LETTER Q
    "\x52"             => "\x72",                     # LATIN CAPITAL LETTER R
    "\x53"             => "\x73",                     # LATIN CAPITAL LETTER S
    "\x54"             => "\x74",                     # LATIN CAPITAL LETTER T
    "\x55"             => "\x75",                     # LATIN CAPITAL LETTER U
    "\x56"             => "\x76",                     # LATIN CAPITAL LETTER V
    "\x57"             => "\x77",                     # LATIN CAPITAL LETTER W
    "\x58"             => "\x78",                     # LATIN CAPITAL LETTER X
    "\x59"             => "\x79",                     # LATIN CAPITAL LETTER Y
    "\x5A"             => "\x7A",                     # LATIN CAPITAL LETTER Z
    "\xC2\xB5"         => "\xCE\xBC",                 # MICRO SIGN
    "\xC3\x80"         => "\xC3\xA0",                 # LATIN CAPITAL LETTER A WITH GRAVE
    "\xC3\x81"         => "\xC3\xA1",                 # LATIN CAPITAL LETTER A WITH ACUTE
    "\xC3\x82"         => "\xC3\xA2",                 # LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    "\xC3\x83"         => "\xC3\xA3",                 # LATIN CAPITAL LETTER A WITH TILDE
    "\xC3\x84"         => "\xC3\xA4",                 # LATIN CAPITAL LETTER A WITH DIAERESIS
    "\xC3\x85"         => "\xC3\xA5",                 # LATIN CAPITAL LETTER A WITH RING ABOVE
    "\xC3\x86"         => "\xC3\xA6",                 # LATIN CAPITAL LETTER AE
    "\xC3\x87"         => "\xC3\xA7",                 # LATIN CAPITAL LETTER C WITH CEDILLA
    "\xC3\x88"         => "\xC3\xA8",                 # LATIN CAPITAL LETTER E WITH GRAVE
    "\xC3\x89"         => "\xC3\xA9",                 # LATIN CAPITAL LETTER E WITH ACUTE
    "\xC3\x8A"         => "\xC3\xAA",                 # LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    "\xC3\x8B"         => "\xC3\xAB",                 # LATIN CAPITAL LETTER E WITH DIAERESIS
    "\xC3\x8C"         => "\xC3\xAC",                 # LATIN CAPITAL LETTER I WITH GRAVE
    "\xC3\x8D"         => "\xC3\xAD",                 # LATIN CAPITAL LETTER I WITH ACUTE
    "\xC3\x8E"         => "\xC3\xAE",                 # LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    "\xC3\x8F"         => "\xC3\xAF",                 # LATIN CAPITAL LETTER I WITH DIAERESIS
    "\xC3\x90"         => "\xC3\xB0",                 # LATIN CAPITAL LETTER ETH
    "\xC3\x91"         => "\xC3\xB1",                 # LATIN CAPITAL LETTER N WITH TILDE
    "\xC3\x92"         => "\xC3\xB2",                 # LATIN CAPITAL LETTER O WITH GRAVE
    "\xC3\x93"         => "\xC3\xB3",                 # LATIN CAPITAL LETTER O WITH ACUTE
    "\xC3\x94"         => "\xC3\xB4",                 # LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    "\xC3\x95"         => "\xC3\xB5",                 # LATIN CAPITAL LETTER O WITH TILDE
    "\xC3\x96"         => "\xC3\xB6",                 # LATIN CAPITAL LETTER O WITH DIAERESIS
    "\xC3\x98"         => "\xC3\xB8",                 # LATIN CAPITAL LETTER O WITH STROKE
    "\xC3\x99"         => "\xC3\xB9",                 # LATIN CAPITAL LETTER U WITH GRAVE
    "\xC3\x9A"         => "\xC3\xBA",                 # LATIN CAPITAL LETTER U WITH ACUTE
    "\xC3\x9B"         => "\xC3\xBB",                 # LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    "\xC3\x9C"         => "\xC3\xBC",                 # LATIN CAPITAL LETTER U WITH DIAERESIS
    "\xC3\x9D"         => "\xC3\xBD",                 # LATIN CAPITAL LETTER Y WITH ACUTE
    "\xC3\x9E"         => "\xC3\xBE",                 # LATIN CAPITAL LETTER THORN
    "\xC3\x9F"         => "\x73\x73",                 # LATIN SMALL LETTER SHARP S
    "\xC4\x80"         => "\xC4\x81",                 # LATIN CAPITAL LETTER A WITH MACRON
    "\xC4\x82"         => "\xC4\x83",                 # LATIN CAPITAL LETTER A WITH BREVE
    "\xC4\x84"         => "\xC4\x85",                 # LATIN CAPITAL LETTER A WITH OGONEK
    "\xC4\x86"         => "\xC4\x87",                 # LATIN CAPITAL LETTER C WITH ACUTE
    "\xC4\x88"         => "\xC4\x89",                 # LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    "\xC4\x8A"         => "\xC4\x8B",                 # LATIN CAPITAL LETTER C WITH DOT ABOVE
    "\xC4\x8C"         => "\xC4\x8D",                 # LATIN CAPITAL LETTER C WITH CARON
    "\xC4\x8E"         => "\xC4\x8F",                 # LATIN CAPITAL LETTER D WITH CARON
    "\xC4\x90"         => "\xC4\x91",                 # LATIN CAPITAL LETTER D WITH STROKE
    "\xC4\x92"         => "\xC4\x93",                 # LATIN CAPITAL LETTER E WITH MACRON
    "\xC4\x94"         => "\xC4\x95",                 # LATIN CAPITAL LETTER E WITH BREVE
    "\xC4\x96"         => "\xC4\x97",                 # LATIN CAPITAL LETTER E WITH DOT ABOVE
    "\xC4\x98"         => "\xC4\x99",                 # LATIN CAPITAL LETTER E WITH OGONEK
    "\xC4\x9A"         => "\xC4\x9B",                 # LATIN CAPITAL LETTER E WITH CARON
    "\xC4\x9C"         => "\xC4\x9D",                 # LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    "\xC4\x9E"         => "\xC4\x9F",                 # LATIN CAPITAL LETTER G WITH BREVE
    "\xC4\xA0"         => "\xC4\xA1",                 # LATIN CAPITAL LETTER G WITH DOT ABOVE
    "\xC4\xA2"         => "\xC4\xA3",                 # LATIN CAPITAL LETTER G WITH CEDILLA
    "\xC4\xA4"         => "\xC4\xA5",                 # LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    "\xC4\xA6"         => "\xC4\xA7",                 # LATIN CAPITAL LETTER H WITH STROKE
    "\xC4\xA8"         => "\xC4\xA9",                 # LATIN CAPITAL LETTER I WITH TILDE
    "\xC4\xAA"         => "\xC4\xAB",                 # LATIN CAPITAL LETTER I WITH MACRON
    "\xC4\xAC"         => "\xC4\xAD",                 # LATIN CAPITAL LETTER I WITH BREVE
    "\xC4\xAE"         => "\xC4\xAF",                 # LATIN CAPITAL LETTER I WITH OGONEK
    "\xC4\xB0"         => "\x69\xCC\x87",             # LATIN CAPITAL LETTER I WITH DOT ABOVE
    "\xC4\xB2"         => "\xC4\xB3",                 # LATIN CAPITAL LIGATURE IJ
    "\xC4\xB4"         => "\xC4\xB5",                 # LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    "\xC4\xB6"         => "\xC4\xB7",                 # LATIN CAPITAL LETTER K WITH CEDILLA
    "\xC4\xB9"         => "\xC4\xBA",                 # LATIN CAPITAL LETTER L WITH ACUTE
    "\xC4\xBB"         => "\xC4\xBC",                 # LATIN CAPITAL LETTER L WITH CEDILLA
    "\xC4\xBD"         => "\xC4\xBE",                 # LATIN CAPITAL LETTER L WITH CARON
    "\xC4\xBF"         => "\xC5\x80",                 # LATIN CAPITAL LETTER L WITH MIDDLE DOT
    "\xC5\x81"         => "\xC5\x82",                 # LATIN CAPITAL LETTER L WITH STROKE
    "\xC5\x83"         => "\xC5\x84",                 # LATIN CAPITAL LETTER N WITH ACUTE
    "\xC5\x85"         => "\xC5\x86",                 # LATIN CAPITAL LETTER N WITH CEDILLA
    "\xC5\x87"         => "\xC5\x88",                 # LATIN CAPITAL LETTER N WITH CARON
    "\xC5\x89"         => "\xCA\xBC\x6E",             # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    "\xC5\x8A"         => "\xC5\x8B",                 # LATIN CAPITAL LETTER ENG
    "\xC5\x8C"         => "\xC5\x8D",                 # LATIN CAPITAL LETTER O WITH MACRON
    "\xC5\x8E"         => "\xC5\x8F",                 # LATIN CAPITAL LETTER O WITH BREVE
    "\xC5\x90"         => "\xC5\x91",                 # LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    "\xC5\x92"         => "\xC5\x93",                 # LATIN CAPITAL LIGATURE OE
    "\xC5\x94"         => "\xC5\x95",                 # LATIN CAPITAL LETTER R WITH ACUTE
    "\xC5\x96"         => "\xC5\x97",                 # LATIN CAPITAL LETTER R WITH CEDILLA
    "\xC5\x98"         => "\xC5\x99",                 # LATIN CAPITAL LETTER R WITH CARON
    "\xC5\x9A"         => "\xC5\x9B",                 # LATIN CAPITAL LETTER S WITH ACUTE
    "\xC5\x9C"         => "\xC5\x9D",                 # LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    "\xC5\x9E"         => "\xC5\x9F",                 # LATIN CAPITAL LETTER S WITH CEDILLA
    "\xC5\xA0"         => "\xC5\xA1",                 # LATIN CAPITAL LETTER S WITH CARON
    "\xC5\xA2"         => "\xC5\xA3",                 # LATIN CAPITAL LETTER T WITH CEDILLA
    "\xC5\xA4"         => "\xC5\xA5",                 # LATIN CAPITAL LETTER T WITH CARON
    "\xC5\xA6"         => "\xC5\xA7",                 # LATIN CAPITAL LETTER T WITH STROKE
    "\xC5\xA8"         => "\xC5\xA9",                 # LATIN CAPITAL LETTER U WITH TILDE
    "\xC5\xAA"         => "\xC5\xAB",                 # LATIN CAPITAL LETTER U WITH MACRON
    "\xC5\xAC"         => "\xC5\xAD",                 # LATIN CAPITAL LETTER U WITH BREVE
    "\xC5\xAE"         => "\xC5\xAF",                 # LATIN CAPITAL LETTER U WITH RING ABOVE
    "\xC5\xB0"         => "\xC5\xB1",                 # LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    "\xC5\xB2"         => "\xC5\xB3",                 # LATIN CAPITAL LETTER U WITH OGONEK
    "\xC5\xB4"         => "\xC5\xB5",                 # LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    "\xC5\xB6"         => "\xC5\xB7",                 # LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    "\xC5\xB8"         => "\xC3\xBF",                 # LATIN CAPITAL LETTER Y WITH DIAERESIS
    "\xC5\xB9"         => "\xC5\xBA",                 # LATIN CAPITAL LETTER Z WITH ACUTE
    "\xC5\xBB"         => "\xC5\xBC",                 # LATIN CAPITAL LETTER Z WITH DOT ABOVE
    "\xC5\xBD"         => "\xC5\xBE",                 # LATIN CAPITAL LETTER Z WITH CARON
    "\xC5\xBF"         => "\x73",                     # LATIN SMALL LETTER LONG S
    "\xC6\x81"         => "\xC9\x93",                 # LATIN CAPITAL LETTER B WITH HOOK
    "\xC6\x82"         => "\xC6\x83",                 # LATIN CAPITAL LETTER B WITH TOPBAR
    "\xC6\x84"         => "\xC6\x85",                 # LATIN CAPITAL LETTER TONE SIX
    "\xC6\x86"         => "\xC9\x94",                 # LATIN CAPITAL LETTER OPEN O
    "\xC6\x87"         => "\xC6\x88",                 # LATIN CAPITAL LETTER C WITH HOOK
    "\xC6\x89"         => "\xC9\x96",                 # LATIN CAPITAL LETTER AFRICAN D
    "\xC6\x8A"         => "\xC9\x97",                 # LATIN CAPITAL LETTER D WITH HOOK
    "\xC6\x8B"         => "\xC6\x8C",                 # LATIN CAPITAL LETTER D WITH TOPBAR
    "\xC6\x8E"         => "\xC7\x9D",                 # LATIN CAPITAL LETTER REVERSED E
    "\xC6\x8F"         => "\xC9\x99",                 # LATIN CAPITAL LETTER SCHWA
    "\xC6\x90"         => "\xC9\x9B",                 # LATIN CAPITAL LETTER OPEN E
    "\xC6\x91"         => "\xC6\x92",                 # LATIN CAPITAL LETTER F WITH HOOK
    "\xC6\x93"         => "\xC9\xA0",                 # LATIN CAPITAL LETTER G WITH HOOK
    "\xC6\x94"         => "\xC9\xA3",                 # LATIN CAPITAL LETTER GAMMA
    "\xC6\x96"         => "\xC9\xA9",                 # LATIN CAPITAL LETTER IOTA
    "\xC6\x97"         => "\xC9\xA8",                 # LATIN CAPITAL LETTER I WITH STROKE
    "\xC6\x98"         => "\xC6\x99",                 # LATIN CAPITAL LETTER K WITH HOOK
    "\xC6\x9C"         => "\xC9\xAF",                 # LATIN CAPITAL LETTER TURNED M
    "\xC6\x9D"         => "\xC9\xB2",                 # LATIN CAPITAL LETTER N WITH LEFT HOOK
    "\xC6\x9F"         => "\xC9\xB5",                 # LATIN CAPITAL LETTER O WITH MIDDLE TILDE
    "\xC6\xA0"         => "\xC6\xA1",                 # LATIN CAPITAL LETTER O WITH HORN
    "\xC6\xA2"         => "\xC6\xA3",                 # LATIN CAPITAL LETTER OI
    "\xC6\xA4"         => "\xC6\xA5",                 # LATIN CAPITAL LETTER P WITH HOOK
    "\xC6\xA6"         => "\xCA\x80",                 # LATIN LETTER YR
    "\xC6\xA7"         => "\xC6\xA8",                 # LATIN CAPITAL LETTER TONE TWO
    "\xC6\xA9"         => "\xCA\x83",                 # LATIN CAPITAL LETTER ESH
    "\xC6\xAC"         => "\xC6\xAD",                 # LATIN CAPITAL LETTER T WITH HOOK
    "\xC6\xAE"         => "\xCA\x88",                 # LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
    "\xC6\xAF"         => "\xC6\xB0",                 # LATIN CAPITAL LETTER U WITH HORN
    "\xC6\xB1"         => "\xCA\x8A",                 # LATIN CAPITAL LETTER UPSILON
    "\xC6\xB2"         => "\xCA\x8B",                 # LATIN CAPITAL LETTER V WITH HOOK
    "\xC6\xB3"         => "\xC6\xB4",                 # LATIN CAPITAL LETTER Y WITH HOOK
    "\xC6\xB5"         => "\xC6\xB6",                 # LATIN CAPITAL LETTER Z WITH STROKE
    "\xC6\xB7"         => "\xCA\x92",                 # LATIN CAPITAL LETTER EZH
    "\xC6\xB8"         => "\xC6\xB9",                 # LATIN CAPITAL LETTER EZH REVERSED
    "\xC6\xBC"         => "\xC6\xBD",                 # LATIN CAPITAL LETTER TONE FIVE
    "\xC7\x84"         => "\xC7\x86",                 # LATIN CAPITAL LETTER DZ WITH CARON
    "\xC7\x85"         => "\xC7\x86",                 # LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    "\xC7\x87"         => "\xC7\x89",                 # LATIN CAPITAL LETTER LJ
    "\xC7\x88"         => "\xC7\x89",                 # LATIN CAPITAL LETTER L WITH SMALL LETTER J
    "\xC7\x8A"         => "\xC7\x8C",                 # LATIN CAPITAL LETTER NJ
    "\xC7\x8B"         => "\xC7\x8C",                 # LATIN CAPITAL LETTER N WITH SMALL LETTER J
    "\xC7\x8D"         => "\xC7\x8E",                 # LATIN CAPITAL LETTER A WITH CARON
    "\xC7\x8F"         => "\xC7\x90",                 # LATIN CAPITAL LETTER I WITH CARON
    "\xC7\x91"         => "\xC7\x92",                 # LATIN CAPITAL LETTER O WITH CARON
    "\xC7\x93"         => "\xC7\x94",                 # LATIN CAPITAL LETTER U WITH CARON
    "\xC7\x95"         => "\xC7\x96",                 # LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    "\xC7\x97"         => "\xC7\x98",                 # LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    "\xC7\x99"         => "\xC7\x9A",                 # LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    "\xC7\x9B"         => "\xC7\x9C",                 # LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    "\xC7\x9E"         => "\xC7\x9F",                 # LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    "\xC7\xA0"         => "\xC7\xA1",                 # LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    "\xC7\xA2"         => "\xC7\xA3",                 # LATIN CAPITAL LETTER AE WITH MACRON
    "\xC7\xA4"         => "\xC7\xA5",                 # LATIN CAPITAL LETTER G WITH STROKE
    "\xC7\xA6"         => "\xC7\xA7",                 # LATIN CAPITAL LETTER G WITH CARON
    "\xC7\xA8"         => "\xC7\xA9",                 # LATIN CAPITAL LETTER K WITH CARON
    "\xC7\xAA"         => "\xC7\xAB",                 # LATIN CAPITAL LETTER O WITH OGONEK
    "\xC7\xAC"         => "\xC7\xAD",                 # LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    "\xC7\xAE"         => "\xC7\xAF",                 # LATIN CAPITAL LETTER EZH WITH CARON
    "\xC7\xB0"         => "\x6A\xCC\x8C",             # LATIN SMALL LETTER J WITH CARON
    "\xC7\xB1"         => "\xC7\xB3",                 # LATIN CAPITAL LETTER DZ
    "\xC7\xB2"         => "\xC7\xB3",                 # LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    "\xC7\xB4"         => "\xC7\xB5",                 # LATIN CAPITAL LETTER G WITH ACUTE
    "\xC7\xB6"         => "\xC6\x95",                 # LATIN CAPITAL LETTER HWAIR
    "\xC7\xB7"         => "\xC6\xBF",                 # LATIN CAPITAL LETTER WYNN
    "\xC7\xB8"         => "\xC7\xB9",                 # LATIN CAPITAL LETTER N WITH GRAVE
    "\xC7\xBA"         => "\xC7\xBB",                 # LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    "\xC7\xBC"         => "\xC7\xBD",                 # LATIN CAPITAL LETTER AE WITH ACUTE
    "\xC7\xBE"         => "\xC7\xBF",                 # LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    "\xC8\x80"         => "\xC8\x81",                 # LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    "\xC8\x82"         => "\xC8\x83",                 # LATIN CAPITAL LETTER A WITH INVERTED BREVE
    "\xC8\x84"         => "\xC8\x85",                 # LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    "\xC8\x86"         => "\xC8\x87",                 # LATIN CAPITAL LETTER E WITH INVERTED BREVE
    "\xC8\x88"         => "\xC8\x89",                 # LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    "\xC8\x8A"         => "\xC8\x8B",                 # LATIN CAPITAL LETTER I WITH INVERTED BREVE
    "\xC8\x8C"         => "\xC8\x8D",                 # LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    "\xC8\x8E"         => "\xC8\x8F",                 # LATIN CAPITAL LETTER O WITH INVERTED BREVE
    "\xC8\x90"         => "\xC8\x91",                 # LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    "\xC8\x92"         => "\xC8\x93",                 # LATIN CAPITAL LETTER R WITH INVERTED BREVE
    "\xC8\x94"         => "\xC8\x95",                 # LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    "\xC8\x96"         => "\xC8\x97",                 # LATIN CAPITAL LETTER U WITH INVERTED BREVE
    "\xC8\x98"         => "\xC8\x99",                 # LATIN CAPITAL LETTER S WITH COMMA BELOW
    "\xC8\x9A"         => "\xC8\x9B",                 # LATIN CAPITAL LETTER T WITH COMMA BELOW
    "\xC8\x9C"         => "\xC8\x9D",                 # LATIN CAPITAL LETTER YOGH
    "\xC8\x9E"         => "\xC8\x9F",                 # LATIN CAPITAL LETTER H WITH CARON
    "\xC8\xA0"         => "\xC6\x9E",                 # LATIN CAPITAL LETTER N WITH LONG RIGHT LEG
    "\xC8\xA2"         => "\xC8\xA3",                 # LATIN CAPITAL LETTER OU
    "\xC8\xA4"         => "\xC8\xA5",                 # LATIN CAPITAL LETTER Z WITH HOOK
    "\xC8\xA6"         => "\xC8\xA7",                 # LATIN CAPITAL LETTER A WITH DOT ABOVE
    "\xC8\xA8"         => "\xC8\xA9",                 # LATIN CAPITAL LETTER E WITH CEDILLA
    "\xC8\xAA"         => "\xC8\xAB",                 # LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    "\xC8\xAC"         => "\xC8\xAD",                 # LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    "\xC8\xAE"         => "\xC8\xAF",                 # LATIN CAPITAL LETTER O WITH DOT ABOVE
    "\xC8\xB0"         => "\xC8\xB1",                 # LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    "\xC8\xB2"         => "\xC8\xB3",                 # LATIN CAPITAL LETTER Y WITH MACRON
    "\xC8\xBA"         => "\xE2\xB1\xA5",             # LATIN CAPITAL LETTER A WITH STROKE
    "\xC8\xBB"         => "\xC8\xBC",                 # LATIN CAPITAL LETTER C WITH STROKE
    "\xC8\xBD"         => "\xC6\x9A",                 # LATIN CAPITAL LETTER L WITH BAR
    "\xC8\xBE"         => "\xE2\xB1\xA6",             # LATIN CAPITAL LETTER T WITH DIAGONAL STROKE
    "\xC9\x81"         => "\xC9\x82",                 # LATIN CAPITAL LETTER GLOTTAL STOP
    "\xC9\x83"         => "\xC6\x80",                 # LATIN CAPITAL LETTER B WITH STROKE
    "\xC9\x84"         => "\xCA\x89",                 # LATIN CAPITAL LETTER U BAR
    "\xC9\x85"         => "\xCA\x8C",                 # LATIN CAPITAL LETTER TURNED V
    "\xC9\x86"         => "\xC9\x87",                 # LATIN CAPITAL LETTER E WITH STROKE
    "\xC9\x88"         => "\xC9\x89",                 # LATIN CAPITAL LETTER J WITH STROKE
    "\xC9\x8A"         => "\xC9\x8B",                 # LATIN CAPITAL LETTER SMALL Q WITH HOOK TAIL
    "\xC9\x8C"         => "\xC9\x8D",                 # LATIN CAPITAL LETTER R WITH STROKE
    "\xC9\x8E"         => "\xC9\x8F",                 # LATIN CAPITAL LETTER Y WITH STROKE
    "\xCD\x85"         => "\xCE\xB9",                 # COMBINING GREEK YPOGEGRAMMENI
    "\xCD\xB0"         => "\xCD\xB1",                 # GREEK CAPITAL LETTER HETA
    "\xCD\xB2"         => "\xCD\xB3",                 # GREEK CAPITAL LETTER ARCHAIC SAMPI
    "\xCD\xB6"         => "\xCD\xB7",                 # GREEK CAPITAL LETTER PAMPHYLIAN DIGAMMA
    "\xCE\x86"         => "\xCE\xAC",                 # GREEK CAPITAL LETTER ALPHA WITH TONOS
    "\xCE\x88"         => "\xCE\xAD",                 # GREEK CAPITAL LETTER EPSILON WITH TONOS
    "\xCE\x89"         => "\xCE\xAE",                 # GREEK CAPITAL LETTER ETA WITH TONOS
    "\xCE\x8A"         => "\xCE\xAF",                 # GREEK CAPITAL LETTER IOTA WITH TONOS
    "\xCE\x8C"         => "\xCF\x8C",                 # GREEK CAPITAL LETTER OMICRON WITH TONOS
    "\xCE\x8E"         => "\xCF\x8D",                 # GREEK CAPITAL LETTER UPSILON WITH TONOS
    "\xCE\x8F"         => "\xCF\x8E",                 # GREEK CAPITAL LETTER OMEGA WITH TONOS
    "\xCE\x90"         => "\xCE\xB9\xCC\x88\xCC\x81", # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    "\xCE\x91"         => "\xCE\xB1",                 # GREEK CAPITAL LETTER ALPHA
    "\xCE\x92"         => "\xCE\xB2",                 # GREEK CAPITAL LETTER BETA
    "\xCE\x93"         => "\xCE\xB3",                 # GREEK CAPITAL LETTER GAMMA
    "\xCE\x94"         => "\xCE\xB4",                 # GREEK CAPITAL LETTER DELTA
    "\xCE\x95"         => "\xCE\xB5",                 # GREEK CAPITAL LETTER EPSILON
    "\xCE\x96"         => "\xCE\xB6",                 # GREEK CAPITAL LETTER ZETA
    "\xCE\x97"         => "\xCE\xB7",                 # GREEK CAPITAL LETTER ETA
    "\xCE\x98"         => "\xCE\xB8",                 # GREEK CAPITAL LETTER THETA
    "\xCE\x99"         => "\xCE\xB9",                 # GREEK CAPITAL LETTER IOTA
    "\xCE\x9A"         => "\xCE\xBA",                 # GREEK CAPITAL LETTER KAPPA
    "\xCE\x9B"         => "\xCE\xBB",                 # GREEK CAPITAL LETTER LAMDA
    "\xCE\x9C"         => "\xCE\xBC",                 # GREEK CAPITAL LETTER MU
    "\xCE\x9D"         => "\xCE\xBD",                 # GREEK CAPITAL LETTER NU
    "\xCE\x9E"         => "\xCE\xBE",                 # GREEK CAPITAL LETTER XI
    "\xCE\x9F"         => "\xCE\xBF",                 # GREEK CAPITAL LETTER OMICRON
    "\xCE\xA0"         => "\xCF\x80",                 # GREEK CAPITAL LETTER PI
    "\xCE\xA1"         => "\xCF\x81",                 # GREEK CAPITAL LETTER RHO
    "\xCE\xA3"         => "\xCF\x83",                 # GREEK CAPITAL LETTER SIGMA
    "\xCE\xA4"         => "\xCF\x84",                 # GREEK CAPITAL LETTER TAU
    "\xCE\xA5"         => "\xCF\x85",                 # GREEK CAPITAL LETTER UPSILON
    "\xCE\xA6"         => "\xCF\x86",                 # GREEK CAPITAL LETTER PHI
    "\xCE\xA7"         => "\xCF\x87",                 # GREEK CAPITAL LETTER CHI
    "\xCE\xA8"         => "\xCF\x88",                 # GREEK CAPITAL LETTER PSI
    "\xCE\xA9"         => "\xCF\x89",                 # GREEK CAPITAL LETTER OMEGA
    "\xCE\xAA"         => "\xCF\x8A",                 # GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    "\xCE\xAB"         => "\xCF\x8B",                 # GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    "\xCE\xB0"         => "\xCF\x85\xCC\x88\xCC\x81", # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    "\xCF\x82"         => "\xCF\x83",                 # GREEK SMALL LETTER FINAL SIGMA
    "\xCF\x8F"         => "\xCF\x97",                 # GREEK CAPITAL KAI SYMBOL
    "\xCF\x90"         => "\xCE\xB2",                 # GREEK BETA SYMBOL
    "\xCF\x91"         => "\xCE\xB8",                 # GREEK THETA SYMBOL
    "\xCF\x95"         => "\xCF\x86",                 # GREEK PHI SYMBOL
    "\xCF\x96"         => "\xCF\x80",                 # GREEK PI SYMBOL
    "\xCF\x98"         => "\xCF\x99",                 # GREEK LETTER ARCHAIC KOPPA
    "\xCF\x9A"         => "\xCF\x9B",                 # GREEK LETTER STIGMA
    "\xCF\x9C"         => "\xCF\x9D",                 # GREEK LETTER DIGAMMA
    "\xCF\x9E"         => "\xCF\x9F",                 # GREEK LETTER KOPPA
    "\xCF\xA0"         => "\xCF\xA1",                 # GREEK LETTER SAMPI
    "\xCF\xA2"         => "\xCF\xA3",                 # COPTIC CAPITAL LETTER SHEI
    "\xCF\xA4"         => "\xCF\xA5",                 # COPTIC CAPITAL LETTER FEI
    "\xCF\xA6"         => "\xCF\xA7",                 # COPTIC CAPITAL LETTER KHEI
    "\xCF\xA8"         => "\xCF\xA9",                 # COPTIC CAPITAL LETTER HORI
    "\xCF\xAA"         => "\xCF\xAB",                 # COPTIC CAPITAL LETTER GANGIA
    "\xCF\xAC"         => "\xCF\xAD",                 # COPTIC CAPITAL LETTER SHIMA
    "\xCF\xAE"         => "\xCF\xAF",                 # COPTIC CAPITAL LETTER DEI
    "\xCF\xB0"         => "\xCE\xBA",                 # GREEK KAPPA SYMBOL
    "\xCF\xB1"         => "\xCF\x81",                 # GREEK RHO SYMBOL
    "\xCF\xB4"         => "\xCE\xB8",                 # GREEK CAPITAL THETA SYMBOL
    "\xCF\xB5"         => "\xCE\xB5",                 # GREEK LUNATE EPSILON SYMBOL
    "\xCF\xB7"         => "\xCF\xB8",                 # GREEK CAPITAL LETTER SHO
    "\xCF\xB9"         => "\xCF\xB2",                 # GREEK CAPITAL LUNATE SIGMA SYMBOL
    "\xCF\xBA"         => "\xCF\xBB",                 # GREEK CAPITAL LETTER SAN
    "\xCF\xBD"         => "\xCD\xBB",                 # GREEK CAPITAL REVERSED LUNATE SIGMA SYMBOL
    "\xCF\xBE"         => "\xCD\xBC",                 # GREEK CAPITAL DOTTED LUNATE SIGMA SYMBOL
    "\xCF\xBF"         => "\xCD\xBD",                 # GREEK CAPITAL REVERSED DOTTED LUNATE SIGMA SYMBOL
    "\xD0\x80"         => "\xD1\x90",                 # CYRILLIC CAPITAL LETTER IE WITH GRAVE
    "\xD0\x81"         => "\xD1\x91",                 # CYRILLIC CAPITAL LETTER IO
    "\xD0\x82"         => "\xD1\x92",                 # CYRILLIC CAPITAL LETTER DJE
    "\xD0\x83"         => "\xD1\x93",                 # CYRILLIC CAPITAL LETTER GJE
    "\xD0\x84"         => "\xD1\x94",                 # CYRILLIC CAPITAL LETTER UKRAINIAN IE
    "\xD0\x85"         => "\xD1\x95",                 # CYRILLIC CAPITAL LETTER DZE
    "\xD0\x86"         => "\xD1\x96",                 # CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    "\xD0\x87"         => "\xD1\x97",                 # CYRILLIC CAPITAL LETTER YI
    "\xD0\x88"         => "\xD1\x98",                 # CYRILLIC CAPITAL LETTER JE
    "\xD0\x89"         => "\xD1\x99",                 # CYRILLIC CAPITAL LETTER LJE
    "\xD0\x8A"         => "\xD1\x9A",                 # CYRILLIC CAPITAL LETTER NJE
    "\xD0\x8B"         => "\xD1\x9B",                 # CYRILLIC CAPITAL LETTER TSHE
    "\xD0\x8C"         => "\xD1\x9C",                 # CYRILLIC CAPITAL LETTER KJE
    "\xD0\x8D"         => "\xD1\x9D",                 # CYRILLIC CAPITAL LETTER I WITH GRAVE
    "\xD0\x8E"         => "\xD1\x9E",                 # CYRILLIC CAPITAL LETTER SHORT U
    "\xD0\x8F"         => "\xD1\x9F",                 # CYRILLIC CAPITAL LETTER DZHE
    "\xD0\x90"         => "\xD0\xB0",                 # CYRILLIC CAPITAL LETTER A
    "\xD0\x91"         => "\xD0\xB1",                 # CYRILLIC CAPITAL LETTER BE
    "\xD0\x92"         => "\xD0\xB2",                 # CYRILLIC CAPITAL LETTER VE
    "\xD0\x93"         => "\xD0\xB3",                 # CYRILLIC CAPITAL LETTER GHE
    "\xD0\x94"         => "\xD0\xB4",                 # CYRILLIC CAPITAL LETTER DE
    "\xD0\x95"         => "\xD0\xB5",                 # CYRILLIC CAPITAL LETTER IE
    "\xD0\x96"         => "\xD0\xB6",                 # CYRILLIC CAPITAL LETTER ZHE
    "\xD0\x97"         => "\xD0\xB7",                 # CYRILLIC CAPITAL LETTER ZE
    "\xD0\x98"         => "\xD0\xB8",                 # CYRILLIC CAPITAL LETTER I
    "\xD0\x99"         => "\xD0\xB9",                 # CYRILLIC CAPITAL LETTER SHORT I
    "\xD0\x9A"         => "\xD0\xBA",                 # CYRILLIC CAPITAL LETTER KA
    "\xD0\x9B"         => "\xD0\xBB",                 # CYRILLIC CAPITAL LETTER EL
    "\xD0\x9C"         => "\xD0\xBC",                 # CYRILLIC CAPITAL LETTER EM
    "\xD0\x9D"         => "\xD0\xBD",                 # CYRILLIC CAPITAL LETTER EN
    "\xD0\x9E"         => "\xD0\xBE",                 # CYRILLIC CAPITAL LETTER O
    "\xD0\x9F"         => "\xD0\xBF",                 # CYRILLIC CAPITAL LETTER PE
    "\xD0\xA0"         => "\xD1\x80",                 # CYRILLIC CAPITAL LETTER ER
    "\xD0\xA1"         => "\xD1\x81",                 # CYRILLIC CAPITAL LETTER ES
    "\xD0\xA2"         => "\xD1\x82",                 # CYRILLIC CAPITAL LETTER TE
    "\xD0\xA3"         => "\xD1\x83",                 # CYRILLIC CAPITAL LETTER U
    "\xD0\xA4"         => "\xD1\x84",                 # CYRILLIC CAPITAL LETTER EF
    "\xD0\xA5"         => "\xD1\x85",                 # CYRILLIC CAPITAL LETTER HA
    "\xD0\xA6"         => "\xD1\x86",                 # CYRILLIC CAPITAL LETTER TSE
    "\xD0\xA7"         => "\xD1\x87",                 # CYRILLIC CAPITAL LETTER CHE
    "\xD0\xA8"         => "\xD1\x88",                 # CYRILLIC CAPITAL LETTER SHA
    "\xD0\xA9"         => "\xD1\x89",                 # CYRILLIC CAPITAL LETTER SHCHA
    "\xD0\xAA"         => "\xD1\x8A",                 # CYRILLIC CAPITAL LETTER HARD SIGN
    "\xD0\xAB"         => "\xD1\x8B",                 # CYRILLIC CAPITAL LETTER YERU
    "\xD0\xAC"         => "\xD1\x8C",                 # CYRILLIC CAPITAL LETTER SOFT SIGN
    "\xD0\xAD"         => "\xD1\x8D",                 # CYRILLIC CAPITAL LETTER E
    "\xD0\xAE"         => "\xD1\x8E",                 # CYRILLIC CAPITAL LETTER YU
    "\xD0\xAF"         => "\xD1\x8F",                 # CYRILLIC CAPITAL LETTER YA
    "\xD1\xA0"         => "\xD1\xA1",                 # CYRILLIC CAPITAL LETTER OMEGA
    "\xD1\xA2"         => "\xD1\xA3",                 # CYRILLIC CAPITAL LETTER YAT
    "\xD1\xA4"         => "\xD1\xA5",                 # CYRILLIC CAPITAL LETTER IOTIFIED E
    "\xD1\xA6"         => "\xD1\xA7",                 # CYRILLIC CAPITAL LETTER LITTLE YUS
    "\xD1\xA8"         => "\xD1\xA9",                 # CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
    "\xD1\xAA"         => "\xD1\xAB",                 # CYRILLIC CAPITAL LETTER BIG YUS
    "\xD1\xAC"         => "\xD1\xAD",                 # CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
    "\xD1\xAE"         => "\xD1\xAF",                 # CYRILLIC CAPITAL LETTER KSI
    "\xD1\xB0"         => "\xD1\xB1",                 # CYRILLIC CAPITAL LETTER PSI
    "\xD1\xB2"         => "\xD1\xB3",                 # CYRILLIC CAPITAL LETTER FITA
    "\xD1\xB4"         => "\xD1\xB5",                 # CYRILLIC CAPITAL LETTER IZHITSA
    "\xD1\xB6"         => "\xD1\xB7",                 # CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    "\xD1\xB8"         => "\xD1\xB9",                 # CYRILLIC CAPITAL LETTER UK
    "\xD1\xBA"         => "\xD1\xBB",                 # CYRILLIC CAPITAL LETTER ROUND OMEGA
    "\xD1\xBC"         => "\xD1\xBD",                 # CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
    "\xD1\xBE"         => "\xD1\xBF",                 # CYRILLIC CAPITAL LETTER OT
    "\xD2\x80"         => "\xD2\x81",                 # CYRILLIC CAPITAL LETTER KOPPA
    "\xD2\x8A"         => "\xD2\x8B",                 # CYRILLIC CAPITAL LETTER SHORT I WITH TAIL
    "\xD2\x8C"         => "\xD2\x8D",                 # CYRILLIC CAPITAL LETTER SEMISOFT SIGN
    "\xD2\x8E"         => "\xD2\x8F",                 # CYRILLIC CAPITAL LETTER ER WITH TICK
    "\xD2\x90"         => "\xD2\x91",                 # CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    "\xD2\x92"         => "\xD2\x93",                 # CYRILLIC CAPITAL LETTER GHE WITH STROKE
    "\xD2\x94"         => "\xD2\x95",                 # CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
    "\xD2\x96"         => "\xD2\x97",                 # CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    "\xD2\x98"         => "\xD2\x99",                 # CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
    "\xD2\x9A"         => "\xD2\x9B",                 # CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    "\xD2\x9C"         => "\xD2\x9D",                 # CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    "\xD2\x9E"         => "\xD2\x9F",                 # CYRILLIC CAPITAL LETTER KA WITH STROKE
    "\xD2\xA0"         => "\xD2\xA1",                 # CYRILLIC CAPITAL LETTER BASHKIR KA
    "\xD2\xA2"         => "\xD2\xA3",                 # CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    "\xD2\xA4"         => "\xD2\xA5",                 # CYRILLIC CAPITAL LIGATURE EN GHE
    "\xD2\xA6"         => "\xD2\xA7",                 # CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
    "\xD2\xA8"         => "\xD2\xA9",                 # CYRILLIC CAPITAL LETTER ABKHASIAN HA
    "\xD2\xAA"         => "\xD2\xAB",                 # CYRILLIC CAPITAL LETTER ES WITH DESCENDER
    "\xD2\xAC"         => "\xD2\xAD",                 # CYRILLIC CAPITAL LETTER TE WITH DESCENDER
    "\xD2\xAE"         => "\xD2\xAF",                 # CYRILLIC CAPITAL LETTER STRAIGHT U
    "\xD2\xB0"         => "\xD2\xB1",                 # CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    "\xD2\xB2"         => "\xD2\xB3",                 # CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    "\xD2\xB4"         => "\xD2\xB5",                 # CYRILLIC CAPITAL LIGATURE TE TSE
    "\xD2\xB6"         => "\xD2\xB7",                 # CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    "\xD2\xB8"         => "\xD2\xB9",                 # CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    "\xD2\xBA"         => "\xD2\xBB",                 # CYRILLIC CAPITAL LETTER SHHA
    "\xD2\xBC"         => "\xD2\xBD",                 # CYRILLIC CAPITAL LETTER ABKHASIAN CHE
    "\xD2\xBE"         => "\xD2\xBF",                 # CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
    "\xD3\x80"         => "\xD3\x8F",                 # CYRILLIC LETTER PALOCHKA
    "\xD3\x81"         => "\xD3\x82",                 # CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    "\xD3\x83"         => "\xD3\x84",                 # CYRILLIC CAPITAL LETTER KA WITH HOOK
    "\xD3\x85"         => "\xD3\x86",                 # CYRILLIC CAPITAL LETTER EL WITH TAIL
    "\xD3\x87"         => "\xD3\x88",                 # CYRILLIC CAPITAL LETTER EN WITH HOOK
    "\xD3\x89"         => "\xD3\x8A",                 # CYRILLIC CAPITAL LETTER EN WITH TAIL
    "\xD3\x8B"         => "\xD3\x8C",                 # CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
    "\xD3\x8D"         => "\xD3\x8E",                 # CYRILLIC CAPITAL LETTER EM WITH TAIL
    "\xD3\x90"         => "\xD3\x91",                 # CYRILLIC CAPITAL LETTER A WITH BREVE
    "\xD3\x92"         => "\xD3\x93",                 # CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    "\xD3\x94"         => "\xD3\x95",                 # CYRILLIC CAPITAL LIGATURE A IE
    "\xD3\x96"         => "\xD3\x97",                 # CYRILLIC CAPITAL LETTER IE WITH BREVE
    "\xD3\x98"         => "\xD3\x99",                 # CYRILLIC CAPITAL LETTER SCHWA
    "\xD3\x9A"         => "\xD3\x9B",                 # CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    "\xD3\x9C"         => "\xD3\x9D",                 # CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    "\xD3\x9E"         => "\xD3\x9F",                 # CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    "\xD3\xA0"         => "\xD3\xA1",                 # CYRILLIC CAPITAL LETTER ABKHASIAN DZE
    "\xD3\xA2"         => "\xD3\xA3",                 # CYRILLIC CAPITAL LETTER I WITH MACRON
    "\xD3\xA4"         => "\xD3\xA5",                 # CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    "\xD3\xA6"         => "\xD3\xA7",                 # CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    "\xD3\xA8"         => "\xD3\xA9",                 # CYRILLIC CAPITAL LETTER BARRED O
    "\xD3\xAA"         => "\xD3\xAB",                 # CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    "\xD3\xAC"         => "\xD3\xAD",                 # CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    "\xD3\xAE"         => "\xD3\xAF",                 # CYRILLIC CAPITAL LETTER U WITH MACRON
    "\xD3\xB0"         => "\xD3\xB1",                 # CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    "\xD3\xB2"         => "\xD3\xB3",                 # CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    "\xD3\xB4"         => "\xD3\xB5",                 # CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    "\xD3\xB6"         => "\xD3\xB7",                 # CYRILLIC CAPITAL LETTER GHE WITH DESCENDER
    "\xD3\xB8"         => "\xD3\xB9",                 # CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    "\xD3\xBA"         => "\xD3\xBB",                 # CYRILLIC CAPITAL LETTER GHE WITH STROKE AND HOOK
    "\xD3\xBC"         => "\xD3\xBD",                 # CYRILLIC CAPITAL LETTER HA WITH HOOK
    "\xD3\xBE"         => "\xD3\xBF",                 # CYRILLIC CAPITAL LETTER HA WITH STROKE
    "\xD4\x80"         => "\xD4\x81",                 # CYRILLIC CAPITAL LETTER KOMI DE
    "\xD4\x82"         => "\xD4\x83",                 # CYRILLIC CAPITAL LETTER KOMI DJE
    "\xD4\x84"         => "\xD4\x85",                 # CYRILLIC CAPITAL LETTER KOMI ZJE
    "\xD4\x86"         => "\xD4\x87",                 # CYRILLIC CAPITAL LETTER KOMI DZJE
    "\xD4\x88"         => "\xD4\x89",                 # CYRILLIC CAPITAL LETTER KOMI LJE
    "\xD4\x8A"         => "\xD4\x8B",                 # CYRILLIC CAPITAL LETTER KOMI NJE
    "\xD4\x8C"         => "\xD4\x8D",                 # CYRILLIC CAPITAL LETTER KOMI SJE
    "\xD4\x8E"         => "\xD4\x8F",                 # CYRILLIC CAPITAL LETTER KOMI TJE
    "\xD4\x90"         => "\xD4\x91",                 # CYRILLIC CAPITAL LETTER REVERSED ZE
    "\xD4\x92"         => "\xD4\x93",                 # CYRILLIC CAPITAL LETTER EL WITH HOOK
    "\xD4\x94"         => "\xD4\x95",                 # CYRILLIC CAPITAL LETTER LHA
    "\xD4\x96"         => "\xD4\x97",                 # CYRILLIC CAPITAL LETTER RHA
    "\xD4\x98"         => "\xD4\x99",                 # CYRILLIC CAPITAL LETTER YAE
    "\xD4\x9A"         => "\xD4\x9B",                 # CYRILLIC CAPITAL LETTER QA
    "\xD4\x9C"         => "\xD4\x9D",                 # CYRILLIC CAPITAL LETTER WE
    "\xD4\x9E"         => "\xD4\x9F",                 # CYRILLIC CAPITAL LETTER ALEUT KA
    "\xD4\xA0"         => "\xD4\xA1",                 # CYRILLIC CAPITAL LETTER EL WITH MIDDLE HOOK
    "\xD4\xA2"         => "\xD4\xA3",                 # CYRILLIC CAPITAL LETTER EN WITH MIDDLE HOOK
    "\xD4\xA4"         => "\xD4\xA5",                 # CYRILLIC CAPITAL LETTER PE WITH DESCENDER
    "\xD4\xA6"         => "\xD4\xA7",                 # CYRILLIC CAPITAL LETTER SHHA WITH DESCENDER
    "\xD4\xB1"         => "\xD5\xA1",                 # ARMENIAN CAPITAL LETTER AYB
    "\xD4\xB2"         => "\xD5\xA2",                 # ARMENIAN CAPITAL LETTER BEN
    "\xD4\xB3"         => "\xD5\xA3",                 # ARMENIAN CAPITAL LETTER GIM
    "\xD4\xB4"         => "\xD5\xA4",                 # ARMENIAN CAPITAL LETTER DA
    "\xD4\xB5"         => "\xD5\xA5",                 # ARMENIAN CAPITAL LETTER ECH
    "\xD4\xB6"         => "\xD5\xA6",                 # ARMENIAN CAPITAL LETTER ZA
    "\xD4\xB7"         => "\xD5\xA7",                 # ARMENIAN CAPITAL LETTER EH
    "\xD4\xB8"         => "\xD5\xA8",                 # ARMENIAN CAPITAL LETTER ET
    "\xD4\xB9"         => "\xD5\xA9",                 # ARMENIAN CAPITAL LETTER TO
    "\xD4\xBA"         => "\xD5\xAA",                 # ARMENIAN CAPITAL LETTER ZHE
    "\xD4\xBB"         => "\xD5\xAB",                 # ARMENIAN CAPITAL LETTER INI
    "\xD4\xBC"         => "\xD5\xAC",                 # ARMENIAN CAPITAL LETTER LIWN
    "\xD4\xBD"         => "\xD5\xAD",                 # ARMENIAN CAPITAL LETTER XEH
    "\xD4\xBE"         => "\xD5\xAE",                 # ARMENIAN CAPITAL LETTER CA
    "\xD4\xBF"         => "\xD5\xAF",                 # ARMENIAN CAPITAL LETTER KEN
    "\xD5\x80"         => "\xD5\xB0",                 # ARMENIAN CAPITAL LETTER HO
    "\xD5\x81"         => "\xD5\xB1",                 # ARMENIAN CAPITAL LETTER JA
    "\xD5\x82"         => "\xD5\xB2",                 # ARMENIAN CAPITAL LETTER GHAD
    "\xD5\x83"         => "\xD5\xB3",                 # ARMENIAN CAPITAL LETTER CHEH
    "\xD5\x84"         => "\xD5\xB4",                 # ARMENIAN CAPITAL LETTER MEN
    "\xD5\x85"         => "\xD5\xB5",                 # ARMENIAN CAPITAL LETTER YI
    "\xD5\x86"         => "\xD5\xB6",                 # ARMENIAN CAPITAL LETTER NOW
    "\xD5\x87"         => "\xD5\xB7",                 # ARMENIAN CAPITAL LETTER SHA
    "\xD5\x88"         => "\xD5\xB8",                 # ARMENIAN CAPITAL LETTER VO
    "\xD5\x89"         => "\xD5\xB9",                 # ARMENIAN CAPITAL LETTER CHA
    "\xD5\x8A"         => "\xD5\xBA",                 # ARMENIAN CAPITAL LETTER PEH
    "\xD5\x8B"         => "\xD5\xBB",                 # ARMENIAN CAPITAL LETTER JHEH
    "\xD5\x8C"         => "\xD5\xBC",                 # ARMENIAN CAPITAL LETTER RA
    "\xD5\x8D"         => "\xD5\xBD",                 # ARMENIAN CAPITAL LETTER SEH
    "\xD5\x8E"         => "\xD5\xBE",                 # ARMENIAN CAPITAL LETTER VEW
    "\xD5\x8F"         => "\xD5\xBF",                 # ARMENIAN CAPITAL LETTER TIWN
    "\xD5\x90"         => "\xD6\x80",                 # ARMENIAN CAPITAL LETTER REH
    "\xD5\x91"         => "\xD6\x81",                 # ARMENIAN CAPITAL LETTER CO
    "\xD5\x92"         => "\xD6\x82",                 # ARMENIAN CAPITAL LETTER YIWN
    "\xD5\x93"         => "\xD6\x83",                 # ARMENIAN CAPITAL LETTER PIWR
    "\xD5\x94"         => "\xD6\x84",                 # ARMENIAN CAPITAL LETTER KEH
    "\xD5\x95"         => "\xD6\x85",                 # ARMENIAN CAPITAL LETTER OH
    "\xD5\x96"         => "\xD6\x86",                 # ARMENIAN CAPITAL LETTER FEH
    "\xD6\x87"         => "\xD5\xA5\xD6\x82",         # ARMENIAN SMALL LIGATURE ECH YIWN
    "\xE1\x82\xA0"     => "\xE2\xB4\x80",             # GEORGIAN CAPITAL LETTER AN
    "\xE1\x82\xA1"     => "\xE2\xB4\x81",             # GEORGIAN CAPITAL LETTER BAN
    "\xE1\x82\xA2"     => "\xE2\xB4\x82",             # GEORGIAN CAPITAL LETTER GAN
    "\xE1\x82\xA3"     => "\xE2\xB4\x83",             # GEORGIAN CAPITAL LETTER DON
    "\xE1\x82\xA4"     => "\xE2\xB4\x84",             # GEORGIAN CAPITAL LETTER EN
    "\xE1\x82\xA5"     => "\xE2\xB4\x85",             # GEORGIAN CAPITAL LETTER VIN
    "\xE1\x82\xA6"     => "\xE2\xB4\x86",             # GEORGIAN CAPITAL LETTER ZEN
    "\xE1\x82\xA7"     => "\xE2\xB4\x87",             # GEORGIAN CAPITAL LETTER TAN
    "\xE1\x82\xA8"     => "\xE2\xB4\x88",             # GEORGIAN CAPITAL LETTER IN
    "\xE1\x82\xA9"     => "\xE2\xB4\x89",             # GEORGIAN CAPITAL LETTER KAN
    "\xE1\x82\xAA"     => "\xE2\xB4\x8A",             # GEORGIAN CAPITAL LETTER LAS
    "\xE1\x82\xAB"     => "\xE2\xB4\x8B",             # GEORGIAN CAPITAL LETTER MAN
    "\xE1\x82\xAC"     => "\xE2\xB4\x8C",             # GEORGIAN CAPITAL LETTER NAR
    "\xE1\x82\xAD"     => "\xE2\xB4\x8D",             # GEORGIAN CAPITAL LETTER ON
    "\xE1\x82\xAE"     => "\xE2\xB4\x8E",             # GEORGIAN CAPITAL LETTER PAR
    "\xE1\x82\xAF"     => "\xE2\xB4\x8F",             # GEORGIAN CAPITAL LETTER ZHAR
    "\xE1\x82\xB0"     => "\xE2\xB4\x90",             # GEORGIAN CAPITAL LETTER RAE
    "\xE1\x82\xB1"     => "\xE2\xB4\x91",             # GEORGIAN CAPITAL LETTER SAN
    "\xE1\x82\xB2"     => "\xE2\xB4\x92",             # GEORGIAN CAPITAL LETTER TAR
    "\xE1\x82\xB3"     => "\xE2\xB4\x93",             # GEORGIAN CAPITAL LETTER UN
    "\xE1\x82\xB4"     => "\xE2\xB4\x94",             # GEORGIAN CAPITAL LETTER PHAR
    "\xE1\x82\xB5"     => "\xE2\xB4\x95",             # GEORGIAN CAPITAL LETTER KHAR
    "\xE1\x82\xB6"     => "\xE2\xB4\x96",             # GEORGIAN CAPITAL LETTER GHAN
    "\xE1\x82\xB7"     => "\xE2\xB4\x97",             # GEORGIAN CAPITAL LETTER QAR
    "\xE1\x82\xB8"     => "\xE2\xB4\x98",             # GEORGIAN CAPITAL LETTER SHIN
    "\xE1\x82\xB9"     => "\xE2\xB4\x99",             # GEORGIAN CAPITAL LETTER CHIN
    "\xE1\x82\xBA"     => "\xE2\xB4\x9A",             # GEORGIAN CAPITAL LETTER CAN
    "\xE1\x82\xBB"     => "\xE2\xB4\x9B",             # GEORGIAN CAPITAL LETTER JIL
    "\xE1\x82\xBC"     => "\xE2\xB4\x9C",             # GEORGIAN CAPITAL LETTER CIL
    "\xE1\x82\xBD"     => "\xE2\xB4\x9D",             # GEORGIAN CAPITAL LETTER CHAR
    "\xE1\x82\xBE"     => "\xE2\xB4\x9E",             # GEORGIAN CAPITAL LETTER XAN
    "\xE1\x82\xBF"     => "\xE2\xB4\x9F",             # GEORGIAN CAPITAL LETTER JHAN
    "\xE1\x83\x80"     => "\xE2\xB4\xA0",             # GEORGIAN CAPITAL LETTER HAE
    "\xE1\x83\x81"     => "\xE2\xB4\xA1",             # GEORGIAN CAPITAL LETTER HE
    "\xE1\x83\x82"     => "\xE2\xB4\xA2",             # GEORGIAN CAPITAL LETTER HIE
    "\xE1\x83\x83"     => "\xE2\xB4\xA3",             # GEORGIAN CAPITAL LETTER WE
    "\xE1\x83\x84"     => "\xE2\xB4\xA4",             # GEORGIAN CAPITAL LETTER HAR
    "\xE1\x83\x85"     => "\xE2\xB4\xA5",             # GEORGIAN CAPITAL LETTER HOE
    "\xE1\x83\x87"     => "\xE2\xB4\xA7",             # GEORGIAN CAPITAL LETTER YN
    "\xE1\x83\x8D"     => "\xE2\xB4\xAD",             # GEORGIAN CAPITAL LETTER AEN
    "\xE1\xB8\x80"     => "\xE1\xB8\x81",             # LATIN CAPITAL LETTER A WITH RING BELOW
    "\xE1\xB8\x82"     => "\xE1\xB8\x83",             # LATIN CAPITAL LETTER B WITH DOT ABOVE
    "\xE1\xB8\x84"     => "\xE1\xB8\x85",             # LATIN CAPITAL LETTER B WITH DOT BELOW
    "\xE1\xB8\x86"     => "\xE1\xB8\x87",             # LATIN CAPITAL LETTER B WITH LINE BELOW
    "\xE1\xB8\x88"     => "\xE1\xB8\x89",             # LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    "\xE1\xB8\x8A"     => "\xE1\xB8\x8B",             # LATIN CAPITAL LETTER D WITH DOT ABOVE
    "\xE1\xB8\x8C"     => "\xE1\xB8\x8D",             # LATIN CAPITAL LETTER D WITH DOT BELOW
    "\xE1\xB8\x8E"     => "\xE1\xB8\x8F",             # LATIN CAPITAL LETTER D WITH LINE BELOW
    "\xE1\xB8\x90"     => "\xE1\xB8\x91",             # LATIN CAPITAL LETTER D WITH CEDILLA
    "\xE1\xB8\x92"     => "\xE1\xB8\x93",             # LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    "\xE1\xB8\x94"     => "\xE1\xB8\x95",             # LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    "\xE1\xB8\x96"     => "\xE1\xB8\x97",             # LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    "\xE1\xB8\x98"     => "\xE1\xB8\x99",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    "\xE1\xB8\x9A"     => "\xE1\xB8\x9B",             # LATIN CAPITAL LETTER E WITH TILDE BELOW
    "\xE1\xB8\x9C"     => "\xE1\xB8\x9D",             # LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    "\xE1\xB8\x9E"     => "\xE1\xB8\x9F",             # LATIN CAPITAL LETTER F WITH DOT ABOVE
    "\xE1\xB8\xA0"     => "\xE1\xB8\xA1",             # LATIN CAPITAL LETTER G WITH MACRON
    "\xE1\xB8\xA2"     => "\xE1\xB8\xA3",             # LATIN CAPITAL LETTER H WITH DOT ABOVE
    "\xE1\xB8\xA4"     => "\xE1\xB8\xA5",             # LATIN CAPITAL LETTER H WITH DOT BELOW
    "\xE1\xB8\xA6"     => "\xE1\xB8\xA7",             # LATIN CAPITAL LETTER H WITH DIAERESIS
    "\xE1\xB8\xA8"     => "\xE1\xB8\xA9",             # LATIN CAPITAL LETTER H WITH CEDILLA
    "\xE1\xB8\xAA"     => "\xE1\xB8\xAB",             # LATIN CAPITAL LETTER H WITH BREVE BELOW
    "\xE1\xB8\xAC"     => "\xE1\xB8\xAD",             # LATIN CAPITAL LETTER I WITH TILDE BELOW
    "\xE1\xB8\xAE"     => "\xE1\xB8\xAF",             # LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    "\xE1\xB8\xB0"     => "\xE1\xB8\xB1",             # LATIN CAPITAL LETTER K WITH ACUTE
    "\xE1\xB8\xB2"     => "\xE1\xB8\xB3",             # LATIN CAPITAL LETTER K WITH DOT BELOW
    "\xE1\xB8\xB4"     => "\xE1\xB8\xB5",             # LATIN CAPITAL LETTER K WITH LINE BELOW
    "\xE1\xB8\xB6"     => "\xE1\xB8\xB7",             # LATIN CAPITAL LETTER L WITH DOT BELOW
    "\xE1\xB8\xB8"     => "\xE1\xB8\xB9",             # LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    "\xE1\xB8\xBA"     => "\xE1\xB8\xBB",             # LATIN CAPITAL LETTER L WITH LINE BELOW
    "\xE1\xB8\xBC"     => "\xE1\xB8\xBD",             # LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    "\xE1\xB8\xBE"     => "\xE1\xB8\xBF",             # LATIN CAPITAL LETTER M WITH ACUTE
    "\xE1\xB9\x80"     => "\xE1\xB9\x81",             # LATIN CAPITAL LETTER M WITH DOT ABOVE
    "\xE1\xB9\x82"     => "\xE1\xB9\x83",             # LATIN CAPITAL LETTER M WITH DOT BELOW
    "\xE1\xB9\x84"     => "\xE1\xB9\x85",             # LATIN CAPITAL LETTER N WITH DOT ABOVE
    "\xE1\xB9\x86"     => "\xE1\xB9\x87",             # LATIN CAPITAL LETTER N WITH DOT BELOW
    "\xE1\xB9\x88"     => "\xE1\xB9\x89",             # LATIN CAPITAL LETTER N WITH LINE BELOW
    "\xE1\xB9\x8A"     => "\xE1\xB9\x8B",             # LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    "\xE1\xB9\x8C"     => "\xE1\xB9\x8D",             # LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    "\xE1\xB9\x8E"     => "\xE1\xB9\x8F",             # LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    "\xE1\xB9\x90"     => "\xE1\xB9\x91",             # LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    "\xE1\xB9\x92"     => "\xE1\xB9\x93",             # LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    "\xE1\xB9\x94"     => "\xE1\xB9\x95",             # LATIN CAPITAL LETTER P WITH ACUTE
    "\xE1\xB9\x96"     => "\xE1\xB9\x97",             # LATIN CAPITAL LETTER P WITH DOT ABOVE
    "\xE1\xB9\x98"     => "\xE1\xB9\x99",             # LATIN CAPITAL LETTER R WITH DOT ABOVE
    "\xE1\xB9\x9A"     => "\xE1\xB9\x9B",             # LATIN CAPITAL LETTER R WITH DOT BELOW
    "\xE1\xB9\x9C"     => "\xE1\xB9\x9D",             # LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    "\xE1\xB9\x9E"     => "\xE1\xB9\x9F",             # LATIN CAPITAL LETTER R WITH LINE BELOW
    "\xE1\xB9\xA0"     => "\xE1\xB9\xA1",             # LATIN CAPITAL LETTER S WITH DOT ABOVE
    "\xE1\xB9\xA2"     => "\xE1\xB9\xA3",             # LATIN CAPITAL LETTER S WITH DOT BELOW
    "\xE1\xB9\xA4"     => "\xE1\xB9\xA5",             # LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    "\xE1\xB9\xA6"     => "\xE1\xB9\xA7",             # LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    "\xE1\xB9\xA8"     => "\xE1\xB9\xA9",             # LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    "\xE1\xB9\xAA"     => "\xE1\xB9\xAB",             # LATIN CAPITAL LETTER T WITH DOT ABOVE
    "\xE1\xB9\xAC"     => "\xE1\xB9\xAD",             # LATIN CAPITAL LETTER T WITH DOT BELOW
    "\xE1\xB9\xAE"     => "\xE1\xB9\xAF",             # LATIN CAPITAL LETTER T WITH LINE BELOW
    "\xE1\xB9\xB0"     => "\xE1\xB9\xB1",             # LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    "\xE1\xB9\xB2"     => "\xE1\xB9\xB3",             # LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    "\xE1\xB9\xB4"     => "\xE1\xB9\xB5",             # LATIN CAPITAL LETTER U WITH TILDE BELOW
    "\xE1\xB9\xB6"     => "\xE1\xB9\xB7",             # LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    "\xE1\xB9\xB8"     => "\xE1\xB9\xB9",             # LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    "\xE1\xB9\xBA"     => "\xE1\xB9\xBB",             # LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    "\xE1\xB9\xBC"     => "\xE1\xB9\xBD",             # LATIN CAPITAL LETTER V WITH TILDE
    "\xE1\xB9\xBE"     => "\xE1\xB9\xBF",             # LATIN CAPITAL LETTER V WITH DOT BELOW
    "\xE1\xBA\x80"     => "\xE1\xBA\x81",             # LATIN CAPITAL LETTER W WITH GRAVE
    "\xE1\xBA\x82"     => "\xE1\xBA\x83",             # LATIN CAPITAL LETTER W WITH ACUTE
    "\xE1\xBA\x84"     => "\xE1\xBA\x85",             # LATIN CAPITAL LETTER W WITH DIAERESIS
    "\xE1\xBA\x86"     => "\xE1\xBA\x87",             # LATIN CAPITAL LETTER W WITH DOT ABOVE
    "\xE1\xBA\x88"     => "\xE1\xBA\x89",             # LATIN CAPITAL LETTER W WITH DOT BELOW
    "\xE1\xBA\x8A"     => "\xE1\xBA\x8B",             # LATIN CAPITAL LETTER X WITH DOT ABOVE
    "\xE1\xBA\x8C"     => "\xE1\xBA\x8D",             # LATIN CAPITAL LETTER X WITH DIAERESIS
    "\xE1\xBA\x8E"     => "\xE1\xBA\x8F",             # LATIN CAPITAL LETTER Y WITH DOT ABOVE
    "\xE1\xBA\x90"     => "\xE1\xBA\x91",             # LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    "\xE1\xBA\x92"     => "\xE1\xBA\x93",             # LATIN CAPITAL LETTER Z WITH DOT BELOW
    "\xE1\xBA\x94"     => "\xE1\xBA\x95",             # LATIN CAPITAL LETTER Z WITH LINE BELOW
    "\xE1\xBA\x96"     => "\x68\xCC\xB1",             # LATIN SMALL LETTER H WITH LINE BELOW
    "\xE1\xBA\x97"     => "\x74\xCC\x88",             # LATIN SMALL LETTER T WITH DIAERESIS
    "\xE1\xBA\x98"     => "\x77\xCC\x8A",             # LATIN SMALL LETTER W WITH RING ABOVE
    "\xE1\xBA\x99"     => "\x79\xCC\x8A",             # LATIN SMALL LETTER Y WITH RING ABOVE
    "\xE1\xBA\x9A"     => "\x61\xCA\xBE",             # LATIN SMALL LETTER A WITH RIGHT HALF RING
    "\xE1\xBA\x9B"     => "\xE1\xB9\xA1",             # LATIN SMALL LETTER LONG S WITH DOT ABOVE
    "\xE1\xBA\x9E"     => "\x73\x73",                 # LATIN CAPITAL LETTER SHARP S
    "\xE1\xBA\xA0"     => "\xE1\xBA\xA1",             # LATIN CAPITAL LETTER A WITH DOT BELOW
    "\xE1\xBA\xA2"     => "\xE1\xBA\xA3",             # LATIN CAPITAL LETTER A WITH HOOK ABOVE
    "\xE1\xBA\xA4"     => "\xE1\xBA\xA5",             # LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    "\xE1\xBA\xA6"     => "\xE1\xBA\xA7",             # LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    "\xE1\xBA\xA8"     => "\xE1\xBA\xA9",             # LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    "\xE1\xBA\xAA"     => "\xE1\xBA\xAB",             # LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    "\xE1\xBA\xAC"     => "\xE1\xBA\xAD",             # LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    "\xE1\xBA\xAE"     => "\xE1\xBA\xAF",             # LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    "\xE1\xBA\xB0"     => "\xE1\xBA\xB1",             # LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    "\xE1\xBA\xB2"     => "\xE1\xBA\xB3",             # LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    "\xE1\xBA\xB4"     => "\xE1\xBA\xB5",             # LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    "\xE1\xBA\xB6"     => "\xE1\xBA\xB7",             # LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    "\xE1\xBA\xB8"     => "\xE1\xBA\xB9",             # LATIN CAPITAL LETTER E WITH DOT BELOW
    "\xE1\xBA\xBA"     => "\xE1\xBA\xBB",             # LATIN CAPITAL LETTER E WITH HOOK ABOVE
    "\xE1\xBA\xBC"     => "\xE1\xBA\xBD",             # LATIN CAPITAL LETTER E WITH TILDE
    "\xE1\xBA\xBE"     => "\xE1\xBA\xBF",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    "\xE1\xBB\x80"     => "\xE1\xBB\x81",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    "\xE1\xBB\x82"     => "\xE1\xBB\x83",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    "\xE1\xBB\x84"     => "\xE1\xBB\x85",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    "\xE1\xBB\x86"     => "\xE1\xBB\x87",             # LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    "\xE1\xBB\x88"     => "\xE1\xBB\x89",             # LATIN CAPITAL LETTER I WITH HOOK ABOVE
    "\xE1\xBB\x8A"     => "\xE1\xBB\x8B",             # LATIN CAPITAL LETTER I WITH DOT BELOW
    "\xE1\xBB\x8C"     => "\xE1\xBB\x8D",             # LATIN CAPITAL LETTER O WITH DOT BELOW
    "\xE1\xBB\x8E"     => "\xE1\xBB\x8F",             # LATIN CAPITAL LETTER O WITH HOOK ABOVE
    "\xE1\xBB\x90"     => "\xE1\xBB\x91",             # LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    "\xE1\xBB\x92"     => "\xE1\xBB\x93",             # LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    "\xE1\xBB\x94"     => "\xE1\xBB\x95",             # LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    "\xE1\xBB\x96"     => "\xE1\xBB\x97",             # LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    "\xE1\xBB\x98"     => "\xE1\xBB\x99",             # LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    "\xE1\xBB\x9A"     => "\xE1\xBB\x9B",             # LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    "\xE1\xBB\x9C"     => "\xE1\xBB\x9D",             # LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    "\xE1\xBB\x9E"     => "\xE1\xBB\x9F",             # LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    "\xE1\xBB\xA0"     => "\xE1\xBB\xA1",             # LATIN CAPITAL LETTER O WITH HORN AND TILDE
    "\xE1\xBB\xA2"     => "\xE1\xBB\xA3",             # LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    "\xE1\xBB\xA4"     => "\xE1\xBB\xA5",             # LATIN CAPITAL LETTER U WITH DOT BELOW
    "\xE1\xBB\xA6"     => "\xE1\xBB\xA7",             # LATIN CAPITAL LETTER U WITH HOOK ABOVE
    "\xE1\xBB\xA8"     => "\xE1\xBB\xA9",             # LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    "\xE1\xBB\xAA"     => "\xE1\xBB\xAB",             # LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    "\xE1\xBB\xAC"     => "\xE1\xBB\xAD",             # LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    "\xE1\xBB\xAE"     => "\xE1\xBB\xAF",             # LATIN CAPITAL LETTER U WITH HORN AND TILDE
    "\xE1\xBB\xB0"     => "\xE1\xBB\xB1",             # LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    "\xE1\xBB\xB2"     => "\xE1\xBB\xB3",             # LATIN CAPITAL LETTER Y WITH GRAVE
    "\xE1\xBB\xB4"     => "\xE1\xBB\xB5",             # LATIN CAPITAL LETTER Y WITH DOT BELOW
    "\xE1\xBB\xB6"     => "\xE1\xBB\xB7",             # LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    "\xE1\xBB\xB8"     => "\xE1\xBB\xB9",             # LATIN CAPITAL LETTER Y WITH TILDE
    "\xE1\xBB\xBA"     => "\xE1\xBB\xBB",             # LATIN CAPITAL LETTER MIDDLE-WELSH LL
    "\xE1\xBB\xBC"     => "\xE1\xBB\xBD",             # LATIN CAPITAL LETTER MIDDLE-WELSH V
    "\xE1\xBB\xBE"     => "\xE1\xBB\xBF",             # LATIN CAPITAL LETTER Y WITH LOOP
    "\xE1\xBC\x88"     => "\xE1\xBC\x80",             # GREEK CAPITAL LETTER ALPHA WITH PSILI
    "\xE1\xBC\x89"     => "\xE1\xBC\x81",             # GREEK CAPITAL LETTER ALPHA WITH DASIA
    "\xE1\xBC\x8A"     => "\xE1\xBC\x82",             # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    "\xE1\xBC\x8B"     => "\xE1\xBC\x83",             # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    "\xE1\xBC\x8C"     => "\xE1\xBC\x84",             # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    "\xE1\xBC\x8D"     => "\xE1\xBC\x85",             # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    "\xE1\xBC\x8E"     => "\xE1\xBC\x86",             # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    "\xE1\xBC\x8F"     => "\xE1\xBC\x87",             # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    "\xE1\xBC\x98"     => "\xE1\xBC\x90",             # GREEK CAPITAL LETTER EPSILON WITH PSILI
    "\xE1\xBC\x99"     => "\xE1\xBC\x91",             # GREEK CAPITAL LETTER EPSILON WITH DASIA
    "\xE1\xBC\x9A"     => "\xE1\xBC\x92",             # GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    "\xE1\xBC\x9B"     => "\xE1\xBC\x93",             # GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    "\xE1\xBC\x9C"     => "\xE1\xBC\x94",             # GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    "\xE1\xBC\x9D"     => "\xE1\xBC\x95",             # GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    "\xE1\xBC\xA8"     => "\xE1\xBC\xA0",             # GREEK CAPITAL LETTER ETA WITH PSILI
    "\xE1\xBC\xA9"     => "\xE1\xBC\xA1",             # GREEK CAPITAL LETTER ETA WITH DASIA
    "\xE1\xBC\xAA"     => "\xE1\xBC\xA2",             # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    "\xE1\xBC\xAB"     => "\xE1\xBC\xA3",             # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    "\xE1\xBC\xAC"     => "\xE1\xBC\xA4",             # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    "\xE1\xBC\xAD"     => "\xE1\xBC\xA5",             # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    "\xE1\xBC\xAE"     => "\xE1\xBC\xA6",             # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    "\xE1\xBC\xAF"     => "\xE1\xBC\xA7",             # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    "\xE1\xBC\xB8"     => "\xE1\xBC\xB0",             # GREEK CAPITAL LETTER IOTA WITH PSILI
    "\xE1\xBC\xB9"     => "\xE1\xBC\xB1",             # GREEK CAPITAL LETTER IOTA WITH DASIA
    "\xE1\xBC\xBA"     => "\xE1\xBC\xB2",             # GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    "\xE1\xBC\xBB"     => "\xE1\xBC\xB3",             # GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    "\xE1\xBC\xBC"     => "\xE1\xBC\xB4",             # GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    "\xE1\xBC\xBD"     => "\xE1\xBC\xB5",             # GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    "\xE1\xBC\xBE"     => "\xE1\xBC\xB6",             # GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    "\xE1\xBC\xBF"     => "\xE1\xBC\xB7",             # GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    "\xE1\xBD\x88"     => "\xE1\xBD\x80",             # GREEK CAPITAL LETTER OMICRON WITH PSILI
    "\xE1\xBD\x89"     => "\xE1\xBD\x81",             # GREEK CAPITAL LETTER OMICRON WITH DASIA
    "\xE1\xBD\x8A"     => "\xE1\xBD\x82",             # GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    "\xE1\xBD\x8B"     => "\xE1\xBD\x83",             # GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    "\xE1\xBD\x8C"     => "\xE1\xBD\x84",             # GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    "\xE1\xBD\x8D"     => "\xE1\xBD\x85",             # GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    "\xE1\xBD\x90"     => "\xCF\x85\xCC\x93",         # GREEK SMALL LETTER UPSILON WITH PSILI
    "\xE1\xBD\x92"     => "\xCF\x85\xCC\x93\xCC\x80", # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    "\xE1\xBD\x94"     => "\xCF\x85\xCC\x93\xCC\x81", # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    "\xE1\xBD\x96"     => "\xCF\x85\xCC\x93\xCD\x82", # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    "\xE1\xBD\x99"     => "\xE1\xBD\x91",             # GREEK CAPITAL LETTER UPSILON WITH DASIA
    "\xE1\xBD\x9B"     => "\xE1\xBD\x93",             # GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    "\xE1\xBD\x9D"     => "\xE1\xBD\x95",             # GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    "\xE1\xBD\x9F"     => "\xE1\xBD\x97",             # GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    "\xE1\xBD\xA8"     => "\xE1\xBD\xA0",             # GREEK CAPITAL LETTER OMEGA WITH PSILI
    "\xE1\xBD\xA9"     => "\xE1\xBD\xA1",             # GREEK CAPITAL LETTER OMEGA WITH DASIA
    "\xE1\xBD\xAA"     => "\xE1\xBD\xA2",             # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    "\xE1\xBD\xAB"     => "\xE1\xBD\xA3",             # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    "\xE1\xBD\xAC"     => "\xE1\xBD\xA4",             # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    "\xE1\xBD\xAD"     => "\xE1\xBD\xA5",             # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    "\xE1\xBD\xAE"     => "\xE1\xBD\xA6",             # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    "\xE1\xBD\xAF"     => "\xE1\xBD\xA7",             # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    "\xE1\xBE\x80"     => "\xE1\xBC\x80\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    "\xE1\xBE\x81"     => "\xE1\xBC\x81\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    "\xE1\xBE\x82"     => "\xE1\xBC\x82\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\x83"     => "\xE1\xBC\x83\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\x84"     => "\xE1\xBC\x84\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\x85"     => "\xE1\xBC\x85\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\x86"     => "\xE1\xBC\x86\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\x87"     => "\xE1\xBC\x87\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\x88"     => "\xE1\xBC\x80\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    "\xE1\xBE\x89"     => "\xE1\xBC\x81\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    "\xE1\xBE\x8A"     => "\xE1\xBC\x82\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\x8B"     => "\xE1\xBC\x83\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\x8C"     => "\xE1\xBC\x84\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\x8D"     => "\xE1\xBC\x85\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\x8E"     => "\xE1\xBC\x86\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\x8F"     => "\xE1\xBC\x87\xCE\xB9",     # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\x90"     => "\xE1\xBC\xA0\xCE\xB9",     # GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    "\xE1\xBE\x91"     => "\xE1\xBC\xA1\xCE\xB9",     # GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    "\xE1\xBE\x92"     => "\xE1\xBC\xA2\xCE\xB9",     # GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\x93"     => "\xE1\xBC\xA3\xCE\xB9",     # GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\x94"     => "\xE1\xBC\xA4\xCE\xB9",     # GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\x95"     => "\xE1\xBC\xA5\xCE\xB9",     # GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\x96"     => "\xE1\xBC\xA6\xCE\xB9",     # GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\x97"     => "\xE1\xBC\xA7\xCE\xB9",     # GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\x98"     => "\xE1\xBC\xA0\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    "\xE1\xBE\x99"     => "\xE1\xBC\xA1\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    "\xE1\xBE\x9A"     => "\xE1\xBC\xA2\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\x9B"     => "\xE1\xBC\xA3\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\x9C"     => "\xE1\xBC\xA4\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\x9D"     => "\xE1\xBC\xA5\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\x9E"     => "\xE1\xBC\xA6\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\x9F"     => "\xE1\xBC\xA7\xCE\xB9",     # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\xA0"     => "\xE1\xBD\xA0\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    "\xE1\xBE\xA1"     => "\xE1\xBD\xA1\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    "\xE1\xBE\xA2"     => "\xE1\xBD\xA2\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\xA3"     => "\xE1\xBD\xA3\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\xA4"     => "\xE1\xBD\xA4\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\xA5"     => "\xE1\xBD\xA5\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\xA6"     => "\xE1\xBD\xA6\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\xA7"     => "\xE1\xBD\xA7\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\xA8"     => "\xE1\xBD\xA0\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    "\xE1\xBE\xA9"     => "\xE1\xBD\xA1\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    "\xE1\xBE\xAA"     => "\xE1\xBD\xA2\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\xAB"     => "\xE1\xBD\xA3\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    "\xE1\xBE\xAC"     => "\xE1\xBD\xA4\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\xAD"     => "\xE1\xBD\xA5\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    "\xE1\xBE\xAE"     => "\xE1\xBD\xA6\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\xAF"     => "\xE1\xBD\xA7\xCE\xB9",     # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    "\xE1\xBE\xB2"     => "\xE1\xBD\xB0\xCE\xB9",     # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    "\xE1\xBE\xB3"     => "\xCE\xB1\xCE\xB9",         # GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    "\xE1\xBE\xB4"     => "\xCE\xAC\xCE\xB9",         # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    "\xE1\xBE\xB6"     => "\xCE\xB1\xCD\x82",         # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    "\xE1\xBE\xB7"     => "\xCE\xB1\xCD\x82\xCE\xB9", # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBE\xB8"     => "\xE1\xBE\xB0",             # GREEK CAPITAL LETTER ALPHA WITH VRACHY
    "\xE1\xBE\xB9"     => "\xE1\xBE\xB1",             # GREEK CAPITAL LETTER ALPHA WITH MACRON
    "\xE1\xBE\xBA"     => "\xE1\xBD\xB0",             # GREEK CAPITAL LETTER ALPHA WITH VARIA
    "\xE1\xBE\xBB"     => "\xE1\xBD\xB1",             # GREEK CAPITAL LETTER ALPHA WITH OXIA
    "\xE1\xBE\xBC"     => "\xCE\xB1\xCE\xB9",         # GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    "\xE1\xBE\xBE"     => "\xCE\xB9",                 # GREEK PROSGEGRAMMENI
    "\xE1\xBF\x82"     => "\xE1\xBD\xB4\xCE\xB9",     # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    "\xE1\xBF\x83"     => "\xCE\xB7\xCE\xB9",         # GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    "\xE1\xBF\x84"     => "\xCE\xAE\xCE\xB9",         # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    "\xE1\xBF\x86"     => "\xCE\xB7\xCD\x82",         # GREEK SMALL LETTER ETA WITH PERISPOMENI
    "\xE1\xBF\x87"     => "\xCE\xB7\xCD\x82\xCE\xB9", # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBF\x88"     => "\xE1\xBD\xB2",             # GREEK CAPITAL LETTER EPSILON WITH VARIA
    "\xE1\xBF\x89"     => "\xE1\xBD\xB3",             # GREEK CAPITAL LETTER EPSILON WITH OXIA
    "\xE1\xBF\x8A"     => "\xE1\xBD\xB4",             # GREEK CAPITAL LETTER ETA WITH VARIA
    "\xE1\xBF\x8B"     => "\xE1\xBD\xB5",             # GREEK CAPITAL LETTER ETA WITH OXIA
    "\xE1\xBF\x8C"     => "\xCE\xB7\xCE\xB9",         # GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    "\xE1\xBF\x92"     => "\xCE\xB9\xCC\x88\xCC\x80", # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    "\xE1\xBF\x93"     => "\xCE\xB9\xCC\x88\xCC\x81", # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    "\xE1\xBF\x96"     => "\xCE\xB9\xCD\x82",         # GREEK SMALL LETTER IOTA WITH PERISPOMENI
    "\xE1\xBF\x97"     => "\xCE\xB9\xCC\x88\xCD\x82", # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    "\xE1\xBF\x98"     => "\xE1\xBF\x90",             # GREEK CAPITAL LETTER IOTA WITH VRACHY
    "\xE1\xBF\x99"     => "\xE1\xBF\x91",             # GREEK CAPITAL LETTER IOTA WITH MACRON
    "\xE1\xBF\x9A"     => "\xE1\xBD\xB6",             # GREEK CAPITAL LETTER IOTA WITH VARIA
    "\xE1\xBF\x9B"     => "\xE1\xBD\xB7",             # GREEK CAPITAL LETTER IOTA WITH OXIA
    "\xE1\xBF\xA2"     => "\xCF\x85\xCC\x88\xCC\x80", # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    "\xE1\xBF\xA3"     => "\xCF\x85\xCC\x88\xCC\x81", # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    "\xE1\xBF\xA4"     => "\xCF\x81\xCC\x93",         # GREEK SMALL LETTER RHO WITH PSILI
    "\xE1\xBF\xA6"     => "\xCF\x85\xCD\x82",         # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    "\xE1\xBF\xA7"     => "\xCF\x85\xCC\x88\xCD\x82", # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    "\xE1\xBF\xA8"     => "\xE1\xBF\xA0",             # GREEK CAPITAL LETTER UPSILON WITH VRACHY
    "\xE1\xBF\xA9"     => "\xE1\xBF\xA1",             # GREEK CAPITAL LETTER UPSILON WITH MACRON
    "\xE1\xBF\xAA"     => "\xE1\xBD\xBA",             # GREEK CAPITAL LETTER UPSILON WITH VARIA
    "\xE1\xBF\xAB"     => "\xE1\xBD\xBB",             # GREEK CAPITAL LETTER UPSILON WITH OXIA
    "\xE1\xBF\xAC"     => "\xE1\xBF\xA5",             # GREEK CAPITAL LETTER RHO WITH DASIA
    "\xE1\xBF\xB2"     => "\xE1\xBD\xBC\xCE\xB9",     # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    "\xE1\xBF\xB3"     => "\xCF\x89\xCE\xB9",         # GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    "\xE1\xBF\xB4"     => "\xCF\x8E\xCE\xB9",         # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    "\xE1\xBF\xB6"     => "\xCF\x89\xCD\x82",         # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    "\xE1\xBF\xB7"     => "\xCF\x89\xCD\x82\xCE\xB9", # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    "\xE1\xBF\xB8"     => "\xE1\xBD\xB8",             # GREEK CAPITAL LETTER OMICRON WITH VARIA
    "\xE1\xBF\xB9"     => "\xE1\xBD\xB9",             # GREEK CAPITAL LETTER OMICRON WITH OXIA
    "\xE1\xBF\xBA"     => "\xE1\xBD\xBC",             # GREEK CAPITAL LETTER OMEGA WITH VARIA
    "\xE1\xBF\xBB"     => "\xE1\xBD\xBD",             # GREEK CAPITAL LETTER OMEGA WITH OXIA
    "\xE1\xBF\xBC"     => "\xCF\x89\xCE\xB9",         # GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    "\xE2\x84\xA6"     => "\xCF\x89",                 # OHM SIGN
    "\xE2\x84\xAA"     => "\x6B",                     # KELVIN SIGN
    "\xE2\x84\xAB"     => "\xC3\xA5",                 # ANGSTROM SIGN
    "\xE2\x84\xB2"     => "\xE2\x85\x8E",             # TURNED CAPITAL F
    "\xE2\x85\xA0"     => "\xE2\x85\xB0",             # ROMAN NUMERAL ONE
    "\xE2\x85\xA1"     => "\xE2\x85\xB1",             # ROMAN NUMERAL TWO
    "\xE2\x85\xA2"     => "\xE2\x85\xB2",             # ROMAN NUMERAL THREE
    "\xE2\x85\xA3"     => "\xE2\x85\xB3",             # ROMAN NUMERAL FOUR
    "\xE2\x85\xA4"     => "\xE2\x85\xB4",             # ROMAN NUMERAL FIVE
    "\xE2\x85\xA5"     => "\xE2\x85\xB5",             # ROMAN NUMERAL SIX
    "\xE2\x85\xA6"     => "\xE2\x85\xB6",             # ROMAN NUMERAL SEVEN
    "\xE2\x85\xA7"     => "\xE2\x85\xB7",             # ROMAN NUMERAL EIGHT
    "\xE2\x85\xA8"     => "\xE2\x85\xB8",             # ROMAN NUMERAL NINE
    "\xE2\x85\xA9"     => "\xE2\x85\xB9",             # ROMAN NUMERAL TEN
    "\xE2\x85\xAA"     => "\xE2\x85\xBA",             # ROMAN NUMERAL ELEVEN
    "\xE2\x85\xAB"     => "\xE2\x85\xBB",             # ROMAN NUMERAL TWELVE
    "\xE2\x85\xAC"     => "\xE2\x85\xBC",             # ROMAN NUMERAL FIFTY
    "\xE2\x85\xAD"     => "\xE2\x85\xBD",             # ROMAN NUMERAL ONE HUNDRED
    "\xE2\x85\xAE"     => "\xE2\x85\xBE",             # ROMAN NUMERAL FIVE HUNDRED
    "\xE2\x85\xAF"     => "\xE2\x85\xBF",             # ROMAN NUMERAL ONE THOUSAND
    "\xE2\x86\x83"     => "\xE2\x86\x84",             # ROMAN NUMERAL REVERSED ONE HUNDRED
    "\xE2\x92\xB6"     => "\xE2\x93\x90",             # CIRCLED LATIN CAPITAL LETTER A
    "\xE2\x92\xB7"     => "\xE2\x93\x91",             # CIRCLED LATIN CAPITAL LETTER B
    "\xE2\x92\xB8"     => "\xE2\x93\x92",             # CIRCLED LATIN CAPITAL LETTER C
    "\xE2\x92\xB9"     => "\xE2\x93\x93",             # CIRCLED LATIN CAPITAL LETTER D
    "\xE2\x92\xBA"     => "\xE2\x93\x94",             # CIRCLED LATIN CAPITAL LETTER E
    "\xE2\x92\xBB"     => "\xE2\x93\x95",             # CIRCLED LATIN CAPITAL LETTER F
    "\xE2\x92\xBC"     => "\xE2\x93\x96",             # CIRCLED LATIN CAPITAL LETTER G
    "\xE2\x92\xBD"     => "\xE2\x93\x97",             # CIRCLED LATIN CAPITAL LETTER H
    "\xE2\x92\xBE"     => "\xE2\x93\x98",             # CIRCLED LATIN CAPITAL LETTER I
    "\xE2\x92\xBF"     => "\xE2\x93\x99",             # CIRCLED LATIN CAPITAL LETTER J
    "\xE2\x93\x80"     => "\xE2\x93\x9A",             # CIRCLED LATIN CAPITAL LETTER K
    "\xE2\x93\x81"     => "\xE2\x93\x9B",             # CIRCLED LATIN CAPITAL LETTER L
    "\xE2\x93\x82"     => "\xE2\x93\x9C",             # CIRCLED LATIN CAPITAL LETTER M
    "\xE2\x93\x83"     => "\xE2\x93\x9D",             # CIRCLED LATIN CAPITAL LETTER N
    "\xE2\x93\x84"     => "\xE2\x93\x9E",             # CIRCLED LATIN CAPITAL LETTER O
    "\xE2\x93\x85"     => "\xE2\x93\x9F",             # CIRCLED LATIN CAPITAL LETTER P
    "\xE2\x93\x86"     => "\xE2\x93\xA0",             # CIRCLED LATIN CAPITAL LETTER Q
    "\xE2\x93\x87"     => "\xE2\x93\xA1",             # CIRCLED LATIN CAPITAL LETTER R
    "\xE2\x93\x88"     => "\xE2\x93\xA2",             # CIRCLED LATIN CAPITAL LETTER S
    "\xE2\x93\x89"     => "\xE2\x93\xA3",             # CIRCLED LATIN CAPITAL LETTER T
    "\xE2\x93\x8A"     => "\xE2\x93\xA4",             # CIRCLED LATIN CAPITAL LETTER U
    "\xE2\x93\x8B"     => "\xE2\x93\xA5",             # CIRCLED LATIN CAPITAL LETTER V
    "\xE2\x93\x8C"     => "\xE2\x93\xA6",             # CIRCLED LATIN CAPITAL LETTER W
    "\xE2\x93\x8D"     => "\xE2\x93\xA7",             # CIRCLED LATIN CAPITAL LETTER X
    "\xE2\x93\x8E"     => "\xE2\x93\xA8",             # CIRCLED LATIN CAPITAL LETTER Y
    "\xE2\x93\x8F"     => "\xE2\x93\xA9",             # CIRCLED LATIN CAPITAL LETTER Z
    "\xE2\xB0\x80"     => "\xE2\xB0\xB0",             # GLAGOLITIC CAPITAL LETTER AZU
    "\xE2\xB0\x81"     => "\xE2\xB0\xB1",             # GLAGOLITIC CAPITAL LETTER BUKY
    "\xE2\xB0\x82"     => "\xE2\xB0\xB2",             # GLAGOLITIC CAPITAL LETTER VEDE
    "\xE2\xB0\x83"     => "\xE2\xB0\xB3",             # GLAGOLITIC CAPITAL LETTER GLAGOLI
    "\xE2\xB0\x84"     => "\xE2\xB0\xB4",             # GLAGOLITIC CAPITAL LETTER DOBRO
    "\xE2\xB0\x85"     => "\xE2\xB0\xB5",             # GLAGOLITIC CAPITAL LETTER YESTU
    "\xE2\xB0\x86"     => "\xE2\xB0\xB6",             # GLAGOLITIC CAPITAL LETTER ZHIVETE
    "\xE2\xB0\x87"     => "\xE2\xB0\xB7",             # GLAGOLITIC CAPITAL LETTER DZELO
    "\xE2\xB0\x88"     => "\xE2\xB0\xB8",             # GLAGOLITIC CAPITAL LETTER ZEMLJA
    "\xE2\xB0\x89"     => "\xE2\xB0\xB9",             # GLAGOLITIC CAPITAL LETTER IZHE
    "\xE2\xB0\x8A"     => "\xE2\xB0\xBA",             # GLAGOLITIC CAPITAL LETTER INITIAL IZHE
    "\xE2\xB0\x8B"     => "\xE2\xB0\xBB",             # GLAGOLITIC CAPITAL LETTER I
    "\xE2\xB0\x8C"     => "\xE2\xB0\xBC",             # GLAGOLITIC CAPITAL LETTER DJERVI
    "\xE2\xB0\x8D"     => "\xE2\xB0\xBD",             # GLAGOLITIC CAPITAL LETTER KAKO
    "\xE2\xB0\x8E"     => "\xE2\xB0\xBE",             # GLAGOLITIC CAPITAL LETTER LJUDIJE
    "\xE2\xB0\x8F"     => "\xE2\xB0\xBF",             # GLAGOLITIC CAPITAL LETTER MYSLITE
    "\xE2\xB0\x90"     => "\xE2\xB1\x80",             # GLAGOLITIC CAPITAL LETTER NASHI
    "\xE2\xB0\x91"     => "\xE2\xB1\x81",             # GLAGOLITIC CAPITAL LETTER ONU
    "\xE2\xB0\x92"     => "\xE2\xB1\x82",             # GLAGOLITIC CAPITAL LETTER POKOJI
    "\xE2\xB0\x93"     => "\xE2\xB1\x83",             # GLAGOLITIC CAPITAL LETTER RITSI
    "\xE2\xB0\x94"     => "\xE2\xB1\x84",             # GLAGOLITIC CAPITAL LETTER SLOVO
    "\xE2\xB0\x95"     => "\xE2\xB1\x85",             # GLAGOLITIC CAPITAL LETTER TVRIDO
    "\xE2\xB0\x96"     => "\xE2\xB1\x86",             # GLAGOLITIC CAPITAL LETTER UKU
    "\xE2\xB0\x97"     => "\xE2\xB1\x87",             # GLAGOLITIC CAPITAL LETTER FRITU
    "\xE2\xB0\x98"     => "\xE2\xB1\x88",             # GLAGOLITIC CAPITAL LETTER HERU
    "\xE2\xB0\x99"     => "\xE2\xB1\x89",             # GLAGOLITIC CAPITAL LETTER OTU
    "\xE2\xB0\x9A"     => "\xE2\xB1\x8A",             # GLAGOLITIC CAPITAL LETTER PE
    "\xE2\xB0\x9B"     => "\xE2\xB1\x8B",             # GLAGOLITIC CAPITAL LETTER SHTA
    "\xE2\xB0\x9C"     => "\xE2\xB1\x8C",             # GLAGOLITIC CAPITAL LETTER TSI
    "\xE2\xB0\x9D"     => "\xE2\xB1\x8D",             # GLAGOLITIC CAPITAL LETTER CHRIVI
    "\xE2\xB0\x9E"     => "\xE2\xB1\x8E",             # GLAGOLITIC CAPITAL LETTER SHA
    "\xE2\xB0\x9F"     => "\xE2\xB1\x8F",             # GLAGOLITIC CAPITAL LETTER YERU
    "\xE2\xB0\xA0"     => "\xE2\xB1\x90",             # GLAGOLITIC CAPITAL LETTER YERI
    "\xE2\xB0\xA1"     => "\xE2\xB1\x91",             # GLAGOLITIC CAPITAL LETTER YATI
    "\xE2\xB0\xA2"     => "\xE2\xB1\x92",             # GLAGOLITIC CAPITAL LETTER SPIDERY HA
    "\xE2\xB0\xA3"     => "\xE2\xB1\x93",             # GLAGOLITIC CAPITAL LETTER YU
    "\xE2\xB0\xA4"     => "\xE2\xB1\x94",             # GLAGOLITIC CAPITAL LETTER SMALL YUS
    "\xE2\xB0\xA5"     => "\xE2\xB1\x95",             # GLAGOLITIC CAPITAL LETTER SMALL YUS WITH TAIL
    "\xE2\xB0\xA6"     => "\xE2\xB1\x96",             # GLAGOLITIC CAPITAL LETTER YO
    "\xE2\xB0\xA7"     => "\xE2\xB1\x97",             # GLAGOLITIC CAPITAL LETTER IOTATED SMALL YUS
    "\xE2\xB0\xA8"     => "\xE2\xB1\x98",             # GLAGOLITIC CAPITAL LETTER BIG YUS
    "\xE2\xB0\xA9"     => "\xE2\xB1\x99",             # GLAGOLITIC CAPITAL LETTER IOTATED BIG YUS
    "\xE2\xB0\xAA"     => "\xE2\xB1\x9A",             # GLAGOLITIC CAPITAL LETTER FITA
    "\xE2\xB0\xAB"     => "\xE2\xB1\x9B",             # GLAGOLITIC CAPITAL LETTER IZHITSA
    "\xE2\xB0\xAC"     => "\xE2\xB1\x9C",             # GLAGOLITIC CAPITAL LETTER SHTAPIC
    "\xE2\xB0\xAD"     => "\xE2\xB1\x9D",             # GLAGOLITIC CAPITAL LETTER TROKUTASTI A
    "\xE2\xB0\xAE"     => "\xE2\xB1\x9E",             # GLAGOLITIC CAPITAL LETTER LATINATE MYSLITE
    "\xE2\xB1\xA0"     => "\xE2\xB1\xA1",             # LATIN CAPITAL LETTER L WITH DOUBLE BAR
    "\xE2\xB1\xA2"     => "\xC9\xAB",                 # LATIN CAPITAL LETTER L WITH MIDDLE TILDE
    "\xE2\xB1\xA3"     => "\xE1\xB5\xBD",             # LATIN CAPITAL LETTER P WITH STROKE
    "\xE2\xB1\xA4"     => "\xC9\xBD",                 # LATIN CAPITAL LETTER R WITH TAIL
    "\xE2\xB1\xA7"     => "\xE2\xB1\xA8",             # LATIN CAPITAL LETTER H WITH DESCENDER
    "\xE2\xB1\xA9"     => "\xE2\xB1\xAA",             # LATIN CAPITAL LETTER K WITH DESCENDER
    "\xE2\xB1\xAB"     => "\xE2\xB1\xAC",             # LATIN CAPITAL LETTER Z WITH DESCENDER
    "\xE2\xB1\xAD"     => "\xC9\x91",                 # LATIN CAPITAL LETTER ALPHA
    "\xE2\xB1\xAE"     => "\xC9\xB1",                 # LATIN CAPITAL LETTER M WITH HOOK
    "\xE2\xB1\xAF"     => "\xC9\x90",                 # LATIN CAPITAL LETTER TURNED A
    "\xE2\xB1\xB0"     => "\xC9\x92",                 # LATIN CAPITAL LETTER TURNED ALPHA
    "\xE2\xB1\xB2"     => "\xE2\xB1\xB3",             # LATIN CAPITAL LETTER W WITH HOOK
    "\xE2\xB1\xB5"     => "\xE2\xB1\xB6",             # LATIN CAPITAL LETTER HALF H
    "\xE2\xB1\xBE"     => "\xC8\xBF",                 # LATIN CAPITAL LETTER S WITH SWASH TAIL
    "\xE2\xB1\xBF"     => "\xC9\x80",                 # LATIN CAPITAL LETTER Z WITH SWASH TAIL
    "\xE2\xB2\x80"     => "\xE2\xB2\x81",             # COPTIC CAPITAL LETTER ALFA
    "\xE2\xB2\x82"     => "\xE2\xB2\x83",             # COPTIC CAPITAL LETTER VIDA
    "\xE2\xB2\x84"     => "\xE2\xB2\x85",             # COPTIC CAPITAL LETTER GAMMA
    "\xE2\xB2\x86"     => "\xE2\xB2\x87",             # COPTIC CAPITAL LETTER DALDA
    "\xE2\xB2\x88"     => "\xE2\xB2\x89",             # COPTIC CAPITAL LETTER EIE
    "\xE2\xB2\x8A"     => "\xE2\xB2\x8B",             # COPTIC CAPITAL LETTER SOU
    "\xE2\xB2\x8C"     => "\xE2\xB2\x8D",             # COPTIC CAPITAL LETTER ZATA
    "\xE2\xB2\x8E"     => "\xE2\xB2\x8F",             # COPTIC CAPITAL LETTER HATE
    "\xE2\xB2\x90"     => "\xE2\xB2\x91",             # COPTIC CAPITAL LETTER THETHE
    "\xE2\xB2\x92"     => "\xE2\xB2\x93",             # COPTIC CAPITAL LETTER IAUDA
    "\xE2\xB2\x94"     => "\xE2\xB2\x95",             # COPTIC CAPITAL LETTER KAPA
    "\xE2\xB2\x96"     => "\xE2\xB2\x97",             # COPTIC CAPITAL LETTER LAULA
    "\xE2\xB2\x98"     => "\xE2\xB2\x99",             # COPTIC CAPITAL LETTER MI
    "\xE2\xB2\x9A"     => "\xE2\xB2\x9B",             # COPTIC CAPITAL LETTER NI
    "\xE2\xB2\x9C"     => "\xE2\xB2\x9D",             # COPTIC CAPITAL LETTER KSI
    "\xE2\xB2\x9E"     => "\xE2\xB2\x9F",             # COPTIC CAPITAL LETTER O
    "\xE2\xB2\xA0"     => "\xE2\xB2\xA1",             # COPTIC CAPITAL LETTER PI
    "\xE2\xB2\xA2"     => "\xE2\xB2\xA3",             # COPTIC CAPITAL LETTER RO
    "\xE2\xB2\xA4"     => "\xE2\xB2\xA5",             # COPTIC CAPITAL LETTER SIMA
    "\xE2\xB2\xA6"     => "\xE2\xB2\xA7",             # COPTIC CAPITAL LETTER TAU
    "\xE2\xB2\xA8"     => "\xE2\xB2\xA9",             # COPTIC CAPITAL LETTER UA
    "\xE2\xB2\xAA"     => "\xE2\xB2\xAB",             # COPTIC CAPITAL LETTER FI
    "\xE2\xB2\xAC"     => "\xE2\xB2\xAD",             # COPTIC CAPITAL LETTER KHI
    "\xE2\xB2\xAE"     => "\xE2\xB2\xAF",             # COPTIC CAPITAL LETTER PSI
    "\xE2\xB2\xB0"     => "\xE2\xB2\xB1",             # COPTIC CAPITAL LETTER OOU
    "\xE2\xB2\xB2"     => "\xE2\xB2\xB3",             # COPTIC CAPITAL LETTER DIALECT-P ALEF
    "\xE2\xB2\xB4"     => "\xE2\xB2\xB5",             # COPTIC CAPITAL LETTER OLD COPTIC AIN
    "\xE2\xB2\xB6"     => "\xE2\xB2\xB7",             # COPTIC CAPITAL LETTER CRYPTOGRAMMIC EIE
    "\xE2\xB2\xB8"     => "\xE2\xB2\xB9",             # COPTIC CAPITAL LETTER DIALECT-P KAPA
    "\xE2\xB2\xBA"     => "\xE2\xB2\xBB",             # COPTIC CAPITAL LETTER DIALECT-P NI
    "\xE2\xB2\xBC"     => "\xE2\xB2\xBD",             # COPTIC CAPITAL LETTER CRYPTOGRAMMIC NI
    "\xE2\xB2\xBE"     => "\xE2\xB2\xBF",             # COPTIC CAPITAL LETTER OLD COPTIC OOU
    "\xE2\xB3\x80"     => "\xE2\xB3\x81",             # COPTIC CAPITAL LETTER SAMPI
    "\xE2\xB3\x82"     => "\xE2\xB3\x83",             # COPTIC CAPITAL LETTER CROSSED SHEI
    "\xE2\xB3\x84"     => "\xE2\xB3\x85",             # COPTIC CAPITAL LETTER OLD COPTIC SHEI
    "\xE2\xB3\x86"     => "\xE2\xB3\x87",             # COPTIC CAPITAL LETTER OLD COPTIC ESH
    "\xE2\xB3\x88"     => "\xE2\xB3\x89",             # COPTIC CAPITAL LETTER AKHMIMIC KHEI
    "\xE2\xB3\x8A"     => "\xE2\xB3\x8B",             # COPTIC CAPITAL LETTER DIALECT-P HORI
    "\xE2\xB3\x8C"     => "\xE2\xB3\x8D",             # COPTIC CAPITAL LETTER OLD COPTIC HORI
    "\xE2\xB3\x8E"     => "\xE2\xB3\x8F",             # COPTIC CAPITAL LETTER OLD COPTIC HA
    "\xE2\xB3\x90"     => "\xE2\xB3\x91",             # COPTIC CAPITAL LETTER L-SHAPED HA
    "\xE2\xB3\x92"     => "\xE2\xB3\x93",             # COPTIC CAPITAL LETTER OLD COPTIC HEI
    "\xE2\xB3\x94"     => "\xE2\xB3\x95",             # COPTIC CAPITAL LETTER OLD COPTIC HAT
    "\xE2\xB3\x96"     => "\xE2\xB3\x97",             # COPTIC CAPITAL LETTER OLD COPTIC GANGIA
    "\xE2\xB3\x98"     => "\xE2\xB3\x99",             # COPTIC CAPITAL LETTER OLD COPTIC DJA
    "\xE2\xB3\x9A"     => "\xE2\xB3\x9B",             # COPTIC CAPITAL LETTER OLD COPTIC SHIMA
    "\xE2\xB3\x9C"     => "\xE2\xB3\x9D",             # COPTIC CAPITAL LETTER OLD NUBIAN SHIMA
    "\xE2\xB3\x9E"     => "\xE2\xB3\x9F",             # COPTIC CAPITAL LETTER OLD NUBIAN NGI
    "\xE2\xB3\xA0"     => "\xE2\xB3\xA1",             # COPTIC CAPITAL LETTER OLD NUBIAN NYI
    "\xE2\xB3\xA2"     => "\xE2\xB3\xA3",             # COPTIC CAPITAL LETTER OLD NUBIAN WAU
    "\xE2\xB3\xAB"     => "\xE2\xB3\xAC",             # COPTIC CAPITAL LETTER CRYPTOGRAMMIC SHEI
    "\xE2\xB3\xAD"     => "\xE2\xB3\xAE",             # COPTIC CAPITAL LETTER CRYPTOGRAMMIC GANGIA
    "\xE2\xB3\xB2"     => "\xE2\xB3\xB3",             # COPTIC CAPITAL LETTER BOHAIRIC KHEI
    "\xEA\x99\x80"     => "\xEA\x99\x81",             # CYRILLIC CAPITAL LETTER ZEMLYA
    "\xEA\x99\x82"     => "\xEA\x99\x83",             # CYRILLIC CAPITAL LETTER DZELO
    "\xEA\x99\x84"     => "\xEA\x99\x85",             # CYRILLIC CAPITAL LETTER REVERSED DZE
    "\xEA\x99\x86"     => "\xEA\x99\x87",             # CYRILLIC CAPITAL LETTER IOTA
    "\xEA\x99\x88"     => "\xEA\x99\x89",             # CYRILLIC CAPITAL LETTER DJERV
    "\xEA\x99\x8A"     => "\xEA\x99\x8B",             # CYRILLIC CAPITAL LETTER MONOGRAPH UK
    "\xEA\x99\x8C"     => "\xEA\x99\x8D",             # CYRILLIC CAPITAL LETTER BROAD OMEGA
    "\xEA\x99\x8E"     => "\xEA\x99\x8F",             # CYRILLIC CAPITAL LETTER NEUTRAL YER
    "\xEA\x99\x90"     => "\xEA\x99\x91",             # CYRILLIC CAPITAL LETTER YERU WITH BACK YER
    "\xEA\x99\x92"     => "\xEA\x99\x93",             # CYRILLIC CAPITAL LETTER IOTIFIED YAT
    "\xEA\x99\x94"     => "\xEA\x99\x95",             # CYRILLIC CAPITAL LETTER REVERSED YU
    "\xEA\x99\x96"     => "\xEA\x99\x97",             # CYRILLIC CAPITAL LETTER IOTIFIED A
    "\xEA\x99\x98"     => "\xEA\x99\x99",             # CYRILLIC CAPITAL LETTER CLOSED LITTLE YUS
    "\xEA\x99\x9A"     => "\xEA\x99\x9B",             # CYRILLIC CAPITAL LETTER BLENDED YUS
    "\xEA\x99\x9C"     => "\xEA\x99\x9D",             # CYRILLIC CAPITAL LETTER IOTIFIED CLOSED LITTLE YUS
    "\xEA\x99\x9E"     => "\xEA\x99\x9F",             # CYRILLIC CAPITAL LETTER YN
    "\xEA\x99\xA0"     => "\xEA\x99\xA1",             # CYRILLIC CAPITAL LETTER REVERSED TSE
    "\xEA\x99\xA2"     => "\xEA\x99\xA3",             # CYRILLIC CAPITAL LETTER SOFT DE
    "\xEA\x99\xA4"     => "\xEA\x99\xA5",             # CYRILLIC CAPITAL LETTER SOFT EL
    "\xEA\x99\xA6"     => "\xEA\x99\xA7",             # CYRILLIC CAPITAL LETTER SOFT EM
    "\xEA\x99\xA8"     => "\xEA\x99\xA9",             # CYRILLIC CAPITAL LETTER MONOCULAR O
    "\xEA\x99\xAA"     => "\xEA\x99\xAB",             # CYRILLIC CAPITAL LETTER BINOCULAR O
    "\xEA\x99\xAC"     => "\xEA\x99\xAD",             # CYRILLIC CAPITAL LETTER DOUBLE MONOCULAR O
    "\xEA\x9A\x80"     => "\xEA\x9A\x81",             # CYRILLIC CAPITAL LETTER DWE
    "\xEA\x9A\x82"     => "\xEA\x9A\x83",             # CYRILLIC CAPITAL LETTER DZWE
    "\xEA\x9A\x84"     => "\xEA\x9A\x85",             # CYRILLIC CAPITAL LETTER ZHWE
    "\xEA\x9A\x86"     => "\xEA\x9A\x87",             # CYRILLIC CAPITAL LETTER CCHE
    "\xEA\x9A\x88"     => "\xEA\x9A\x89",             # CYRILLIC CAPITAL LETTER DZZE
    "\xEA\x9A\x8A"     => "\xEA\x9A\x8B",             # CYRILLIC CAPITAL LETTER TE WITH MIDDLE HOOK
    "\xEA\x9A\x8C"     => "\xEA\x9A\x8D",             # CYRILLIC CAPITAL LETTER TWE
    "\xEA\x9A\x8E"     => "\xEA\x9A\x8F",             # CYRILLIC CAPITAL LETTER TSWE
    "\xEA\x9A\x90"     => "\xEA\x9A\x91",             # CYRILLIC CAPITAL LETTER TSSE
    "\xEA\x9A\x92"     => "\xEA\x9A\x93",             # CYRILLIC CAPITAL LETTER TCHE
    "\xEA\x9A\x94"     => "\xEA\x9A\x95",             # CYRILLIC CAPITAL LETTER HWE
    "\xEA\x9A\x96"     => "\xEA\x9A\x97",             # CYRILLIC CAPITAL LETTER SHWE
    "\xEA\x9C\xA2"     => "\xEA\x9C\xA3",             # LATIN CAPITAL LETTER EGYPTOLOGICAL ALEF
    "\xEA\x9C\xA4"     => "\xEA\x9C\xA5",             # LATIN CAPITAL LETTER EGYPTOLOGICAL AIN
    "\xEA\x9C\xA6"     => "\xEA\x9C\xA7",             # LATIN CAPITAL LETTER HENG
    "\xEA\x9C\xA8"     => "\xEA\x9C\xA9",             # LATIN CAPITAL LETTER TZ
    "\xEA\x9C\xAA"     => "\xEA\x9C\xAB",             # LATIN CAPITAL LETTER TRESILLO
    "\xEA\x9C\xAC"     => "\xEA\x9C\xAD",             # LATIN CAPITAL LETTER CUATRILLO
    "\xEA\x9C\xAE"     => "\xEA\x9C\xAF",             # LATIN CAPITAL LETTER CUATRILLO WITH COMMA
    "\xEA\x9C\xB2"     => "\xEA\x9C\xB3",             # LATIN CAPITAL LETTER AA
    "\xEA\x9C\xB4"     => "\xEA\x9C\xB5",             # LATIN CAPITAL LETTER AO
    "\xEA\x9C\xB6"     => "\xEA\x9C\xB7",             # LATIN CAPITAL LETTER AU
    "\xEA\x9C\xB8"     => "\xEA\x9C\xB9",             # LATIN CAPITAL LETTER AV
    "\xEA\x9C\xBA"     => "\xEA\x9C\xBB",             # LATIN CAPITAL LETTER AV WITH HORIZONTAL BAR
    "\xEA\x9C\xBC"     => "\xEA\x9C\xBD",             # LATIN CAPITAL LETTER AY
    "\xEA\x9C\xBE"     => "\xEA\x9C\xBF",             # LATIN CAPITAL LETTER REVERSED C WITH DOT
    "\xEA\x9D\x80"     => "\xEA\x9D\x81",             # LATIN CAPITAL LETTER K WITH STROKE
    "\xEA\x9D\x82"     => "\xEA\x9D\x83",             # LATIN CAPITAL LETTER K WITH DIAGONAL STROKE
    "\xEA\x9D\x84"     => "\xEA\x9D\x85",             # LATIN CAPITAL LETTER K WITH STROKE AND DIAGONAL STROKE
    "\xEA\x9D\x86"     => "\xEA\x9D\x87",             # LATIN CAPITAL LETTER BROKEN L
    "\xEA\x9D\x88"     => "\xEA\x9D\x89",             # LATIN CAPITAL LETTER L WITH HIGH STROKE
    "\xEA\x9D\x8A"     => "\xEA\x9D\x8B",             # LATIN CAPITAL LETTER O WITH LONG STROKE OVERLAY
    "\xEA\x9D\x8C"     => "\xEA\x9D\x8D",             # LATIN CAPITAL LETTER O WITH LOOP
    "\xEA\x9D\x8E"     => "\xEA\x9D\x8F",             # LATIN CAPITAL LETTER OO
    "\xEA\x9D\x90"     => "\xEA\x9D\x91",             # LATIN CAPITAL LETTER P WITH STROKE THROUGH DESCENDER
    "\xEA\x9D\x92"     => "\xEA\x9D\x93",             # LATIN CAPITAL LETTER P WITH FLOURISH
    "\xEA\x9D\x94"     => "\xEA\x9D\x95",             # LATIN CAPITAL LETTER P WITH SQUIRREL TAIL
    "\xEA\x9D\x96"     => "\xEA\x9D\x97",             # LATIN CAPITAL LETTER Q WITH STROKE THROUGH DESCENDER
    "\xEA\x9D\x98"     => "\xEA\x9D\x99",             # LATIN CAPITAL LETTER Q WITH DIAGONAL STROKE
    "\xEA\x9D\x9A"     => "\xEA\x9D\x9B",             # LATIN CAPITAL LETTER R ROTUNDA
    "\xEA\x9D\x9C"     => "\xEA\x9D\x9D",             # LATIN CAPITAL LETTER RUM ROTUNDA
    "\xEA\x9D\x9E"     => "\xEA\x9D\x9F",             # LATIN CAPITAL LETTER V WITH DIAGONAL STROKE
    "\xEA\x9D\xA0"     => "\xEA\x9D\xA1",             # LATIN CAPITAL LETTER VY
    "\xEA\x9D\xA2"     => "\xEA\x9D\xA3",             # LATIN CAPITAL LETTER VISIGOTHIC Z
    "\xEA\x9D\xA4"     => "\xEA\x9D\xA5",             # LATIN CAPITAL LETTER THORN WITH STROKE
    "\xEA\x9D\xA6"     => "\xEA\x9D\xA7",             # LATIN CAPITAL LETTER THORN WITH STROKE THROUGH DESCENDER
    "\xEA\x9D\xA8"     => "\xEA\x9D\xA9",             # LATIN CAPITAL LETTER VEND
    "\xEA\x9D\xAA"     => "\xEA\x9D\xAB",             # LATIN CAPITAL LETTER ET
    "\xEA\x9D\xAC"     => "\xEA\x9D\xAD",             # LATIN CAPITAL LETTER IS
    "\xEA\x9D\xAE"     => "\xEA\x9D\xAF",             # LATIN CAPITAL LETTER CON
    "\xEA\x9D\xB9"     => "\xEA\x9D\xBA",             # LATIN CAPITAL LETTER INSULAR D
    "\xEA\x9D\xBB"     => "\xEA\x9D\xBC",             # LATIN CAPITAL LETTER INSULAR F
    "\xEA\x9D\xBD"     => "\xE1\xB5\xB9",             # LATIN CAPITAL LETTER INSULAR G
    "\xEA\x9D\xBE"     => "\xEA\x9D\xBF",             # LATIN CAPITAL LETTER TURNED INSULAR G
    "\xEA\x9E\x80"     => "\xEA\x9E\x81",             # LATIN CAPITAL LETTER TURNED L
    "\xEA\x9E\x82"     => "\xEA\x9E\x83",             # LATIN CAPITAL LETTER INSULAR R
    "\xEA\x9E\x84"     => "\xEA\x9E\x85",             # LATIN CAPITAL LETTER INSULAR S
    "\xEA\x9E\x86"     => "\xEA\x9E\x87",             # LATIN CAPITAL LETTER INSULAR T
    "\xEA\x9E\x8B"     => "\xEA\x9E\x8C",             # LATIN CAPITAL LETTER SALTILLO
    "\xEA\x9E\x8D"     => "\xC9\xA5",                 # LATIN CAPITAL LETTER TURNED H
    "\xEA\x9E\x90"     => "\xEA\x9E\x91",             # LATIN CAPITAL LETTER N WITH DESCENDER
    "\xEA\x9E\x92"     => "\xEA\x9E\x93",             # LATIN CAPITAL LETTER C WITH BAR
    "\xEA\x9E\xA0"     => "\xEA\x9E\xA1",             # LATIN CAPITAL LETTER G WITH OBLIQUE STROKE
    "\xEA\x9E\xA2"     => "\xEA\x9E\xA3",             # LATIN CAPITAL LETTER K WITH OBLIQUE STROKE
    "\xEA\x9E\xA4"     => "\xEA\x9E\xA5",             # LATIN CAPITAL LETTER N WITH OBLIQUE STROKE
    "\xEA\x9E\xA6"     => "\xEA\x9E\xA7",             # LATIN CAPITAL LETTER R WITH OBLIQUE STROKE
    "\xEA\x9E\xA8"     => "\xEA\x9E\xA9",             # LATIN CAPITAL LETTER S WITH OBLIQUE STROKE
    "\xEA\x9E\xAA"     => "\xC9\xA6",                 # LATIN CAPITAL LETTER H WITH HOOK
    "\xEF\xAC\x80"     => "\x66\x66",                 # LATIN SMALL LIGATURE FF
    "\xEF\xAC\x81"     => "\x66\x69",                 # LATIN SMALL LIGATURE FI
    "\xEF\xAC\x82"     => "\x66\x6C",                 # LATIN SMALL LIGATURE FL
    "\xEF\xAC\x83"     => "\x66\x66\x69",             # LATIN SMALL LIGATURE FFI
    "\xEF\xAC\x84"     => "\x66\x66\x6C",             # LATIN SMALL LIGATURE FFL
    "\xEF\xAC\x85"     => "\x73\x74",                 # LATIN SMALL LIGATURE LONG S T
    "\xEF\xAC\x86"     => "\x73\x74",                 # LATIN SMALL LIGATURE ST
    "\xEF\xAC\x93"     => "\xD5\xB4\xD5\xB6",         # ARMENIAN SMALL LIGATURE MEN NOW
    "\xEF\xAC\x94"     => "\xD5\xB4\xD5\xA5",         # ARMENIAN SMALL LIGATURE MEN ECH
    "\xEF\xAC\x95"     => "\xD5\xB4\xD5\xAB",         # ARMENIAN SMALL LIGATURE MEN INI
    "\xEF\xAC\x96"     => "\xD5\xBE\xD5\xB6",         # ARMENIAN SMALL LIGATURE VEW NOW
    "\xEF\xAC\x97"     => "\xD5\xB4\xD5\xAD",         # ARMENIAN SMALL LIGATURE MEN XEH
    "\xEF\xBC\xA1"     => "\xEF\xBD\x81",             # FULLWIDTH LATIN CAPITAL LETTER A
    "\xEF\xBC\xA2"     => "\xEF\xBD\x82",             # FULLWIDTH LATIN CAPITAL LETTER B
    "\xEF\xBC\xA3"     => "\xEF\xBD\x83",             # FULLWIDTH LATIN CAPITAL LETTER C
    "\xEF\xBC\xA4"     => "\xEF\xBD\x84",             # FULLWIDTH LATIN CAPITAL LETTER D
    "\xEF\xBC\xA5"     => "\xEF\xBD\x85",             # FULLWIDTH LATIN CAPITAL LETTER E
    "\xEF\xBC\xA6"     => "\xEF\xBD\x86",             # FULLWIDTH LATIN CAPITAL LETTER F
    "\xEF\xBC\xA7"     => "\xEF\xBD\x87",             # FULLWIDTH LATIN CAPITAL LETTER G
    "\xEF\xBC\xA8"     => "\xEF\xBD\x88",             # FULLWIDTH LATIN CAPITAL LETTER H
    "\xEF\xBC\xA9"     => "\xEF\xBD\x89",             # FULLWIDTH LATIN CAPITAL LETTER I
    "\xEF\xBC\xAA"     => "\xEF\xBD\x8A",             # FULLWIDTH LATIN CAPITAL LETTER J
    "\xEF\xBC\xAB"     => "\xEF\xBD\x8B",             # FULLWIDTH LATIN CAPITAL LETTER K
    "\xEF\xBC\xAC"     => "\xEF\xBD\x8C",             # FULLWIDTH LATIN CAPITAL LETTER L
    "\xEF\xBC\xAD"     => "\xEF\xBD\x8D",             # FULLWIDTH LATIN CAPITAL LETTER M
    "\xEF\xBC\xAE"     => "\xEF\xBD\x8E",             # FULLWIDTH LATIN CAPITAL LETTER N
    "\xEF\xBC\xAF"     => "\xEF\xBD\x8F",             # FULLWIDTH LATIN CAPITAL LETTER O
    "\xEF\xBC\xB0"     => "\xEF\xBD\x90",             # FULLWIDTH LATIN CAPITAL LETTER P
    "\xEF\xBC\xB1"     => "\xEF\xBD\x91",             # FULLWIDTH LATIN CAPITAL LETTER Q
    "\xEF\xBC\xB2"     => "\xEF\xBD\x92",             # FULLWIDTH LATIN CAPITAL LETTER R
    "\xEF\xBC\xB3"     => "\xEF\xBD\x93",             # FULLWIDTH LATIN CAPITAL LETTER S
    "\xEF\xBC\xB4"     => "\xEF\xBD\x94",             # FULLWIDTH LATIN CAPITAL LETTER T
    "\xEF\xBC\xB5"     => "\xEF\xBD\x95",             # FULLWIDTH LATIN CAPITAL LETTER U
    "\xEF\xBC\xB6"     => "\xEF\xBD\x96",             # FULLWIDTH LATIN CAPITAL LETTER V
    "\xEF\xBC\xB7"     => "\xEF\xBD\x97",             # FULLWIDTH LATIN CAPITAL LETTER W
    "\xEF\xBC\xB8"     => "\xEF\xBD\x98",             # FULLWIDTH LATIN CAPITAL LETTER X
    "\xEF\xBC\xB9"     => "\xEF\xBD\x99",             # FULLWIDTH LATIN CAPITAL LETTER Y
    "\xEF\xBC\xBA"     => "\xEF\xBD\x9A",             # FULLWIDTH LATIN CAPITAL LETTER Z
    "\xF0\x90\x90\x80" => "\xF0\x90\x90\xA8",         # DESERET CAPITAL LETTER LONG I
    "\xF0\x90\x90\x81" => "\xF0\x90\x90\xA9",         # DESERET CAPITAL LETTER LONG E
    "\xF0\x90\x90\x82" => "\xF0\x90\x90\xAA",         # DESERET CAPITAL LETTER LONG A
    "\xF0\x90\x90\x83" => "\xF0\x90\x90\xAB",         # DESERET CAPITAL LETTER LONG AH
    "\xF0\x90\x90\x84" => "\xF0\x90\x90\xAC",         # DESERET CAPITAL LETTER LONG O
    "\xF0\x90\x90\x85" => "\xF0\x90\x90\xAD",         # DESERET CAPITAL LETTER LONG OO
    "\xF0\x90\x90\x86" => "\xF0\x90\x90\xAE",         # DESERET CAPITAL LETTER SHORT I
    "\xF0\x90\x90\x87" => "\xF0\x90\x90\xAF",         # DESERET CAPITAL LETTER SHORT E
    "\xF0\x90\x90\x88" => "\xF0\x90\x90\xB0",         # DESERET CAPITAL LETTER SHORT A
    "\xF0\x90\x90\x89" => "\xF0\x90\x90\xB1",         # DESERET CAPITAL LETTER SHORT AH
    "\xF0\x90\x90\x8A" => "\xF0\x90\x90\xB2",         # DESERET CAPITAL LETTER SHORT O
    "\xF0\x90\x90\x8B" => "\xF0\x90\x90\xB3",         # DESERET CAPITAL LETTER SHORT OO
    "\xF0\x90\x90\x8C" => "\xF0\x90\x90\xB4",         # DESERET CAPITAL LETTER AY
    "\xF0\x90\x90\x8D" => "\xF0\x90\x90\xB5",         # DESERET CAPITAL LETTER OW
    "\xF0\x90\x90\x8E" => "\xF0\x90\x90\xB6",         # DESERET CAPITAL LETTER WU
    "\xF0\x90\x90\x8F" => "\xF0\x90\x90\xB7",         # DESERET CAPITAL LETTER YEE
    "\xF0\x90\x90\x90" => "\xF0\x90\x90\xB8",         # DESERET CAPITAL LETTER H
    "\xF0\x90\x90\x91" => "\xF0\x90\x90\xB9",         # DESERET CAPITAL LETTER PEE
    "\xF0\x90\x90\x92" => "\xF0\x90\x90\xBA",         # DESERET CAPITAL LETTER BEE
    "\xF0\x90\x90\x93" => "\xF0\x90\x90\xBB",         # DESERET CAPITAL LETTER TEE
    "\xF0\x90\x90\x94" => "\xF0\x90\x90\xBC",         # DESERET CAPITAL LETTER DEE
    "\xF0\x90\x90\x95" => "\xF0\x90\x90\xBD",         # DESERET CAPITAL LETTER CHEE
    "\xF0\x90\x90\x96" => "\xF0\x90\x90\xBE",         # DESERET CAPITAL LETTER JEE
    "\xF0\x90\x90\x97" => "\xF0\x90\x90\xBF",         # DESERET CAPITAL LETTER KAY
    "\xF0\x90\x90\x98" => "\xF0\x90\x91\x80",         # DESERET CAPITAL LETTER GAY
    "\xF0\x90\x90\x99" => "\xF0\x90\x91\x81",         # DESERET CAPITAL LETTER EF
    "\xF0\x90\x90\x9A" => "\xF0\x90\x91\x82",         # DESERET CAPITAL LETTER VEE
    "\xF0\x90\x90\x9B" => "\xF0\x90\x91\x83",         # DESERET CAPITAL LETTER ETH
    "\xF0\x90\x90\x9C" => "\xF0\x90\x91\x84",         # DESERET CAPITAL LETTER THEE
    "\xF0\x90\x90\x9D" => "\xF0\x90\x91\x85",         # DESERET CAPITAL LETTER ES
    "\xF0\x90\x90\x9E" => "\xF0\x90\x91\x86",         # DESERET CAPITAL LETTER ZEE
    "\xF0\x90\x90\x9F" => "\xF0\x90\x91\x87",         # DESERET CAPITAL LETTER ESH
    "\xF0\x90\x90\xA0" => "\xF0\x90\x91\x88",         # DESERET CAPITAL LETTER ZHEE
    "\xF0\x90\x90\xA1" => "\xF0\x90\x91\x89",         # DESERET CAPITAL LETTER ER
    "\xF0\x90\x90\xA2" => "\xF0\x90\x91\x8A",         # DESERET CAPITAL LETTER EL
    "\xF0\x90\x90\xA3" => "\xF0\x90\x91\x8B",         # DESERET CAPITAL LETTER EM
    "\xF0\x90\x90\xA4" => "\xF0\x90\x91\x8C",         # DESERET CAPITAL LETTER EN
    "\xF0\x90\x90\xA5" => "\xF0\x90\x91\x8D",         # DESERET CAPITAL LETTER ENG
    "\xF0\x90\x90\xA6" => "\xF0\x90\x91\x8E",         # DESERET CAPITAL LETTER OI
    "\xF0\x90\x90\xA7" => "\xF0\x90\x91\x8F",         # DESERET CAPITAL LETTER EW
);
my $before_fc = join "\t",               sort keys %fc;
my $after_fc  = join "\t", map {$fc{$_}} sort keys %fc;

if (fc("$before_fc") eq "$after_fc") {
    print qq{ok - 9 fc("\$before_fc") eq "\$after_fc"\n};
}
else {
    print qq{not ok - 9 fc("\$before_fc") eq "\$after_fc"\n};
}

if (fc("$after_fc") eq "$after_fc") {
    print qq{ok - 10 fc("\$after_fc") eq "\$after_fc"\n};
}
else {
    print qq{not ok - 10 fc("\$after_fc") eq "\$after_fc"\n};
}

if (fc("$before_fc") eq fc("$after_fc")) {
    print qq{ok - 11 fc("\$before_fc") eq fc("\$after_fc")\n};
}
else {
    print qq{not ok - 11 fc("\$before_fc") eq fc("\$after_fc")\n};
}

if ("\F$before_fc\E" eq "$after_fc") {
    print qq{ok - 12 "\\F\$before_fc\\E" eq "\$after_fc"\n};
}
else {
    print qq{not ok - 12 "\\F\$before_fc\\E" eq "\$after_fc"\n};
}

if ("\F$after_fc\E" eq "$after_fc") {
    print qq{ok - 13 "\\F\$after_fc\\E" eq "\$after_fc"\n};
}
else {
    print qq{not ok - 13 "\\F\$after_fc\\E" eq "\$after_fc"\n};
}

if ("\F$before_fc\E" eq "\F$after_fc\E") {
    print qq{ok - 14 "\\F\$before_fc\\E" eq "\\F\$after_fc\\E"\n};
}
else {
    print qq{not ok - 14 "\\F\$before_fc\\E" eq "\\F\$after_fc\\E"\n};
}

if ("$after_fc" =~ /\F$before_fc\E/) {
    print qq{ok - 15 "\$after_fc" =~ /\\F\$before_fc\\E/\n};
}
else {
    print qq{not ok - 15 "\$after_fc" =~ /\\F\$before_fc\\E/\n};
}

if ("$after_fc" =~ /\F$after_fc\E/) {
    print qq{ok - 16 "\$after_fc" =~ /\\F\$after_fc\\E/\n};
}
else {
    print qq{not ok - 16 "\$after_fc" =~ /\\F\$after_fc\\E/\n};
}

if ("\F$before_fc\E" =~ /$after_fc/) {
    print qq{ok - 17 "\\F\$before_fc\\E" =~ /\$after_fc/\n};
}
else {
    print qq{not ok - 17 "\\F\$before_fc\\E" =~ /\$after_fc/\n};
}

if ("\F$before_fc\E" =~ /\F$before_fc\E/) {
    print qq{ok - 18 "\\F\$before_fc\\E" =~ /\\F\$before_fc\\E/\n};
}
else {
    print qq{not ok - 18 "\\F\$before_fc\\E" =~ /\\F\$before_fc\\E/\n};
}

if ("\F$before_fc\E" =~ /\F$after_fc\E/) {
    print qq{ok - 19 "\\F\$before_fc\\E" =~ /\\F\$after_fc\\E/\n};
}
else {
    print qq{not ok - 19 "\\F\$before_fc\\E" =~ /\\F\$after_fc\\E/\n};
}

if ("\F$after_fc\E" =~ /$after_fc/) {
    print qq{ok - 20 "\\F\$after_fc\\E" =~ /\$after_fc/\n};
}
else {
    print qq{not ok - 20 "\\F\$after_fc\\E" =~ /\$after_fc/\n};
}

if ("\F$after_fc\E" =~ /\F$before_fc\E/) {
    print qq{ok - 21 "\\F\$after_fc\\E" =~ /\\F\$before_fc\\E/\n};
}
else {
    print qq{not ok - 21 "\\F\$after_fc\\E" =~ /\\F\$before_fc\\E/\n};
}

if ("\F$after_fc\E" =~ /\F$after_fc\E/) {
    print qq{ok - 22 "\\F\$after_fc\\E" =~ /\\F\$after_fc\\E/\n};
}
else {
    print qq{not ok - 22 "\\F\$after_fc\\E" =~ /\\F\$after_fc\\E/\n};
}

if ("$after_fc" =~ /\F$before_fc\E/i) {
    print qq{ok - 23 "\$after_fc" =~ /\\F\$before_fc\\E/i\n};
}
else {
    print qq{not ok - 23 "\$after_fc" =~ /\\F\$before_fc\\E/i\n};
}

if ("$after_fc" =~ /\F$after_fc\E/i) {
    print qq{ok - 24 "\$after_fc" =~ /\\F\$after_fc\\E/i\n};
}
else {
    print qq{not ok - 24 "\$after_fc" =~ /\\F\$after_fc\\E/i\n};
}

if ("\F$before_fc\E" =~ /$after_fc/i) {
    print qq{ok - 25 "\\F\$before_fc\\E" =~ /\$after_fc/i\n};
}
else {
    print qq{not ok - 25 "\\F\$before_fc\\E" =~ /\$after_fc/i\n};
}

if ("\F$before_fc\E" =~ /\F$before_fc\E/i) {
    print qq{ok - 26 "\\F\$before_fc\\E" =~ /\\F\$before_fc\\E/i\n};
}
else {
    print qq{not ok - 26 "\\F\$before_fc\\E" =~ /\\F\$before_fc\\E/i\n};
}

if ("\F$before_fc\E" =~ /\F$after_fc\E/i) {
    print qq{ok - 27 "\\F\$before_fc\\E" =~ /\\F\$after_fc\\E/i\n};
}
else {
    print qq{not ok - 27 "\\F\$before_fc\\E" =~ /\\F\$after_fc\\E/i\n};
}

if ("\F$after_fc\E" =~ /$after_fc/i) {
    print qq{ok - 28 "\\F\$after_fc\\E" =~ /\$after_fc/i\n};
}
else {
    print qq{not ok - 28 "\\F\$after_fc\\E" =~ /\$after_fc/i\n};
}

if ("\F$after_fc\E" =~ /\F$before_fc\E/i) {
    print qq{ok - 29 "\\F\$after_fc\\E" =~ /\\F\$before_fc\\E/i\n};
}
else {
    print qq{not ok - 29 "\\F\$after_fc\\E" =~ /\\F\$before_fc\\E/i\n};
}

if ("\F$after_fc\E" =~ /\F$after_fc\E/i) {
    print qq{ok - 30 "\\F\$after_fc\\E" =~ /\\F\$after_fc\\E/i\n};
}
else {
    print qq{not ok - 30 "\\F\$after_fc\\E" =~ /\\F\$after_fc\\E/i\n};
}

__END__

