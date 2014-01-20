package Char::Eutf2;
######################################################################
#
# Char::Eutf2 - Run-time routines for Char/UTF2.pm
#
# http://search.cpan.org/dist/Char-Char::UTF2/
#
# Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014 INABA Hitoshi <ina@cpan.org>
######################################################################

use 5.00503;    # Galapagos Consensus 1998 for primetools
# use 5.008001; # Lancaster Consensus 2013 for toolchains

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 0.95 $ =~ /(\d+)/xmsg;

BEGIN {
    if ($^X =~ / jperl /oxmsi) {
        die __FILE__, ": needs perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
    if (CORE::ord('A') == 193) {
        die __FILE__, ": is not US-ASCII script (may be EBCDIC or EBCDIK script).";
    }
    if (CORE::ord('A') != 0x41) {
        die __FILE__, ": is not US-ASCII script (must be US-ASCII script).";
    }
}

BEGIN {

    # instead of utf8.pm
    eval q{
        no warnings qw(redefine);
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    };
    if ($@) {
        *utf8::upgrade   = sub { CORE::length $_[0] };
        *utf8::downgrade = sub { 1 };
        *utf8::encode    = sub {   };
        *utf8::decode    = sub { 1 };
        *utf8::is_utf8   = sub {   };
        *utf8::valid     = sub { 1 };
    }
}

# instead of Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;

    sub gensym () {
        my $name = "GEN" . $genseq++;

        # here, no strict qw(refs); if strict.pm exists

        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        return $ref;
    }

    sub qualify ($;$) {
        my ($name) = @_;
        if (!ref($name) && (Char::Eutf2::index($name, '::') == -1) && (Char::Eutf2::index($name, "'") == -1)) {
            my $pkg;
            my %global = map {$_ => 1} qw(ARGV ARGVOUT ENV INC SIG STDERR STDIN STDOUT DATA);

            # Global names: special character, "^xyz", or other.
            if ($name =~ /^(([^\x80-\xFFa-z])|(\^[a-z_]+))\z/i || $global{$name}) {
                # RGS 2001-11-05 : translate leading ^X to control-char
                $name =~ s/^\^([a-z_])/'qq(\c'.$1.')'/eei;
                $pkg = "main";
            }
            else {
                $pkg = (@_ > 1) ? $_[1] : caller;
            }
            $name = $pkg . "::" . $name;
        }
        return $name;
    }

    sub qualify_to_ref ($;$) {

        # here, no strict qw(refs); if strict.pm exists

        return \*{ qualify $_[0], @_ > 1 ? $_[1] : caller };
    }
}

# Column: local $@
# in Chapter 9. Osaete okitai Perl no kiso
# of ISBN 10: 4798119172 | ISBN 13: 978-4798119175 MODAN Perl NYUMON
# (and so on)

# use strict; if strict.pm exists
BEGIN {
    if (eval { local $@; CORE::require strict }) {
        strict::->import;
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# P.863 flock
# in Chapter 27: Functions
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

# instead of Carp.pm
sub carp;
sub croak;
sub cluck;
sub confess;

my $your_char = q{(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[\x00-\x7F\xF5-\xFF]};

# regexp of character
BEGIN { eval q{ use vars qw($q_char) } }
$q_char = qr/$your_char/oxms;

#
# UTF-8 character range per length
#
my %range_tr = ();

#
# alias of encoding name
#
BEGIN { eval q{ use vars qw($encoding_alias) } }

#
# UTF-8 case conversion
#
my %lc = ();
@lc{qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)} =
    qw(a b c d e f g h i j k l m n o p q r s t u v w x y z);
my %uc = ();
@uc{qw(a b c d e f g h i j k l m n o p q r s t u v w x y z)} =
    qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z);
my %fc = ();
@fc{qw(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)} =
    qw(a b c d e f g h i j k l m n o p q r s t u v w x y z);

if (0) {
}

elsif (__PACKAGE__ =~ / \b Eutf2 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F],
               [0xF5..0xFF], # malformed octet
             ],
        2 => [ [0xC2..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xE0],[0xA0..0xBF],[0x80..0xBF],
               [0xE1..0xEC],[0x80..0xBF],[0x80..0xBF],
               [0xED..0xED],[0x80..0x9F],[0x80..0xBF],
               [0xEE..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF0],[0x90..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF1..0xF3],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
               [0xF4..0xF4],[0x80..0x8F],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: utf-8 | utf-8-strict | utf-?2 ) \b /oxmsi;

    # CaseFolding-6.3.0.txt
    # Date: 2012-12-20, 22:14:35 GMT [MD]
    #
    # Unicode Character Database
    # Copyright (c) 1991-2013 Unicode, Inc.
    # For terms of use, see http://www.unicode.org/terms_of_use.html
    # For documentation, see http://www.unicode.org/reports/tr44/

    %fc = (
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
}

else {
    croak "Don't know my package name '@{[__PACKAGE__]}'";
}

#
# @ARGV wildcard globbing
#
sub import {

    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        my @argv = ();
        for (@ARGV) {

            # has space
            if (/\A (?:$q_char)*? [ ] /oxms) {
                if (my @glob = Char::Eutf2::glob(qq{"$_"})) {
                    push @argv, @glob;
                }
                else {
                    push @argv, $_;
                }
            }

            # has wildcard metachar
            elsif (/\A (?:$q_char)*? [*?] /oxms) {
                if (my @glob = Char::Eutf2::glob($_)) {
                    push @argv, @glob;
                }
                else {
                    push @argv, $_;
                }
            }

            # no wildcard globbing
            else {
                push @argv, $_;
            }
        }
        @ARGV = @argv;
    }
}

# P.230 Care with Prototypes
# in Chapter 6: Subroutines
# of ISBN 0-596-00027-8 Programming Perl Third Edition.
#
# If you aren't careful, you can get yourself into trouble with prototypes.
# But if you are careful, you can do a lot of neat things with them. This is
# all very powerful, of course, and should only be used in moderation to make
# the world a better place.

# P.332 Care with Prototypes
# in Chapter 7: Subroutines
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.
#
# If you aren't careful, you can get yourself into trouble with prototypes.
# But if you are careful, you can do a lot of neat things with them. This is
# all very powerful, of course, and should only be used in moderation to make
# the world a better place.

#
# Prototypes of subroutines
#
sub unimport {}
sub Char::Eutf2::split(;$$$);
sub Char::Eutf2::tr($$$$;$);
sub Char::Eutf2::chop(@);
sub Char::Eutf2::index($$;$);
sub Char::Eutf2::rindex($$;$);
sub Char::Eutf2::lcfirst(@);
sub Char::Eutf2::lcfirst_();
sub Char::Eutf2::lc(@);
sub Char::Eutf2::lc_();
sub Char::Eutf2::ucfirst(@);
sub Char::Eutf2::ucfirst_();
sub Char::Eutf2::uc(@);
sub Char::Eutf2::uc_();
sub Char::Eutf2::fc(@);
sub Char::Eutf2::fc_();
sub Char::Eutf2::ignorecase;
sub Char::Eutf2::classic_character_class;
sub Char::Eutf2::capture;
sub Char::Eutf2::chr(;$);
sub Char::Eutf2::chr_();
sub Char::Eutf2::glob($);
sub Char::Eutf2::glob_();

sub Char::UTF2::ord(;$);
sub Char::UTF2::ord_();
sub Char::UTF2::reverse(@);
sub Char::UTF2::getc(;*@);
sub Char::UTF2::length(;$);
sub Char::UTF2::substr($$;$$);
sub Char::UTF2::index($$;$);
sub Char::UTF2::rindex($$;$);

#
# Regexp work
#
BEGIN { eval q{ use vars qw(
    $Char::UTF2::re_a
    $Char::UTF2::re_t
    $Char::UTF2::re_n
    $Char::UTF2::re_r
) } }

#
# Character class
#
BEGIN { eval q{ use vars qw(
    $dot
    $dot_s
    $eD
    $eS
    $eW
    $eH
    $eV
    $eR
    $eN
    $not_alnum
    $not_alpha
    $not_ascii
    $not_blank
    $not_cntrl
    $not_digit
    $not_graph
    $not_lower
    $not_lower_i
    $not_print
    $not_punct
    $not_space
    $not_upper
    $not_upper_i
    $not_word
    $not_xdigit
    $eb
    $eB
) } }

${Char::Eutf2::dot}         = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A])};
${Char::Eutf2::dot_s}       = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])};
${Char::Eutf2::eD}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF0-9])};

# Vertical tabs are now whitespace
# \s in a regex now matches a vertical tab in all circumstances.
# http://search.cpan.org/dist/perl-5.18.0/pod/perldelta.pod#Vertical_tabs_are_now_whitespace
# ${Char::Eutf2::eS}        = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x0A    \x0C\x0D\x20])};
# ${Char::Eutf2::eS}        = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x0A\x0B\x0C\x0D\x20])};
${Char::Eutf2::eS}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\s])};

${Char::Eutf2::eW}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF0-9A-Z_a-z])};
${Char::Eutf2::eH}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x20])};
${Char::Eutf2::eV}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A\x0B\x0C\x0D])};
${Char::Eutf2::eR}          = qr{(?:\x0D\x0A|[\x0A\x0D])};
${Char::Eutf2::eN}          = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A])};
${Char::Eutf2::not_alnum}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x61-\x7A])};
${Char::Eutf2::not_alpha}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x41-\x5A\x61-\x7A])};
${Char::Eutf2::not_ascii}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x00-\x7F])};
${Char::Eutf2::not_blank}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x20])};
${Char::Eutf2::not_cntrl}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x00-\x1F\x7F])};
${Char::Eutf2::not_digit}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39])};
${Char::Eutf2::not_graph}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x21-\x7F])};
${Char::Eutf2::not_lower}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x61-\x7A])};
${Char::Eutf2::not_lower_i} = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])};
${Char::Eutf2::not_print}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x20-\x7F])};
${Char::Eutf2::not_punct}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])};
${Char::Eutf2::not_space}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\s\x0B])};
${Char::Eutf2::not_upper}   = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x41-\x5A])};
${Char::Eutf2::not_upper_i} = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])};
${Char::Eutf2::not_word}    = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x5F\x61-\x7A])};
${Char::Eutf2::not_xdigit}  = qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x46\x61-\x66])};
${Char::Eutf2::eb}          = qr{(?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))};
${Char::Eutf2::eB}          = qr{(?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))};

# avoid: Name "Char::Eutf2::foo" used only once: possible typo at here.
${Char::Eutf2::dot}         = ${Char::Eutf2::dot};
${Char::Eutf2::dot_s}       = ${Char::Eutf2::dot_s};
${Char::Eutf2::eD}          = ${Char::Eutf2::eD};
${Char::Eutf2::eS}          = ${Char::Eutf2::eS};
${Char::Eutf2::eW}          = ${Char::Eutf2::eW};
${Char::Eutf2::eH}          = ${Char::Eutf2::eH};
${Char::Eutf2::eV}          = ${Char::Eutf2::eV};
${Char::Eutf2::eR}          = ${Char::Eutf2::eR};
${Char::Eutf2::eN}          = ${Char::Eutf2::eN};
${Char::Eutf2::not_alnum}   = ${Char::Eutf2::not_alnum};
${Char::Eutf2::not_alpha}   = ${Char::Eutf2::not_alpha};
${Char::Eutf2::not_ascii}   = ${Char::Eutf2::not_ascii};
${Char::Eutf2::not_blank}   = ${Char::Eutf2::not_blank};
${Char::Eutf2::not_cntrl}   = ${Char::Eutf2::not_cntrl};
${Char::Eutf2::not_digit}   = ${Char::Eutf2::not_digit};
${Char::Eutf2::not_graph}   = ${Char::Eutf2::not_graph};
${Char::Eutf2::not_lower}   = ${Char::Eutf2::not_lower};
${Char::Eutf2::not_lower_i} = ${Char::Eutf2::not_lower_i};
${Char::Eutf2::not_print}   = ${Char::Eutf2::not_print};
${Char::Eutf2::not_punct}   = ${Char::Eutf2::not_punct};
${Char::Eutf2::not_space}   = ${Char::Eutf2::not_space};
${Char::Eutf2::not_upper}   = ${Char::Eutf2::not_upper};
${Char::Eutf2::not_upper_i} = ${Char::Eutf2::not_upper_i};
${Char::Eutf2::not_word}    = ${Char::Eutf2::not_word};
${Char::Eutf2::not_xdigit}  = ${Char::Eutf2::not_xdigit};
${Char::Eutf2::eb}          = ${Char::Eutf2::eb};
${Char::Eutf2::eB}          = ${Char::Eutf2::eB};

#
# UTF-8 split
#
sub Char::Eutf2::split(;$$$) {

    # P.794 29.2.161. split
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.951 split
    # in Chapter 27: Functions
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    my $pattern = $_[0];
    my $string  = $_[1];
    my $limit   = $_[2];

    # if $pattern is also omitted or is the literal space, " "
    if (not defined $pattern) {
        $pattern = ' ';
    }

    # if $string is omitted, the function splits the $_ string
    if (not defined $string) {
        if (defined $_) {
            $string = $_;
        }
        else {
            $string = '';
        }
    }

    my @split = ();

    # when string is empty
    if ($string eq '') {

        # resulting list value in list context
        if (wantarray) {
            return @split;
        }

        # count of substrings in scalar context
        else {
            carp "Use of implicit split to \@_ is deprecated" if $^W;
            @_ = @split;
            return scalar @_;
        }
    }

    # split's first argument is more consistently interpreted
    #
    # After some changes earlier in v5.17, split's behavior has been simplified:
    # if the PATTERN argument evaluates to a string containing one space, it is
    # treated the way that a literal string containing one space once was.
    # http://search.cpan.org/dist/perl-5.18.0/pod/perldelta.pod#split's_first_argument_is_more_consistently_interpreted

    # if $pattern is also omitted or is the literal space, " ", the function splits
    # on whitespace, /\s+/, after skipping any leading whitespace
    # (and so on)

    elsif ($pattern eq ' ') {
        if (not defined $limit) {
            return CORE::split(' ', $string);
        }
        else {
            return CORE::split(' ', $string, $limit);
        }
    }

    # if $limit is negative, it is treated as if an arbitrarily large $limit has been specified
    if ((not defined $limit) or ($limit <= 0)) {

        # a pattern capable of matching either the null string or something longer than the
        # null string will split the value of $string into separate characters wherever it
        # matches the null string between characters
        # (and so on)

        if ('' =~ / \A $pattern \z /xms) {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            my $limit = scalar(() = $string =~ /($pattern)/oxmsg);

            # P.1024 Appendix W.10 Multibyte Processing
            # of ISBN 1-56592-224-7 CJKV Information Processing
            # (and so on)

            # the //m modifier is assumed when you split on the pattern /^/
            # (and so on)

            #                                                     V
            while ((--$limit > 0) and ($string =~ s/\A((?:$q_char)+?)$pattern//m)) {

                # if the $pattern contains parentheses, then the substring matched by each pair of parentheses
                # is included in the resulting list, interspersed with the fields that are ordinarily returned
                # (and so on)

                local $@;
                for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                    push @split, eval('$' . $digit);
                }
            }
        }

        else {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);

            #                                 V
            while ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                local $@;
                for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                    push @split, eval('$' . $digit);
                }
            }
        }
    }

    elsif ($limit > 0) {
        if ('' =~ / \A $pattern \z /xms) {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ((--$limit > 0) and (CORE::length($string) > 0)) {

                #                              V
                if ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                    local $@;
                    for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                        push @split, eval('$' . $digit);
                    }
                }
            }
        }
        else {
            my $last_subexpression_offsets = _last_subexpression_offsets($pattern);
            while ((--$limit > 0) and (CORE::length($string) > 0)) {

                #                              V
                if ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                    local $@;
                    for (my $digit=1; $digit <= ($last_subexpression_offsets + 1); $digit++) {
                        push @split, eval('$' . $digit);
                    }
                }
            }
        }
    }

    if (CORE::length($string) > 0) {
        push @split, $string;
    }

    # if $_[2] (NOT "$limit") is omitted or zero, trailing null fields are stripped from the result
    if ((not defined $_[2]) or ($_[2] == 0)) {
        while ((scalar(@split) >= 1) and ($split[-1] eq '')) {
            pop @split;
        }
    }

    # resulting list value in list context
    if (wantarray) {
        return @split;
    }

    # count of substrings in scalar context
    else {
        carp "Use of implicit split to \@_ is deprecated" if $^W;
        @_ = @split;
        return scalar @_;
    }
}

#
# get last subexpression offsets
#
sub _last_subexpression_offsets {
    my $pattern = $_[0];

    # remove comment
    $pattern =~ s/\(\?\# .*? \)//oxmsg;

    my $modifier = '';
    if ($pattern =~ /\(\?\^? ([\-A-Za-z]+) :/oxms) {
        $modifier = $1;
        $modifier =~ s/-[A-Za-z]*//;
    }

    # with /x modifier
    my @char = ();
    if ($modifier =~ /x/oxms) {
        @char = $pattern =~ /\G(
            \\ (?:$q_char)                  |
            \# (?:$q_char)*? $              |
            \[ (?: \\\] | (?:$q_char))+? \] |
            \(\?                            |
            (?:$q_char)
        )/oxmsg;
    }

    # without /x modifier
    else {
        @char = $pattern =~ /\G(
            \\ (?:$q_char)                  |
            \[ (?: \\\] | (?:$q_char))+? \] |
            \(\?                            |
            (?:$q_char)
        )/oxmsg;
    }

    return scalar grep { $_ eq '(' } @char;
}

#
# UTF-8 transliteration (tr///)
#
sub Char::Eutf2::tr($$$$;$) {

    my $bind_operator   = $_[1];
    my $searchlist      = $_[2];
    my $replacementlist = $_[3];
    my $modifier        = $_[4] || '';

    if ($modifier =~ /r/oxms) {
        if ($bind_operator =~ / !~ /oxms) {
            croak "Using !~ with tr///r doesn't make sense";
        }
    }

    my @char            = $_[0] =~ /\G ($q_char) /oxmsg;
    my @searchlist      = _charlist_tr($searchlist);
    my @replacementlist = _charlist_tr($replacementlist);

    my %tr = ();
    for (my $i=0; $i <= $#searchlist; $i++) {
        if (not exists $tr{$searchlist[$i]}) {
            if (defined $replacementlist[$i] and ($replacementlist[$i] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[$i];
            }
            elsif ($modifier =~ /d/oxms) {
                $tr{$searchlist[$i]} = '';
            }
            elsif (defined $replacementlist[-1] and ($replacementlist[-1] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[-1];
            }
            else {
                $tr{$searchlist[$i]} = $searchlist[$i];
            }
        }
    }

    my $tr = 0;
    my $replaced = '';
    if ($modifier =~ /c/oxms) {
        while (defined(my $char = shift @char)) {
            if (not exists $tr{$char}) {
                if (defined $replacementlist[0]) {
                    $replaced .= $replacementlist[0];
                }
                $tr++;
                if ($modifier =~ /s/oxms) {
                    while (@char and (not exists $tr{$char[0]})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }
    else {
        while (defined(my $char = shift @char)) {
            if (exists $tr{$char}) {
                $replaced .= $tr{$char};
                $tr++;
                if ($modifier =~ /s/oxms) {
                    while (@char and (exists $tr{$char[0]}) and ($tr{$char[0]} eq $tr{$char})) {
                        shift @char;
                        $tr++;
                    }
                }
            }
            else {
                $replaced .= $char;
            }
        }
    }

    if ($modifier =~ /r/oxms) {
        return $replaced;
    }
    else {
        $_[0] = $replaced;
        if ($bind_operator =~ / !~ /oxms) {
            return not $tr;
        }
        else {
            return $tr;
        }
    }
}

#
# UTF-8 chop
#
sub Char::Eutf2::chop(@) {

    my $chop;
    if (@_ == 0) {
        my @char = /\G ($q_char) /oxmsg;
        $chop = pop @char;
        $_ = join '', @char;
    }
    else {
        for (@_) {
            my @char = /\G ($q_char) /oxmsg;
            $chop = pop @char;
            $_ = join '', @char;
        }
    }
    return $chop;
}

#
# UTF-8 index by octet
#
sub Char::Eutf2::index($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= 0;
    my $pos = 0;

    while ($pos < CORE::length($str)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            if ($pos >= $position) {
                return $pos;
            }
        }
        if (CORE::substr($str,$pos) =~ /\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return -1;
}

#
# UTF-8 reverse index
#
sub Char::Eutf2::rindex($$;$) {

    my($str,$substr,$position) = @_;
    $position ||= CORE::length($str) - 1;
    my $pos = 0;
    my $rindex = -1;

    while (($pos < CORE::length($str)) and ($pos <= $position)) {
        if (CORE::substr($str,$pos,CORE::length($substr)) eq $substr) {
            $rindex = $pos;
        }
        if (CORE::substr($str,$pos) =~ /\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return $rindex;
}

#
# UTF-8 lower case first with parameter
#
sub Char::Eutf2::lcfirst(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return Char::Eutf2::lc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
        }
        else {
            return Char::Eutf2::lc(CORE::substr($s,0,1)) . CORE::substr($s,1);
        }
    }
    else {
        return Char::Eutf2::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }
}

#
# UTF-8 lower case first without parameter
#
sub Char::Eutf2::lcfirst_() {
    return Char::Eutf2::lc(CORE::substr($_,0,1)) . CORE::substr($_,1);
}

#
# UTF-8 lower case with parameter
#
sub Char::Eutf2::lc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eutf2::lc_();
    }
}

#
# UTF-8 lower case without parameter
#
sub Char::Eutf2::lc_() {
    my $s = $_;
    return join '', map {defined($lc{$_}) ? $lc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# UTF-8 upper case first with parameter
#
sub Char::Eutf2::ucfirst(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return Char::Eutf2::uc(CORE::substr($s,0,1)) . CORE::substr($s,1), @_;
        }
        else {
            return Char::Eutf2::uc(CORE::substr($s,0,1)) . CORE::substr($s,1);
        }
    }
    else {
        return Char::Eutf2::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
    }
}

#
# UTF-8 upper case first without parameter
#
sub Char::Eutf2::ucfirst_() {
    return Char::Eutf2::uc(CORE::substr($_,0,1)) . CORE::substr($_,1);
}

#
# UTF-8 upper case with parameter
#
sub Char::Eutf2::uc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eutf2::uc_();
    }
}

#
# UTF-8 upper case without parameter
#
sub Char::Eutf2::uc_() {
    my $s = $_;
    return join '', map {defined($uc{$_}) ? $uc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# UTF-8 fold case with parameter
#
sub Char::Eutf2::fc(@) {
    if (@_) {
        my $s = shift @_;
        if (@_ and wantarray) {
            return join('', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg)), @_;
        }
        else {
            return join('', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg));
        }
    }
    else {
        return Char::Eutf2::fc_();
    }
}

#
# UTF-8 fold case without parameter
#
sub Char::Eutf2::fc_() {
    my $s = $_;
    return join '', map {defined($fc{$_}) ? $fc{$_} : $_} ($s =~ /\G ($q_char) /oxmsg);
}

#
# UTF-8 regexp capture
#
{
    sub Char::Eutf2::capture {
        return $_[0];
    }
}

#
# UTF-8 regexp ignore case modifier
#
sub Char::Eutf2::ignorecase {

    my @string = @_;
    my $metachar = qr/[\@\\|[\]{]/oxms;

    # ignore case of $scalar or @array
    for my $string (@string) {

        # split regexp
        my @char = $string =~ /\G(
            \[\^ |
                \\? (?:$q_char)
        )/oxmsg;

        # unescape character
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            # open character class [...]
            if ($char[$i] eq '[') {
                my $left = $i;

                # [] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "Unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {
                            if (0) {
                            }

                            elsif ($char =~ /\A [.|)] \z/oxms) {
                                $char = $1 . '\\' . $char;
                            }
                        }

                        # [...]
                        splice @char, $left, $right-$left+1, '(?:' . join('|', @charlist) . ')';

                        $i = $left;
                        last;
                    }
                }
            }

            # open character class [^...]
            elsif ($char[$i] eq '[^') {
                my $left = $i;

                # [^] make die "unmatched [] in regexp ..."

                if ($char[$i+1] eq ']') {
                    $i++;
                }

                while (1) {
                    if (++$i > $#char) {
                        croak "Unmatched [] in regexp";
                    }
                    if ($char[$i] eq ']') {
                        my $right = $i;
                        my @charlist = charlist_not_qr(@char[$left+1..$right-1], 'i');

                        # escape character
                        for my $char (@charlist) {
                            if (0) {
                            }

                            elsif ($char =~ /\A [.|)] \z/oxms) {
                                $char = '\\' . $char;
                            }
                        }

                        # [^...]
                        splice @char, $left, $right-$left+1, '(?!' . join('|', @charlist) . ")(?:$your_char)";

                        $i = $left;
                        last;
                    }
                }
            }

            # rewrite classic character class or escape character
            elsif (my $char = classic_character_class($char[$i])) {
                $char[$i] = $char;
            }

            # with /i modifier
            elsif ($char[$i] =~ /\A [\x00-\xFF] \z/oxms) {
                my $uc = Char::Eutf2::uc($char[$i]);
                my $fc = Char::Eutf2::fc($char[$i]);
                if ($uc ne $fc) {
                    if (CORE::length($fc) == 1) {
                        $char[$i] = '['   . $uc       . $fc . ']';
                    }
                    else {
                        $char[$i] = '(?:' . $uc . '|' . $fc . ')';
                    }
                }
            }
        }

        # characterize
        for (my $i=0; $i <= $#char; $i++) {
            next if not defined $char[$i];

            if (0) {
            }

            # quote character before ? + * {
            elsif (($i >= 1) and ($char[$i] =~ /\A [\?\+\*\{] \z/oxms)) {
                if ($char[$i-1] !~ /\A [\x00-\xFF] \z/oxms) {
                    $char[$i-1] = '(?:' . $char[$i-1] . ')';
                }
            }
        }

        $string = join '', @char;
    }

    # make regexp string
    return @string;
}

#
# classic character class ( \D \S \W \d \s \w \C \X \H \V \h \v \R \N \b \B )
#
sub Char::Eutf2::classic_character_class {
    my($char) = @_;

    return {
        '\D' => '${Char::Eutf2::eD}',
        '\S' => '${Char::Eutf2::eS}',
        '\W' => '${Char::Eutf2::eW}',
        '\d' => '[0-9]',

        # Before Perl 5.6, \s only matched the five whitespace characters
        # tab, newline, form-feed, carriage return, and the space character
        # itself, which, taken together, is the character class [\t\n\f\r ].

        # Vertical tabs are now whitespace
        # \s in a regex now matches a vertical tab in all circumstances.
        # http://search.cpan.org/dist/perl-5.18.0/pod/perldelta.pod#Vertical_tabs_are_now_whitespace
        #            \t  \n  \v  \f  \r space
        # '\s' => '[\x09\x0A    \x0C\x0D\x20]',
        # '\s' => '[\x09\x0A\x0B\x0C\x0D\x20]',
        '\s'   => '\s',

        '\w' => '[0-9A-Z_a-z]',
        '\C' => '[\x00-\xFF]',
        '\X' => 'X',

        # \h \v \H \V

        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        # P.357 13.2.3 Whitespace
        # in Chapter 13: perlrecharclass: Perl Regular Expression Character Classes
        # of ISBN-13: 978-1-906966-02-7 The Perl Language Reference Manual (for Perl version 5.12.1)
        #
        # 0x00009   CHARACTER TABULATION  h s
        # 0x0000a         LINE FEED (LF)   vs
        # 0x0000b        LINE TABULATION   v
        # 0x0000c         FORM FEED (FF)   vs
        # 0x0000d   CARRIAGE RETURN (CR)   vs
        # 0x00020                  SPACE  h s

        # P.196 Table 5-9. Alphanumeric regex metasymbols
        # in Chapter 5. Pattern Matching
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        # (and so on)

        '\H' => '${Char::Eutf2::eH}',
        '\V' => '${Char::Eutf2::eV}',
        '\h' => '[\x09\x20]',
        '\v' => '[\x0A\x0B\x0C\x0D]',
        '\R' => '${Char::Eutf2::eR}',

        # \N
        #
        # http://perldoc.perl.org/perlre.html
        # Character Classes and other Special Escapes
        # Any character but \n (experimental). Not affected by /s modifier

        '\N' => '${Char::Eutf2::eN}',

        # \b \B

        # P.180 Boundaries: The \b and \B Assertions
        # in Chapter 5: Pattern Matching
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        # P.219 Boundaries: The \b and \B Assertions
        # in Chapter 5: Pattern Matching
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        # '\b' => '(?:(?<=\A|\W)(?=\w)|(?<=\w)(?=\W|\z))',
        '\b' => '${Char::Eutf2::eb}',

        # '\B' => '(?:(?<=\w)(?=\w)|(?<=\W)(?=\W))',
        '\B' => '${Char::Eutf2::eB}',

    }->{$char} || '';
}

#
# prepare UTF-8 characters per length
#

# 1 octet characters
my @chars1 = ();
sub chars1 {
    if (@chars1) {
        return @chars1;
    }
    if (exists $range_tr{1}) {
        my @ranges = @{ $range_tr{1} };
        while (my @range = splice(@ranges,0,1)) {
            for my $oct0 (@{$range[0]}) {
                push @chars1, pack 'C', $oct0;
            }
        }
    }
    return @chars1;
}

# 2 octets characters
my @chars2 = ();
sub chars2 {
    if (@chars2) {
        return @chars2;
    }
    if (exists $range_tr{2}) {
        my @ranges = @{ $range_tr{2} };
        while (my @range = splice(@ranges,0,2)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    push @chars2, pack 'CC', $oct0,$oct1;
                }
            }
        }
    }
    return @chars2;
}

# 3 octets characters
my @chars3 = ();
sub chars3 {
    if (@chars3) {
        return @chars3;
    }
    if (exists $range_tr{3}) {
        my @ranges = @{ $range_tr{3} };
        while (my @range = splice(@ranges,0,3)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        push @chars3, pack 'CCC', $oct0,$oct1,$oct2;
                    }
                }
            }
        }
    }
    return @chars3;
}

# 4 octets characters
my @chars4 = ();
sub chars4 {
    if (@chars4) {
        return @chars4;
    }
    if (exists $range_tr{4}) {
        my @ranges = @{ $range_tr{4} };
        while (my @range = splice(@ranges,0,4)) {
            for my $oct0 (@{$range[0]}) {
                for my $oct1 (@{$range[1]}) {
                    for my $oct2 (@{$range[2]}) {
                        for my $oct3 (@{$range[3]}) {
                            push @chars4, pack 'CCCC', $oct0,$oct1,$oct2,$oct3;
                        }
                    }
                }
            }
        }
    }
    return @chars4;
}

#
# UTF-8 open character list for tr
#
sub _charlist_tr {

    local $_ = shift @_;

    # unescape character
    my @char = ();
    while (not /\G \z/oxmsgc) {
        if (/\G (\\0?55|\\x2[Dd]|\\-) /oxmsgc) {
            push @char, '\-';
        }
        elsif (/\G \\ ([0-7]{2,3}) /oxmsgc) {
            push @char, CORE::chr(oct $1);
        }
        elsif (/\G \\x ([0-9A-Fa-f]{1,2}) /oxmsgc) {
            push @char, CORE::chr(hex $1);
        }
        elsif (/\G \\c ([\x40-\x5F]) /oxmsgc) {
            push @char, CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif (/\G (\\ [0nrtfbae]) /oxmsgc) {
            push @char, {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
            }->{$1};
        }
        elsif (/\G \\ ($q_char) /oxmsgc) {
            push @char, $1;
        }
        elsif (/\G ($q_char) /oxmsgc) {
            push @char, $1;
        }
    }

    # join separated multiple-octet
    @char = join('',@char) =~ /\G (\\-|$q_char) /oxmsg;

    # unescape '-'
    my @i = ();
    for my $i (0 .. $#char) {
        if ($char[$i] eq '\-') {
            $char[$i] = '-';
        }
        elsif ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                push @i, $i;
            }
        }
    }

    # open character list (reverse for splice)
    for my $i (CORE::reverse @i) {
        my @range = ();

        # range error
        if ((CORE::length($char[$i-1]) > CORE::length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
            croak "Invalid tr/// range \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
        }

        # range of multiple-octet code
        if (CORE::length($char[$i-1]) == 1) {
            if (CORE::length($char[$i+1]) == 1) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars1();
            }
            elsif (CORE::length($char[$i+1]) == 2) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range, grep {$_ le $char[$i+1]}                           chars2();
            }
            elsif (CORE::length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range,                                                    chars2();
                push @range, grep {$_ le $char[$i+1]}                           chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars1();
                push @range,                                                    chars2();
                push @range,                                                    chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 2) {
            if (CORE::length($char[$i+1]) == 2) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars2();
            }
            elsif (CORE::length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           chars2();
                push @range, grep {$_ le $char[$i+1]}                           chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars2();
                push @range,                                                    chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 3) {
            if (CORE::length($char[$i+1]) == 3) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars3();
            }
            elsif (CORE::length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           chars3();
                push @range, grep {$_ le $char[$i+1]}                           chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        elsif (CORE::length($char[$i-1]) == 4) {
            if (CORE::length($char[$i+1]) == 4) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} chars4();
            }
            else {
                croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
            }
        }
        else {
            croak "Invalid tr/// range (over 4octets) \"\\x" . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]) . '"';
        }

        splice @char, $i-1, 3, @range;
    }

    return @char;
}

#
# UTF-8 open character class
#
sub _cc {
    if (scalar(@_) == 0) {
        die __FILE__, ": subroutine cc got no parameter.";
    }
    elsif (scalar(@_) == 1) {
        return sprintf('\x%02X',$_[0]);
    }
    elsif (scalar(@_) == 2) {
        if ($_[0] > $_[1]) {
            die __FILE__, ": subroutine cc got \$_[0] > \$_[1] parameters).";
        }
        elsif ($_[0] == $_[1]) {
            return sprintf('\x%02X',$_[0]);
        }
        elsif (($_[0]+1) == $_[1]) {
            return sprintf('[\\x%02X\\x%02X]',$_[0],$_[1]);
        }
        else {
            return sprintf('[\\x%02X-\\x%02X]',$_[0],$_[1]);
        }
    }
    else {
        die __FILE__, ": subroutine cc got 3 or more parameters (@{[scalar(@_)]} parameters).";
    }
}

#
# UTF-8 octet range
#
sub _octets {
    my $length = shift @_;

    if ($length == 1) {
        my($a1) = unpack 'C', $_[0];
        my($z1) = unpack 'C', $_[1];

        if ($a1 > $z1) {
            croak 'Invalid [] range in regexp (CORE::ord(A) > CORE::ord(B)) ' . '\x' . unpack('H*',$a1) . '-\x' . unpack('H*',$z1);
        }

        if ($a1 == $z1) {
            return sprintf('\x%02X',$a1);
        }
        elsif (($a1+1) == $z1) {
            return sprintf('\x%02X\x%02X',$a1,$z1);
        }
        else {
            return sprintf('\x%02X-\x%02X',$a1,$z1);
        }
    }
    elsif ($length == 2) {
        my($a1,$a2) = unpack 'CC', $_[0];
        my($z1,$z2) = unpack 'CC', $_[1];
        my($A1,$A2) = unpack 'CC', $_[2];
        my($Z1,$Z2) = unpack 'CC', $_[3];

        if ($a1 == $z1) {
            return (
            #   11111111   222222222222
            #        A          A   Z
                _cc($a1) . _cc($a2,$z2), # a2-z2
            );
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   222222222222
            #        A  Z          A   Z
                _cc($a1)    . _cc($a2,$Z2), # a2-
                _cc(   $z1) . _cc($A2,$z2), #   -z2
            );
        }
        else {
            return (
            #   1111111111111111   222222222222
            #        A     Z            A   Z
                _cc($a1)         . _cc($a2,$Z2), # a2-
                _cc($a1+1,$z1-1) . _cc($A2,$Z2), #   -
                _cc(      $z1)   . _cc($A2,$z2), #   -z2
            );
        }
    }
    elsif ($length == 3) {
        my($a1,$a2,$a3) = unpack 'CCC', $_[0];
        my($z1,$z2,$z3) = unpack 'CCC', $_[1];
        my($A1,$A2,$A3) = unpack 'CCC', $_[2];
        my($Z1,$Z2,$Z3) = unpack 'CCC', $_[3];

        if ($a1 == $z1) {
            if ($a2 == $z2) {
                return (
                #   11111111   22222222   333333333333
                #        A          A          A   Z
                    _cc($a1) . _cc($a2) . _cc($a3,$z3), # a3-z3
                );
            }
            elsif (($a2+1) == $z2) {
                return (
                #   11111111   22222222222   333333333333
                #        A          A  Z          A   Z
                    _cc($a1) . _cc($a2)    . _cc($a3,$Z3), # a3-
                    _cc($a1) . _cc(   $z2) . _cc($A3,$z3), #   -z3
                );
            }
            else {
                return (
                #   11111111   2222222222222222   333333333333
                #        A          A     Z            A   Z
                    _cc($a1) . _cc($a2)         . _cc($a3,$Z3), # a3-
                    _cc($a1) . _cc($a2+1,$z2-1) . _cc($A3,$Z3), #   -
                    _cc($a1) . _cc(      $z2)   . _cc($A3,$z3), #   -z3
                );
            }
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   22222222222222   333333333333
            #        A  Z          A     Z          A   Z
                _cc($a1)    . _cc($a2)       . _cc($a3,$Z3), # a3-
                _cc($a1)    . _cc($a2+1,$Z2) . _cc($A3,$Z3), #   -
                _cc(   $z1) . _cc($A2,$z2-1) . _cc($A3,$Z3), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc($A3,$z3), #   -z3
            );
        }
        else {
            return (
            #   1111111111111111   22222222222222   333333333333
            #        A     Z            A     Z          A   Z
                _cc($a1)         . _cc($a2)       . _cc($a3,$Z3), # a3-
                _cc($a1)         . _cc($a2+1,$Z2) . _cc($A3,$Z3), #   -
                _cc($a1+1,$z1-1) . _cc($A2,$Z2)   . _cc($A3,$Z3), #   -
                _cc(      $z1)   . _cc($A2,$z2-1) . _cc($A3,$Z3), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc($A3,$z3), #   -z3
            );
        }
    }
    elsif ($length == 4) {
        my($a1,$a2,$a3,$a4) = unpack 'CCCC', $_[0];
        my($z1,$z2,$z3,$z4) = unpack 'CCCC', $_[1];
        my($A1,$A2,$A3,$A4) = unpack 'CCCC', $_[0];
        my($Z1,$Z2,$Z3,$Z4) = unpack 'CCCC', $_[1];

        if ($a1 == $z1) {
            if ($a2 == $z2) {
                if ($a3 == $z3) {
                    return (
                    #   11111111   22222222   33333333   444444444444
                    #        A          A          A          A   Z
                        _cc($a1) . _cc($a2) . _cc($a3) . _cc($a4,$z4), # a4-z4
                    );
                }
                elsif (($a3+1) == $z3) {
                    return (
                    #   11111111   22222222   33333333333   444444444444
                    #        A          A          A  Z          A   Z
                        _cc($a1) . _cc($a2) . _cc($a3)    . _cc($a4,$Z4), # a4-
                        _cc($a1) . _cc($a2) . _cc(   $z3) . _cc($A4,$z4), #   -z4
                    );
                }
                else {
                    return (
                    #   11111111   22222222   3333333333333333   444444444444
                    #        A          A          A     Z            A   Z
                        _cc($a1) . _cc($a2) . _cc($a3)         . _cc($a4,$Z4), # a4-
                        _cc($a1) . _cc($a2) . _cc($a3+1,$z3-1) . _cc($A4,$Z4), #   -
                        _cc($a1) . _cc($a2) . _cc(      $z3)   . _cc($A4,$z4), #   -z4
                    );
                }
            }
            elsif (($a2+1) == $z2) {
                return (
                #   11111111   22222222222   33333333333333   444444444444
                #        A          A  Z          A     Z          A   Z
                    _cc($a1) . _cc($a2)    . _cc($a3)       . _cc($a4,$Z4), # a4-
                    _cc($a1) . _cc($a2)    . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(   $z2) . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(   $z2) . _cc(    $z3)   . _cc($A4,$z4), #   -z4
                );
            }
            else {
                return (
                #   11111111   2222222222222222   33333333333333   444444444444
                #        A          A     Z            A     Z          A   Z
                    _cc($a1) . _cc($a2)         . _cc($a3)       . _cc($a4,$Z4), # a4-
                    _cc($a1) . _cc($a2)         . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc($a2+1,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(      $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                    _cc($a1) . _cc(      $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
                );
            }
        }
        elsif (($a1+1) == $z1) {
            return (
            #   11111111111   22222222222222   33333333333333   444444444444
            #        A  Z          A     Z          A     Z          A   Z
                _cc($a1)    . _cc($a2)       . _cc($a3)       . _cc($a4,$Z4), # a4-
                _cc($a1)    . _cc($a2)       . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                _cc($a1)    . _cc($a2+1,$Z2) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc($A2,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                _cc(   $z1) . _cc(    $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
            );
        }
        else {
            return (
            #   1111111111111111   22222222222222   33333333333333   444444444444
            #        A     Z            A     Z          A     Z          A   Z
                _cc($a1)         . _cc($a2)       . _cc($a3)       . _cc($a4,$Z4), # a4-
                _cc($a1)         . _cc($a2)       . _cc($a3+1,$Z3) . _cc($A4,$Z4), #   -
                _cc($a1)         . _cc($a2+1,$Z2) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc($a1+1,$z1-1) . _cc($A2,$Z2)   . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc($A2,$z2-1) . _cc($A3,$Z3)   . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc($A3,$z3-1) . _cc($A4,$Z4), #   -
                _cc(      $z1)   . _cc(    $z2)   . _cc(    $z3)   . _cc($A4,$z4), #   -z4
            );
        }
    }
    else {
        die __FILE__, ": subroutine _octets got invalid length ($length).";
    }
}

#
# UTF-8 range regexp
#
sub _range_regexp {
    my($length,$first,$last) = @_;

    my @range_regexp = ();
    if (not exists $range_tr{$length}) {
        return @range_regexp;
    }

    my @ranges = @{ $range_tr{$length} };
    while (my @range = splice(@ranges,0,$length)) {
        my $min = '';
        my $max = '';
        for (my $i=0; $i < $length; $i++) {
            $min .= pack 'C', $range[$i][0];
            $max .= pack 'C', $range[$i][-1];
        }

# min___max
#            FIRST_____________LAST
#       (nothing)

        if ($max lt $first) {
        }

#            **********
#       min_________max
#            FIRST_____________LAST
#            **********

        elsif (($min le $first) and ($first le $max) and ($max le $last)) {
            push @range_regexp, _octets($length,$first,$max,$min,$max);
        }

#            **********************
#            min________________max
#            FIRST_____________LAST
#            **********************

        elsif (($min eq $first) and ($max eq $last)) {
            push @range_regexp, _octets($length,$first,$last,$min,$max);
        }

#                   *********
#                   min___max
#            FIRST_____________LAST
#                   *********

        elsif (($first le $min) and ($max le $last)) {
            push @range_regexp, _octets($length,$min,$max,$min,$max);
        }

#            **********************
#       min__________________________max
#            FIRST_____________LAST
#            **********************

        elsif (($min le $first) and ($last le $max)) {
            push @range_regexp, _octets($length,$first,$last,$min,$max);
        }

#                         *********
#                         min________max
#            FIRST_____________LAST
#                         *********

        elsif (($first le $min) and ($min le $last) and ($last le $max)) {
            push @range_regexp, _octets($length,$min,$last,$min,$max);
        }

#                                    min___max
#            FIRST_____________LAST
#                              (nothing)

        elsif ($last lt $min) {
        }

        else {
            die __FILE__, ": subroutine _range_regexp panic.";
        }
    }

    return @range_regexp;
}

#
# UTF-8 open character list for qr and not qr
#
sub _charlist {

    my $modifier = pop @_;
    my @char = @_;

    my $ignorecase = ($modifier =~ /i/oxms) ? 1 : 0;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {

        # escape - to ...
        if ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                $char[$i] = '...';
            }
        }

        # octal escape sequence
        elsif ($char[$i] =~ /\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ /\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = hexchr($1);
        }

        # \N{CHARNAME} --> N\{CHARNAME}
        elsif ($char[$i] =~ /\A \\ (N) ( \{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # \p{PROPERTY} --> p\{PROPERTY}
        elsif ($char[$i] =~ /\A \\ (p) ( \{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # \P{PROPERTY} --> P\{PROPERTY}
        elsif ($char[$i] =~ /\A \\ (P) ( \{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1 . '\\' . $2;
        }

        # \p, \P, \X --> p, P, X
        elsif ($char[$i] =~ /\A \\ ( [pPX] ) \z/oxms) {
            $char[$i] = $1;
        }

        elsif ($char[$i] =~ /\A \\ ([0-7]{2,3}) \z/oxms) {
            $char[$i] = CORE::chr oct $1;
        }
        elsif ($char[$i] =~ /\A \\x ([0-9A-Fa-f]{1,2}) \z/oxms) {
            $char[$i] = CORE::chr hex $1;
        }
        elsif ($char[$i] =~ /\A \\c ([\x40-\x5F]) \z/oxms) {
            $char[$i] = CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif ($char[$i] =~ /\A (\\ [0nrtfbaedswDSWHVhvR]) \z/oxms) {
            $char[$i] = {
                '\0' => "\0",
                '\n' => "\n",
                '\r' => "\r",
                '\t' => "\t",
                '\f' => "\f",
                '\b' => "\x08", # \b means backspace in character class
                '\a' => "\a",
                '\e' => "\e",
                '\d' => '[0-9]',

                # Vertical tabs are now whitespace
                # \s in a regex now matches a vertical tab in all circumstances.
                # http://search.cpan.org/dist/perl-5.18.0/pod/perldelta.pod#Vertical_tabs_are_now_whitespace
                #            \t  \n  \v  \f  \r space
                # '\s' => '[\x09\x0A    \x0C\x0D\x20]',
                # '\s' => '[\x09\x0A\x0B\x0C\x0D\x20]',
                '\s'   => '\s',

                '\w' => '[0-9A-Z_a-z]',
                '\D' => '${Char::Eutf2::eD}',
                '\S' => '${Char::Eutf2::eS}',
                '\W' => '${Char::Eutf2::eW}',

                '\H' => '${Char::Eutf2::eH}',
                '\V' => '${Char::Eutf2::eV}',
                '\h' => '[\x09\x20]',
                '\v' => '[\x0A\x0B\x0C\x0D]',
                '\R' => '${Char::Eutf2::eR}',

            }->{$1};
        }

        # POSIX-style character classes
        elsif ($ignorecase and ($char[$i] =~ /\A ( \[\: \^? (?:lower|upper) :\] ) \z/oxms)) {
            $char[$i] = {

                '[:lower:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:upper:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:^lower:]'  => '${Char::Eutf2::not_lower_i}',
                '[:^upper:]'  => '${Char::Eutf2::not_upper_i}',

            }->{$1};
        }
        elsif ($char[$i] =~ /\A ( \[\: \^? (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] ) \z/oxms) {
            $char[$i] = {

                '[:alnum:]'   => '[\x30-\x39\x41-\x5A\x61-\x7A]',
                '[:alpha:]'   => '[\x41-\x5A\x61-\x7A]',
                '[:ascii:]'   => '[\x00-\x7F]',
                '[:blank:]'   => '[\x09\x20]',
                '[:cntrl:]'   => '[\x00-\x1F\x7F]',
                '[:digit:]'   => '[\x30-\x39]',
                '[:graph:]'   => '[\x21-\x7F]',
                '[:lower:]'   => '[\x61-\x7A]',
                '[:print:]'   => '[\x20-\x7F]',
                '[:punct:]'   => '[\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E]',

                # P.174 POSIX-Style Character Classes
                # in Chapter 5: Pattern Matching
                # of ISBN 0-596-00027-8 Programming Perl Third Edition.

                # P.311 11.2.4 Character Classes and other Special Escapes
                # in Chapter 11: perlre: Perl regular expressions
                # of ISBN-13: 978-1-906966-02-7 The Perl Language Reference Manual (for Perl version 5.12.1)

                # P.210 POSIX-Style Character Classes
                # in Chapter 5: Pattern Matching
                # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

                '[:space:]'   => '[\s\x0B]', # "\s" plus vertical tab ("\cK")

                '[:upper:]'   => '[\x41-\x5A]',
                '[:word:]'    => '[\x30-\x39\x41-\x5A\x5F\x61-\x7A]',
                '[:xdigit:]'  => '[\x30-\x39\x41-\x46\x61-\x66]',
                '[:^alnum:]'  => '${Char::Eutf2::not_alnum}',
                '[:^alpha:]'  => '${Char::Eutf2::not_alpha}',
                '[:^ascii:]'  => '${Char::Eutf2::not_ascii}',
                '[:^blank:]'  => '${Char::Eutf2::not_blank}',
                '[:^cntrl:]'  => '${Char::Eutf2::not_cntrl}',
                '[:^digit:]'  => '${Char::Eutf2::not_digit}',
                '[:^graph:]'  => '${Char::Eutf2::not_graph}',
                '[:^lower:]'  => '${Char::Eutf2::not_lower}',
                '[:^print:]'  => '${Char::Eutf2::not_print}',
                '[:^punct:]'  => '${Char::Eutf2::not_punct}',
                '[:^space:]'  => '${Char::Eutf2::not_space}',
                '[:^upper:]'  => '${Char::Eutf2::not_upper}',
                '[:^word:]'   => '${Char::Eutf2::not_word}',
                '[:^xdigit:]' => '${Char::Eutf2::not_xdigit}',

            }->{$1};
        }
        elsif ($char[$i] =~ /\A \\ ($q_char) \z/oxms) {
            $char[$i] = $1;
        }
    }

    # open character list
    my @singleoctet   = ();
    my @multipleoctet = ();
    for (my $i=0; $i <= $#char; ) {

        # escaped -
        if (defined($char[$i+1]) and ($char[$i+1] eq '...')) {
            $i += 1;
            next;
        }

        # make range regexp
        elsif ($char[$i] eq '...') {

            # range error
            if (CORE::length($char[$i-1]) > CORE::length($char[$i+1])) {
                croak 'Invalid [] range in regexp (length(A) > length(B)) ' . '\x' . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]);
            }
            elsif (CORE::length($char[$i-1]) == CORE::length($char[$i+1])) {
                if ($char[$i-1] gt $char[$i+1]) {
                    croak 'Invalid [] range in regexp (CORE::ord(A) > CORE::ord(B)) ' . '\x' . unpack('H*',$char[$i-1]) . '-\x' . unpack('H*',$char[$i+1]);
                }
            }

            # make range regexp per length
            for my $length (CORE::length($char[$i-1]) .. CORE::length($char[$i+1])) {
                my @regexp = ();

                # is first and last
                if (($length == CORE::length($char[$i-1])) and ($length == CORE::length($char[$i+1]))) {
                    push @regexp, _range_regexp($length, $char[$i-1], $char[$i+1]);
                }

                # is first
                elsif ($length == CORE::length($char[$i-1])) {
                    push @regexp, _range_regexp($length, $char[$i-1], "\xFF" x $length);
                }

                # is inside in first and last
                elsif ((CORE::length($char[$i-1]) < $length) and ($length < CORE::length($char[$i+1]))) {
                    push @regexp, _range_regexp($length, "\x00" x $length, "\xFF" x $length);
                }

                # is last
                elsif ($length == CORE::length($char[$i+1])) {
                    push @regexp, _range_regexp($length, "\x00" x $length, $char[$i+1]);
                }

                else {
                    die __FILE__, ": subroutine make_regexp panic.";
                }

                if ($length == 1) {
                    push @singleoctet, @regexp;
                }
                else {
                    push @multipleoctet, @regexp;
                }
            }

            $i += 2;
        }

        # with /i modifier
        elsif ($char[$i] =~ /\A [\x00-\xFF] \z/oxms) {
            if ($modifier =~ /i/oxms) {
                my $uc = Char::Eutf2::uc($char[$i]);
                my $fc = Char::Eutf2::fc($char[$i]);
                if ($uc ne $fc) {
                    if (CORE::length($fc) == 1) {
                        push @singleoctet, $uc, $fc;
                    }
                    else {
                        push @singleoctet,   $uc;
                        push @multipleoctet, $fc;
                    }
                }
                else {
                    push @singleoctet, $char[$i];
                }
            }
            else {
                push @singleoctet, $char[$i];
            }
            $i += 1;
        }

        # single character of single octet code
        elsif ($char[$i] =~ /\A (?: \\h ) \z/oxms) {
            push @singleoctet, "\t", "\x20";
            $i += 1;
        }
        elsif ($char[$i] =~ /\A (?: \\v ) \z/oxms) {
            push @singleoctet, "\x0A", "\x0B", "\x0C", "\x0D";
            $i += 1;
        }
        elsif ($char[$i] =~ /\A (?: \\d | \\s | \\w ) \z/oxms) {
            push @singleoctet, $char[$i];
            $i += 1;
        }

        # single character of multiple-octet code
        else {
            push @multipleoctet, $char[$i];
            $i += 1;
        }
    }

    # quote metachar
    for (@singleoctet) {
        if ($_ eq '...') {
            $_ = '-';
        }
        elsif (/\A \n \z/oxms) {
            $_ = '\n';
        }
        elsif (/\A \r \z/oxms) {
            $_ = '\r';
        }
        elsif (/\A ([\x00-\x20\x7F-\xFF]) \z/oxms) {
            $_ = sprintf('\x%02X', CORE::ord $1);
        }
        elsif (/\A [\x00-\xFF] \z/oxms) {
            $_ = quotemeta $_;
        }
    }

    # return character list
    return \@singleoctet, \@multipleoctet;
}

#
# UTF-8 octal escape sequence
#
sub octchr {
    my($octdigit) = @_;

    my @binary = ();
    for my $octal (split(//,$octdigit)) {
        push @binary, {
            '0' => '000',
            '1' => '001',
            '2' => '010',
            '3' => '011',
            '4' => '100',
            '5' => '101',
            '6' => '110',
            '7' => '111',
        }->{$octal};
    }
    my $binary = join '', @binary;

    my $octchr = {
        #                1234567
        1 => pack('B*', "0000000$binary"),
        2 => pack('B*', "000000$binary"),
        3 => pack('B*', "00000$binary"),
        4 => pack('B*', "0000$binary"),
        5 => pack('B*', "000$binary"),
        6 => pack('B*', "00$binary"),
        7 => pack('B*', "0$binary"),
        0 => pack('B*', "$binary"),

    }->{CORE::length($binary) % 8};

    return $octchr;
}

#
# UTF-8 hexadecimal escape sequence
#
sub hexchr {
    my($hexdigit) = @_;

    my $hexchr = {
        1 => pack('H*', "0$hexdigit"),
        0 => pack('H*', "$hexdigit"),

    }->{CORE::length($_[0]) % 2};

    return $hexchr;
}

#
# UTF-8 open character list for qr
#
sub charlist_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $multipleoctet) = _charlist(@char, $modifier);
    my @singleoctet   = @$singleoctet;
    my @multipleoctet = @$multipleoctet;

    # return character list
    if (scalar(@singleoctet) >= 1) {

        # with /i modifier
        if ($modifier =~ m/i/oxms) {
            my %singleoctet_ignorecase = ();
            for (@singleoctet) {
                while (s/ \A \\x(..) - \\x(..) //oxms or s/ \A \\x((..)) //oxms) {
                    for my $ord (hex($1) .. hex($2)) {
                        my $char = CORE::chr($ord);
                        my $uc = Char::Eutf2::uc($char);
                        my $fc = Char::Eutf2::fc($char);
                        if ($uc eq $fc) {
                            $singleoctet_ignorecase{unpack 'C*', $char} = 1;
                        }
                        else {
                            if (CORE::length($fc) == 1) {
                                $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                                $singleoctet_ignorecase{unpack 'C*', $fc} = 1;
                            }
                            else {
                                $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                                push @multipleoctet, join '', map {sprintf('\x%02X',$_)} unpack 'C*', $fc;
                            }
                        }
                    }
                }
            }
            my $i = 0;
            my @singleoctet_ignorecase = ();
            for my $ord (0 .. 255) {
                if (exists $singleoctet_ignorecase{$ord}) {
                    push @{$singleoctet_ignorecase[$i]}, $ord;
                }
                else {
                    $i++;
                }
            }
            @singleoctet = ();
            for my $range (@singleoctet_ignorecase) {
                if (ref $range) {
                    if (scalar(@{$range}) == 1) {
                        push @singleoctet, sprintf('\x%02X', @{$range}[0]);
                    }
                    elsif (scalar(@{$range}) == 2) {
                        push @singleoctet, sprintf('\x%02X\x%02X', @{$range}[0], @{$range}[-1]);
                    }
                    else {
                        push @singleoctet, sprintf('\x%02X-\x%02X', @{$range}[0], @{$range}[-1]);
                    }
                }
            }
        }

        my $not_anchor = '';
        $not_anchor = '(?!(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF]))';

        push @multipleoctet, join('', $not_anchor, '[', @singleoctet, ']' );
    }
    if (scalar(@multipleoctet) >= 2) {
        return '(?:' . join('|', @multipleoctet) . ')';
    }
    else {
        return $multipleoctet[0];
    }
}

#
# UTF-8 open character list for not qr
#
sub charlist_not_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $multipleoctet) = _charlist(@char, $modifier);
    my @singleoctet   = @$singleoctet;
    my @multipleoctet = @$multipleoctet;

    # with /i modifier
    if ($modifier =~ m/i/oxms) {
        my %singleoctet_ignorecase = ();
        for (@singleoctet) {
            while (s/ \A \\x(..) - \\x(..) //oxms or s/ \A \\x((..)) //oxms) {
                for my $ord (hex($1) .. hex($2)) {
                    my $char = CORE::chr($ord);
                    my $uc = Char::Eutf2::uc($char);
                    my $fc = Char::Eutf2::fc($char);
                    if ($uc eq $fc) {
                        $singleoctet_ignorecase{unpack 'C*', $char} = 1;
                    }
                    else {
                        if (CORE::length($fc) == 1) {
                            $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                            $singleoctet_ignorecase{unpack 'C*', $fc} = 1;
                        }
                        else {
                            $singleoctet_ignorecase{unpack 'C*', $uc} = 1;
                            push @multipleoctet, join '', map {sprintf('\x%02X',$_)} unpack 'C*', $fc;
                        }
                    }
                }
            }
        }
        my $i = 0;
        my @singleoctet_ignorecase = ();
        for my $ord (0 .. 255) {
            if (exists $singleoctet_ignorecase{$ord}) {
                push @{$singleoctet_ignorecase[$i]}, $ord;
            }
            else {
                $i++;
            }
        }
        @singleoctet = ();
        for my $range (@singleoctet_ignorecase) {
            if (ref $range) {
                if (scalar(@{$range}) == 1) {
                    push @singleoctet, sprintf('\x%02X', @{$range}[0]);
                }
                elsif (scalar(@{$range}) == 2) {
                    push @singleoctet, sprintf('\x%02X\x%02X', @{$range}[0], @{$range}[-1]);
                }
                else {
                    push @singleoctet, sprintf('\x%02X-\x%02X', @{$range}[0], @{$range}[-1]);
                }
            }
        }
    }

    # return character list
    if (scalar(@multipleoctet) >= 1) {
        if (scalar(@singleoctet) >= 1) {

            # any character other than multiple-octet and single octet character class
            return '(?!' . join('|', @multipleoctet) . ')(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF' . join('', @singleoctet) . '])';
        }
        else {

            # any character other than multiple-octet character class
            return '(?!' . join('|', @multipleoctet) . ")(?:$your_char)";
        }
    }
    else {
        if (scalar(@singleoctet) >= 1) {

            # any character other than single octet character class
            return                                      '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF' . join('', @singleoctet) . '])';
        }
        else {

            # any character
            return                                      "(?:$your_char)";
        }
    }
}

#
# open file in read mode
#
sub _open_r {
    my(undef,$file) = @_;
    $file =~ s#\A (\s) #./$1#oxms;
    return eval(q{open($_[0],'<',$_[1])}) ||
                  open($_[0],"< $file\0");
}

#
# open file in write mode
#
sub _open_w {
    my(undef,$file) = @_;
    $file =~ s#\A (\s) #./$1#oxms;
    return eval(q{open($_[0],'>',$_[1])}) ||
                  open($_[0],"> $file\0");
}

#
# open file in append mode
#
sub _open_a {
    my(undef,$file) = @_;
    $file =~ s#\A (\s) #./$1#oxms;
    return eval(q{open($_[0],'>>',$_[1])}) ||
                  open($_[0],">> $file\0");
}

#
# safe system
#
sub _systemx {

    # P.707 29.2.33. exec
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.
    #
    # Be aware that in older releases of Perl, exec (and system) did not flush
    # your output buffer, so you needed to enable command buffering by setting $|
    # on one or more filehandles to avoid lost output in the case of exec, or
    # misordererd output in the case of system. This situation was largely remedied
    # in the 5.6 release of Perl. (So, 5.005 release not yet.)

    # P.855 exec
    # in Chapter 27: Functions
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.
    #
    # In very old release of Perl (before v5.6), exec (and system) did not flush
    # your output buffer, so you needed to enable command buffering by setting $|
    # on one or more filehandles to avoid lost output with exec or misordered
    # output with system.

    $| = 1;

    # P.565 23.1.2. Cleaning Up Your Environment
    # in Chapter 23: Security
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.656 Cleaning Up Your Environment
    # in Chapter 20: Security
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    # local $ENV{'PATH'} = '.';
    local @ENV{qw(IFS CDPATH ENV BASH_ENV)}; # Make %ENV safer

    # P.707 29.2.33. exec
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.
    #
    # As we mentioned earlier, exec treats a discrete list of arguments as an
    # indication that it should bypass shell processing. However, there is one
    # place where you might still get tripped up. The exec call (and system, too)
    # will not distinguish between a single scalar argument and an array containing
    # only one element.
    #
    #     @args = ("echo surprise");  # just one element in list
    #     exec @args                  # still subject to shell escapes
    #         or die "exec: $!";      #   because @args == 1
    #
    # To avoid this, you can use the PATHNAME syntax, explicitly duplicating the
    # first argument as the pathname, which forces the rest of the arguments to be
    # interpreted as a list, even if there is only one of them:
    #
    #     exec { $args[0] } @args  # safe even with one-argument list
    #         or die "can't exec @args: $!";

    # P.855 exec
    # in Chapter 27: Functions
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.
    #
    # As we mentioned earlier, exec treats a discrete list of arguments as a
    # directive to bypass shell processing. However, there is one place where
    # you might still get tripped up. The exec call (and system, too) cannot
    # distinguish between a single scalar argument and an array containing
    # only one element.
    #
    #     @args = ("echo surprise");  # just one element in list
    #     exec @args                  # still subject to shell escapes
    #         || die "exec: $!";      #   because @args == 1
    #
    # To avoid this, use the PATHNAME syntax, explicitly duplicating the first
    # argument as the pathname, which forces the rest of the arguments to be
    # interpreted as a list, even if there is only one of them:
    #
    #     exec { $args[0] } @args  # safe even with one-argument list
    #         || die "can't exec @args: $!";

    return CORE::system { $_[0] } @_; # safe even with one-argument list
}

#
# UTF-8 order to character (with parameter)
#
sub Char::Eutf2::chr(;$) {

    my $c = @_ ? $_[0] : $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# UTF-8 order to character (without parameter)
#
sub Char::Eutf2::chr_() {

    my $c = $_;

    if ($c == 0x00) {
        return "\x00";
    }
    else {
        my @chr = ();
        while ($c > 0) {
            unshift @chr, ($c % 0x100);
            $c = int($c / 0x100);
        }
        return pack 'C*', @chr;
    }
}

#
# UTF-8 path globbing (with parameter)
#
sub Char::Eutf2::glob($) {

    if (wantarray) {
        my @glob = _DOS_like_glob(@_);
        for my $glob (@glob) {
            $glob =~ s{ \A (?:\./)+ }{}oxms;
        }
        return @glob;
    }
    else {
        my $glob = _DOS_like_glob(@_);
        $glob =~ s{ \A (?:\./)+ }{}oxms;
        return $glob;
    }
}

#
# UTF-8 path globbing (without parameter)
#
sub Char::Eutf2::glob_() {

    if (wantarray) {
        my @glob = _DOS_like_glob();
        for my $glob (@glob) {
            $glob =~ s{ \A (?:\./)+ }{}oxms;
        }
        return @glob;
    }
    else {
        my $glob = _DOS_like_glob();
        $glob =~ s{ \A (?:\./)+ }{}oxms;
        return $glob;
    }
}

#
# UTF-8 path globbing via File::DosGlob 1.10
#
# Often I confuse "_dosglob" and "_doglob".
# So, I renamed "_dosglob" to "_DOS_like_glob".
#
my %iter;
my %entries;
sub _DOS_like_glob {

    # context (keyed by second cxix argument provided by core)
    my($expr,$cxix) = @_;

    # glob without args defaults to $_
    $expr = $_ if not defined $expr;

    # represents the current user's home directory
    #
    # 7.3. Expanding Tildes in Filenames
    # in Chapter 7. File Access
    # of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
    #
    # and File::HomeDir, File::HomeDir::Windows module

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $expr =~ s{ \A ~ (?= [^/\\] ) }
                  { my_home_MSWin32() }oxmse;
    }

    # UNIX-like system
    else {
        $expr =~ s{ \A ~ ( (?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF/])* ) }
                  { $1 ? (eval(q{(getpwnam($1))[7]})||my_home()) : my_home() }oxmse;
    }

    # assume global context if not provided one
    $cxix = '_G_' if not defined $cxix;
    $iter{$cxix} = 0 if not exists $iter{$cxix};

    # if we're just beginning, do it all first
    if ($iter{$cxix} == 0) {
            $entries{$cxix} = [ _do_glob(1, _parse_line($expr)) ];
    }

    # chuck it all out, quick or slow
    if (wantarray) {
        delete $iter{$cxix};
        return @{delete $entries{$cxix}};
    }
    else {
        if ($iter{$cxix} = scalar @{$entries{$cxix}}) {
            return shift @{$entries{$cxix}};
        }
        else {
            # return undef for EOL
            delete $iter{$cxix};
            delete $entries{$cxix};
            return undef;
        }
    }
}

#
# UTF-8 path globbing subroutine
#
sub _do_glob {

    my($cond,@expr) = @_;
    my @glob = ();
    my $fix_drive_relative_paths = 0;

OUTER:
    for my $expr (@expr) {
        next OUTER if not defined $expr;
        next OUTER if $expr eq '';

        my @matched = ();
        my @globdir = ();
        my $head    = '.';
        my $pathsep = '/';
        my $tail;

        # if argument is within quotes strip em and do no globbing
        if ($expr =~ /\A " ((?:$q_char)*) " \z/oxms) {
            $expr = $1;
            if ($cond eq 'd') {
                if (-d $expr) {
                    push @glob, $expr;
                }
            }
            else {
                if (-e $expr) {
                    push @glob, $expr;
                }
            }
            next OUTER;
        }

        # wildcards with a drive prefix such as h:*.pm must be changed
        # to h:./*.pm to expand correctly
        if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
            if ($expr =~ s# \A ((?:[A-Za-z]:)?) ((?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF/\\]) #$1./$2#oxms) {
                $fix_drive_relative_paths = 1;
            }
        }

        if (($head, $tail) = _parse_path($expr,$pathsep)) {
            if ($tail eq '') {
                push @glob, $expr;
                next OUTER;
            }
            if ($head =~ / \A (?:$q_char)*? [*?] /oxms) {
                if (@globdir = _do_glob('d', $head)) {
                    push @glob, _do_glob($cond, map {"$_$pathsep$tail"} @globdir);
                    next OUTER;
                }
            }
            if ($head eq '' or $head =~ /\A [A-Za-z]: \z/oxms) {
                $head .= $pathsep;
            }
            $expr = $tail;
        }

        # If file component has no wildcards, we can avoid opendir
        if ($expr !~ / \A (?:$q_char)*? [*?] /oxms) {
            if ($head eq '.') {
                $head = '';
            }
            if ($head ne '' and ($head =~ / \G ($q_char) /oxmsg)[-1] ne $pathsep) {
                $head .= $pathsep;
            }
            $head .= $expr;
            if ($cond eq 'd') {
                if (-d $head) {
                    push @glob, $head;
                }
            }
            else {
                if (-e $head) {
                    push @glob, $head;
                }
            }
            next OUTER;
        }
        opendir(*DIR, $head) or next OUTER;
        my @leaf = readdir DIR;
        closedir DIR;

        if ($head eq '.') {
            $head = '';
        }
        if ($head ne '' and ($head =~ / \G ($q_char) /oxmsg)[-1] ne $pathsep) {
            $head .= $pathsep;
        }

        my $pattern = '';
        while ($expr =~ / \G ($q_char) /oxgc) {
            my $char = $1;
            if ($char eq '*') {
                $pattern .= "(?:$your_char)*",
            }
            elsif ($char eq '?') {
                $pattern .= "(?:$your_char)?",  # DOS style
#               $pattern .= "(?:$your_char)",   # UNIX style
            }
            elsif ((my $fc = Char::Eutf2::fc($char)) ne $char) {
                $pattern .= $fc;
            }
            else {
                $pattern .= quotemeta $char;
            }
        }
        my $matchsub = sub { Char::Eutf2::fc($_[0]) =~ /\A $pattern \z/xms };

#       if ($@) {
#           print STDERR "$0: $@\n";
#           next OUTER;
#       }

INNER:
        for my $leaf (@leaf) {
            if ($leaf eq '.' or $leaf eq '..') {
                next INNER;
            }
            if ($cond eq 'd' and not -d "$head$leaf") {
                next INNER;
            }

            if (&$matchsub($leaf)) {
                push @matched, "$head$leaf";
                next INNER;
            }

            # [DOS compatibility special case]
            # Failed, add a trailing dot and try again, but only...

            if (Char::Eutf2::index($leaf,'.') == -1 and   # if name does not have a dot in it *and*
                CORE::length($leaf) <= 8 and        # name is shorter than or equal to 8 chars *and*
                Char::Eutf2::index($pattern,'\\.') != -1  # pattern has a dot.
            ) {
                if (&$matchsub("$leaf.")) {
                    push @matched, "$head$leaf";
                    next INNER;
                }
            }
        }
        if (@matched) {
            push @glob, @matched;
        }
    }
    if ($fix_drive_relative_paths) {
        for my $glob (@glob) {
            $glob =~ s# \A ([A-Za-z]:) \./ #$1#oxms;
        }
    }
    return @glob;
}

#
# UTF-8 parse line
#
sub _parse_line {

    my($line) = @_;

    $line .= ' ';
    my @piece = ();
    while ($line =~ /
        " ( (?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF"]   )*  ) " \s+ |
          ( (?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF"\s] )*  )   \s+
        /oxmsg
    ) {
        push @piece, defined($1) ? $1 : $2;
    }
    return @piece;
}

#
# UTF-8 parse path
#
sub _parse_path {

    my($path,$pathsep) = @_;

    $path .= '/';
    my @subpath = ();
    while ($path =~ /
        ((?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\/\\] )+?) [\/\\]
        /oxmsg
    ) {
        push @subpath, $1;
    }

    my $tail = pop @subpath;
    my $head = join $pathsep, @subpath;
    return $head, $tail;
}

#
# via File::HomeDir::Windows 1.00
#
sub my_home_MSWin32 {

    # A lot of unix people and unix-derived tools rely on
    # the ability to overload HOME. We will support it too
    # so that they can replace raw HOME calls with File::HomeDir.
    if (exists $ENV{'HOME'} and $ENV{'HOME'}) {
        return $ENV{'HOME'};
    }

    # Do we have a user profile?
    elsif (exists $ENV{'USERPROFILE'} and $ENV{'USERPROFILE'}) {
        return $ENV{'USERPROFILE'};
    }

    # Some Windows use something like $ENV{'HOME'}
    elsif (exists $ENV{'HOMEDRIVE'} and exists $ENV{'HOMEPATH'} and $ENV{'HOMEDRIVE'} and $ENV{'HOMEPATH'}) {
        return join '', $ENV{'HOMEDRIVE'}, $ENV{'HOMEPATH'};
    }

    return undef;
}

#
# via File::HomeDir::Unix 1.00
#
sub my_home {
    my $home;

    if (exists $ENV{'HOME'} and defined $ENV{'HOME'}) {
        $home = $ENV{'HOME'};
    }

    # This is from the original code, but I'm guessing
    # it means "login directory" and exists on some Unixes.
    elsif (exists $ENV{'LOGDIR'} and $ENV{'LOGDIR'}) {
        $home = $ENV{'LOGDIR'};
    }

    ### More-desperate methods

    # Light desperation on any (Unixish) platform
    else {
        $home = eval q{ (getpwuid($<))[7] };
    }

    # On Unix in general, a non-existant home means "no home"
    # For example, "nobody"-like users might use /nonexistant
    if (defined $home and ! -d($home)) {
        $home = undef;
    }
    return $home;
}

#
# ${^PREMATCH}, $PREMATCH, $` the string preceding what was matched
#
sub Char::Eutf2::PREMATCH {
    return $`;
}

#
# ${^MATCH}, $MATCH, $& the string that matched
#
sub Char::Eutf2::MATCH {
    return $&;
}

#
# ${^POSTMATCH}, $POSTMATCH, $' the string following what was matched
#
sub Char::Eutf2::POSTMATCH {
    return $';
}

#
# UTF-8 character to order (with parameter)
#
sub Char::UTF2::ord(;$) {

    local $_ = shift if @_;

    if (/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# UTF-8 character to order (without parameter)
#
sub Char::UTF2::ord_() {

    if (/\A ($q_char) /oxms) {
        my @ord = unpack 'C*', $1;
        my $ord = 0;
        while (my $o = shift @ord) {
            $ord = $ord * 0x100 + $o;
        }
        return $ord;
    }
    else {
        return CORE::ord $_;
    }
}

#
# UTF-8 reverse
#
sub Char::UTF2::reverse(@) {

    if (wantarray) {
        return CORE::reverse @_;
    }
    else {

        # One of us once cornered Larry in an elevator and asked him what
        # problem he was solving with this, but he looked as far off into
        # the distance as he could in an elevator and said, "It seemed like
        # a good idea at the time."

        return join '', CORE::reverse(join('',@_) =~ /\G ($q_char) /oxmsg);
    }
}

#
# UTF-8 getc (with parameter, without parameter)
#
sub Char::UTF2::getc(;*@) {

    my($package) = caller;
    my $fh = @_ ? qualify_to_ref(shift,$package) : \*STDIN;
    croak 'Too many arguments for Char::UTF2::getc' if @_ and not wantarray;

    my @length = sort { $a <=> $b } keys %range_tr;
    my $getc = '';
    for my $length ($length[0] .. $length[-1]) {
        $getc .= CORE::getc($fh);
        if (exists $range_tr{CORE::length($getc)}) {
            if ($getc =~ /\A ${Char::Eutf2::dot_s} \z/oxms) {
                return wantarray ? ($getc,@_) : $getc;
            }
        }
    }
    return wantarray ? ($getc,@_) : $getc;
}

#
# UTF-8 length by character
#
sub Char::UTF2::length(;$) {

    local $_ = shift if @_;

    local @_ = /\G ($q_char) /oxmsg;
    return scalar @_;
}

#
# UTF-8 substr by character
#
BEGIN {

    # P.232 The lvalue Attribute
    # in Chapter 6: Subroutines
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.336 The lvalue Attribute
    # in Chapter 7: Subroutines
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    # P.144 8.4 Lvalue subroutines
    # in Chapter 8: perlsub: Perl subroutines
    # of ISBN-13: 978-1-906966-02-7 The Perl Language Reference Manual (for Perl version 5.12.1)

    eval sprintf(<<'END', ($] >= 5.014000) ? ':lvalue' : '');
    #                       vv----------------*******
    sub Char::UTF2::substr($$;$$) %s {

        my @char = $_[0] =~ /\G ($q_char) /oxmsg;

        # If the substring is beyond either end of the string, substr() returns the undefined
        # value and produces a warning. When used as an lvalue, specifying a substring that
        # is entirely outside the string raises an exception.
        # http://perldoc.perl.org/functions/substr.html

        # A return with no argument returns the scalar value undef in scalar context,
        # an empty list () in list context, and (naturally) nothing at all in void
        # context.

        my $offset = $_[1];
        if (($offset > scalar(@char)) or ($offset < (-1 * scalar(@char)))) {
            return;
        }

        # substr($string,$offset,$length,$replacement)
        if (@_ == 4) {
            my(undef,undef,$length,$replacement) = @_;
            my $substr = join '', splice(@char, $offset, $length, $replacement);
            $_[0] = join '', @char;

            # return $substr; this doesn't work, don't say "return"
            $substr;
        }

        # substr($string,$offset,$length)
        elsif (@_ == 3) {
            my(undef,undef,$length) = @_;
            my $octet_offset = 0;
            my $octet_length = 0;
            if ($offset == 0) {
                $octet_offset = 0;
            }
            elsif ($offset > 0) {
                $octet_offset =      CORE::length(join '', @char[0..$offset-1]);
            }
            else {
                $octet_offset = -1 * CORE::length(join '', @char[$#char+$offset+1..$#char]);
            }
            if ($length == 0) {
                $octet_length = 0;
            }
            elsif ($length > 0) {
                $octet_length =      CORE::length(join '', @char[$offset..$offset+$length-1]);
            }
            else {
                $octet_length = -1 * CORE::length(join '', @char[$#char+$length+1..$#char]);
            }
            CORE::substr($_[0], $octet_offset, $octet_length);
        }

        # substr($string,$offset)
        else {
            my $octet_offset = 0;
            if ($offset == 0) {
                $octet_offset = 0;
            }
            elsif ($offset > 0) {
                $octet_offset =      CORE::length(join '', @char[0..$offset-1]);
            }
            else {
                $octet_offset = -1 * CORE::length(join '', @char[$#char+$offset+1..$#char]);
            }
            CORE::substr($_[0], $octet_offset);
        }
    }
END
}

#
# UTF-8 index by character
#
sub Char::UTF2::index($$;$) {

    my $index;
    if (@_ == 3) {
        $index = Char::Eutf2::index($_[0], $_[1], CORE::length(Char::UTF2::substr($_[0], 0, $_[2])));
    }
    else {
        $index = Char::Eutf2::index($_[0], $_[1]);
    }

    if ($index == -1) {
        return -1;
    }
    else {
        return Char::UTF2::length(CORE::substr $_[0], 0, $index);
    }
}

#
# UTF-8 rindex by character
#
sub Char::UTF2::rindex($$;$) {

    my $rindex;
    if (@_ == 3) {
        $rindex = Char::Eutf2::rindex($_[0], $_[1], CORE::length(Char::UTF2::substr($_[0], 0, $_[2])));
    }
    else {
        $rindex = Char::Eutf2::rindex($_[0], $_[1]);
    }

    if ($rindex == -1) {
        return -1;
    }
    else {
        return Char::UTF2::length(CORE::substr $_[0], 0, $rindex);
    }
}

#
# instead of Carp::carp
#
sub carp {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
}

#
# instead of Carp::croak
#
sub croak {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
    die "\n";
}

#
# instead of Carp::cluck
#
sub cluck {
    my $i = 0;
    my @cluck = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @cluck, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR CORE::reverse @cluck;
    print STDERR "\n";
    carp @_;
}

#
# instead of Carp::confess
#
sub confess {
    my $i = 0;
    my @confess = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @confess, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR CORE::reverse @confess;
    print STDERR "\n";
    croak @_;
}

1;

__END__

=pod

=head1 NAME

Char::Eutf2 - Run-time routines for Char/UTF2.pm

=head1 SYNOPSIS

  use Char::Eutf2;

    Char::Eutf2::split(...);
    Char::Eutf2::tr(...);
    Char::Eutf2::chop(...);
    Char::Eutf2::index(...);
    Char::Eutf2::rindex(...);
    Char::Eutf2::lc(...);
    Char::Eutf2::lc_;
    Char::Eutf2::lcfirst(...);
    Char::Eutf2::lcfirst_;
    Char::Eutf2::uc(...);
    Char::Eutf2::uc_;
    Char::Eutf2::ucfirst(...);
    Char::Eutf2::ucfirst_;
    Char::Eutf2::fc(...);
    Char::Eutf2::fc_;
    Char::Eutf2::ignorecase(...);
    Char::Eutf2::capture(...);
    Char::Eutf2::chr(...);
    Char::Eutf2::chr_;
    Char::Eutf2::glob(...);
    Char::Eutf2::glob_;

  # "no Char::Eutf2;" not supported

=head1 ABSTRACT

This module is a run-time routines of the Char/UTF2.pm.
Because the Char/UTF2.pm automatically uses this module, you need not use directly.

=head1 BUGS AND LIMITATIONS

I have tested and verified this software using the best of my ability.
However, a software containing much regular expression is bound to contain
some bugs. Thus, if you happen to find a bug that's in Char::UTF2 software and not
your own program, you can try to reduce it to a minimal test case and then
report it to the following author's address. If you have an idea that could
make this a more useful tool, please let everyone share it.

=head1 HISTORY

This Char::Eutf2 module first appeared in ActivePerl Build 522 Built under
MSWin32 Compiled at Nov 2 1999 09:52:28

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.
For any questions, use E<lt>ina@cpan.orgE<gt> so we can share
this file.

=head1 LICENSE AND COPYRIGHT

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 EXAMPLES

=over 2

=item Split string

  @split = Char::Eutf2::split(/pattern/,$string,$limit);
  @split = Char::Eutf2::split(/pattern/,$string);
  @split = Char::Eutf2::split(/pattern/);
  @split = Char::Eutf2::split('',$string,$limit);
  @split = Char::Eutf2::split('',$string);
  @split = Char::Eutf2::split('');
  @split = Char::Eutf2::split();
  @split = Char::Eutf2::split;

  This subroutine scans a string given by $string for separators, and splits the
  string into a list of substring, returning the resulting list value in list
  context or the count of substring in scalar context. Scalar context also causes
  split to write its result to @_, but this usage is deprecated. The separators
  are determined by repeated pattern matching, using the regular expression given
  in /pattern/, so the separators may be of any size and need not be the same
  string on every match. (The separators are not ordinarily returned; exceptions
  are discussed later in this section.) If the /pattern/ doesn't match the string
  at all, Char::Eutf2::split returns the original string as a single substring, If it
  matches once, you get two substrings, and so on. You may supply regular
  expression modifiers to the /pattern/, like /pattern/i, /pattern/x, etc. The
  //m modifier is assumed when you split on the pattern /^/.

  If $limit is specified and positive, the subroutine splits into no more than that
  many fields (though it may split into fewer if it runs out of separators). If
  $limit is negative, it is treated as if an arbitrarily large $limit has been
  specified If $limit is omitted or zero, trailing null fields are stripped from
  the result (which potential users of pop would do wel to remember). If $string
  is omitted, the subroutine splits the $_ string. If /pattern/ is also omitted or
  is the literal space, " ", the subroutine split on whitespace, /\s+/, after
  skipping any leading whitespace.

  A /pattern/ of /^/ is secretly treated if it it were /^/m, since it isn't much
  use otherwise.

  String of any length can be split:

  @chars  = Char::Eutf2::split(//,  $word);
  @fields = Char::Eutf2::split(/:/, $line);
  @words  = Char::Eutf2::split(" ", $paragraph);
  @lines  = Char::Eutf2::split(/^/, $buffer);

  A pattern capable of matching either the null string or something longer than
  the null string (for instance, a pattern consisting of any single character
  modified by a * or ?) will split the value of $string into separate characters
  wherever it matches the null string between characters; nonnull matches will
  skip over the matched separator characters in the usual fashion. (In other words,
  a pattern won't match in one spot more than once, even if it matched with a zero
  width.) For example:

  print join(":" => Char::Eutf2::split(/ */, "hi there"));

  produces the output "h:i:t:h:e:r:e". The space disappers because it matches
  as part of the separator. As a trivial case, the null pattern // simply splits
  into separate characters, and spaces do not disappear. (For normal pattern
  matches, a // pattern would repeat the last successfully matched pattern, but
  Char::Eutf2::split's pattern is exempt from that wrinkle.)

  The $limit parameter splits only part of a string:

  my ($login, $passwd, $remainder) = Char::Eutf2::split(/:/, $_, 3);

  We encourage you to split to lists of names like this to make your code
  self-documenting. (For purposes of error checking, note that $remainder would
  be undefined if there were fewer than three fields.) When assigning to a list,
  if $limit is omitted, Perl supplies a $limit one larger than the number of
  variables in the list, to avoid unneccessary work. For the split above, $limit
  would have been 4 by default, and $remainder would have received only the third
  field, not all the rest of the fields. In time-critical applications, it behooves
  you not to split into more fields than you really need. (The trouble with
  powerful languages it that they let you be powerfully stupid at times.)

  We said earlier that the separators are not returned, but if the /pattern/
  contains parentheses, then the substring matched by each pair of parentheses is
  included in the resulting list, interspersed with the fields that are ordinarily
  returned. Here's a simple example:

  Char::Eutf2::split(/([-,])/, "1-10,20");

  which produces the list value:

  (1, "-", 10, ",", 20)

  With more parentheses, a field is returned for each pair, even if some pairs
  don't match, in which case undefined values are returned in those positions. So
  if you say:

  Char::Eutf2::split(/(-)|(,)/, "1-10,20");

  you get the value:

  (1, "-", undef, 10, undef, ",", 20)

  The /pattern/ argument may be replaced with an expression to specify patterns
  that vary at runtime. As with ordinary patterns, to do run-time compilation only
  once, use /$variable/o.

  As a special case, if the expression is a single space (" "), the subroutine
  splits on whitespace just as Char::Eutf2::split with no arguments does. Thus,
  Char::Eutf2::split(" ") can be used to emulate awk's default behavior. In contrast,
  Char::Eutf2::split(/ /) will give you as many null initial fields as there are
  leading spaces. (Other than this special case, if you supply a string instead
  of a regular expression, it'll be interpreted as a regular expression anyway.)
  You can use this property to remove leading and trailing whitespace from a
  string and to collapse intervaning stretches of whitespace into a single
  space:

  $string = join(" ", Char::Eutf2::split(" ", $string));

  The following example splits an RFC822 message header into a hash containing
  $head{'Date'}, $head{'Subject'}, and so on. It uses the trick of assigning a
  list of pairs to a hash, because separators altinate with separated fields, It
  users parentheses to return part of each separator as part of the returned list
  value. Since the split pattern is guaranteed to return things in pairs by virtue
  of containing one set of parentheses, the hash assignment is guaranteed to
  receive a list consisting of key/value pairs, where each key is the name of a
  header field. (Unfortunately, this technique loses information for multiple lines
  with the same key field, such as Received-By lines. Ah well)

  $header =~ s/\n\s+/ /g; # Merge continuation lines.
  %head = ("FRONTSTUFF", Char::Eutf2::split(/^(\S*?):\s*/m, $header));

  The following example processes the entries in a Unix passwd(5) file. You could
  leave out the chomp, in which case $shell would have a newline on the end of it.

  open(PASSWD, "/etc/passwd");
  while (<PASSWD>) {
      chomp; # remove trailing newline.
      ($login, $passwd, $uid, $gid, $gcos, $home, $shell) =
          Char::Eutf2::split(/:/);
      ...
  }

  Here's how process each word of each line of each file of input to create a
  word-frequency hash.

  while (<>) {
      for my $word (Char::Eutf2::split()) {
          $count{$word}++;
      }
  }

  The inverse of Char::Eutf2::split is join, except that join can only join with the
  same separator between all fields. To break apart a string with fixed-position
  fields, use unpack.

  Processing long $string (over 32766 octets) requires Perl 5.010001 or later.

=item Transliteration

  $tr = Char::Eutf2::tr($variable,$bind_operator,$searchlist,$replacementlist,$modifier);
  $tr = Char::Eutf2::tr($variable,$bind_operator,$searchlist,$replacementlist);

  This is the transliteration (sometimes erroneously called translation) operator,
  which is like the y/// operator in the Unix sed program, only better, in
  everybody's humble opinion.

  This subroutine scans a UTF-8 string character by character and replaces all
  occurrences of the characters found in $searchlist with the corresponding character
  in $replacementlist. It returns the number of characters replaced or deleted.
  If no UTF-8 string is specified via =~ operator, the $_ variable is translated.
  $modifier are:

  ---------------------------------------------------------------------------
  Modifier   Meaning
  ---------------------------------------------------------------------------
  c          Complement $searchlist.
  d          Delete found but unreplaced characters.
  s          Squash duplicate replaced characters.
  r          Return transliteration and leave the original string untouched.
  ---------------------------------------------------------------------------

  To use with a read-only value without raising an exception, use the /r modifier.

  print Char::Eutf2::tr('bookkeeper','=~','boep','peob','r'); # prints 'peekkoobor'

=item Chop string

  $chop = Char::Eutf2::chop(@list);
  $chop = Char::Eutf2::chop();
  $chop = Char::Eutf2::chop;

  This subroutine chops off the last character of a string variable and returns the
  character chopped. The Char::Eutf2::chop subroutine is used primary to remove the newline
  from the end of an input recoed, and it is more efficient than using a
  substitution. If that's all you're doing, then it would be safer to use chomp,
  since Char::Eutf2::chop always shortens the string no matter what's there, and chomp
  is more selective. If no argument is given, the subroutine chops the $_ variable.

  You cannot Char::Eutf2::chop a literal, only a variable. If you Char::Eutf2::chop a list of
  variables, each string in the list is chopped:

  @lines = `cat myfile`;
  Char::Eutf2::chop(@lines);

  You can Char::Eutf2::chop anything that is an lvalue, including an assignment:

  Char::Eutf2::chop($cwd = `pwd`);
  Char::Eutf2::chop($answer = <STDIN>);

  This is different from:

  $answer = Char::Eutf2::chop($tmp = <STDIN>); # WRONG

  which puts a newline into $answer because Char::Eutf2::chop returns the character
  chopped, not the remaining string (which is in $tmp). One way to get the result
  intended here is with substr:

  $answer = substr <STDIN>, 0, -1;

  But this is more commonly written as:

  Char::Eutf2::chop($answer = <STDIN>);

  In the most general case, Char::Eutf2::chop can be expressed using substr:

  $last_code = Char::Eutf2::chop($var);
  $last_code = substr($var, -1, 1, ""); # same thing

  Once you understand this equivalence, you can use it to do bigger chops. To
  Char::Eutf2::chop more than one character, use substr as an lvalue, assigning a null
  string. The following removes the last five characters of $caravan:

  substr($caravan, -5) = '';

  The negative subscript causes substr to count from the end of the string instead
  of the beginning. To save the removed characters, you could use the four-argument
  form of substr, creating something of a quintuple Char::Eutf2::chop;

  $tail = substr($caravan, -5, 5, '');

  This is all dangerous business dealing with characters instead of graphemes. Perl
  doesn't really have a grapheme mode, so you have to deal with them yourself.

=item Index string

  $byte_pos = Char::Eutf2::index($string,$substr,$byte_offset);
  $byte_pos = Char::Eutf2::index($string,$substr);

  This subroutine searches for one string within another. It returns the byte position
  of the first occurrence of $substring in $string. The $byte_offset, if specified,
  says how many bytes from the start to skip before beginning to look. Positions are
  based at 0. If the substring is not found, the subroutine returns one less than the
  base, ordinarily -1. To work your way through a string, you might say:

  $byte_pos = -1;
  while (($byte_pos = Char::Eutf2::index($string, $lookfor, $byte_pos)) > -1) {
      print "Found at $byte_pos\n";
      $byte_pos++;
  }

=item Reverse index string

  $byte_pos = Char::Eutf2::rindex($string,$substr,$byte_offset);
  $byte_pos = Char::Eutf2::rindex($string,$substr);

  This subroutine works just like Char::Eutf2::index except that it returns the byte
  position of the last occurrence of $substring in $string (a reverse Char::Eutf2::index).
  The subroutine returns -1 if $substring is not found. $byte_offset, if specified,
  is the rightmost byte position that may be returned. To work your way through a
  string backward, say:

  $byte_pos = length($string);
  while (($byte_pos = Char::UTF2::rindex($string, $lookfor, $byte_pos)) >= 0) {
      print "Found at $byte_pos\n";
      $byte_pos--;
  }

=item Lower case string

  $lc = Char::Eutf2::lc($string);
  $lc = Char::Eutf2::lc_;

  This subroutine returns a lowercased version of UTF-8 $string (or $_, if
  $string is omitted). This is the internal subroutine implementing the \L escape
  in double-quoted strings.

  You can use the Char::Eutf2::fc subroutine for case-insensitive comparisons via Char::UTF2
  software.

=item Lower case first character of string

  $lcfirst = Char::Eutf2::lcfirst($string);
  $lcfirst = Char::Eutf2::lcfirst_;

  This subroutine returns a version of UTF-8 $string with the first character
  lowercased (or $_, if $string is omitted). This is the internal subroutine
  implementing the \l escape in double-quoted strings.

=item Upper case string

  $uc = Char::Eutf2::uc($string);
  $uc = Char::Eutf2::uc_;

  This subroutine returns an uppercased version of UTF-8 $string (or $_, if
  $string is omitted). This is the internal subroutine implementing the \U escape
  in interpolated strings. For titlecase, use Char::Eutf2::ucfirst instead.

  You can use the Char::Eutf2::fc subroutine for case-insensitive comparisons via Char::UTF2
  software.

=item Upper case first character of string

  $ucfirst = Char::Eutf2::ucfirst($string);
  $ucfirst = Char::Eutf2::ucfirst_;

  This subroutine returns a version of UTF-8 $string with the first character
  titlecased and other characters left alone (or $_, if $string is omitted).
  Titlecase is "Camel" for an initial capital that has (or expects to have)
  lowercase characters following it, not uppercase ones. Exsamples are the first
  letter of a sentence, of a person's name, of a newspaper headline, or of most
  words in a title. Characters with no titlecase mapping return the uppercase
  mapping instead. This is the internal subroutine implementing the \u escape in
  double-quoted strings.

  To capitalize a string by mapping its first character to titlecase and the rest
  to lowercase, use:

  $titlecase = Char::Eutf2::ucfirst(substr($word,0,1)) . Char::Eutf2::lc(substr($word,1));

  or

  $string =~ s/(\w)(\w*)/\u$1\L$2/g;

  Do not use:

  $do_not_use = Char::Eutf2::ucfirst(Char::Eutf2::lc($word));

  or "\u\L$word", because that can produce a different and incorrect answer with
  certain characters. The titlecase of something that's been lowercased doesn't
  always produce the same thing titlecasing the original produces.

  Because titlecasing only makes sense at the start of a string that's followed
  by lowercase characters, we can't think of any reason you might want to titlecase
  every character in a string.

  See also P.287 A Case of Mistaken Identity
  in Chapter 6: Unicode
  of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

=item Fold case string

  P.860 fc
  in Chapter 27: Functions
  of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

  $fc = Char::Eutf2::fc($string);
  $fc = Char::Eutf2::fc_;

  New to Char::UTF2 software, this subroutine returns the full Unicode-like casefold of
  UTF-8 $string (or $_, if omitted). This is the internal subroutine implementing
  the \F escape in double-quoted strings.

  Just as title-case is based on uppercase but different, foldcase is based on
  lowercase but different. In ASCII there is a one-to-one mapping between only
  two cases, but in other encoding there is a one-to-many mapping and between three
  cases. Because that's too many combinations to check manually each time, a fourth
  casemap called foldcase was invented as a common intermediary for the other three.
  It is not a case itself, but it is a casemap.

  To compare whether two strings are the same without regard to case, do this:

  Char::Eutf2::fc($a) eq Char::Eutf2::fc($b)

  The reliable way to compare string case-insensitively was with the /i pattern
  modifier, because Char::UTF2 software has always used casefolding semantics for
  case-insensitive pattern matches. Knowing this, you can emulate equality
  comparisons like this:

  sub fc_eq ($$) {
      my($a,$b) = @_;
      return $a =~ /\A\Q$b\E\z/i;
  }

=item Make ignore case string

  @ignorecase = Char::Eutf2::ignorecase(@string);

  This subroutine is internal use to m/ /i, s/ / /i, split / /i and qr/ /i.

=item Make capture number

  $capturenumber = Char::Eutf2::capture($string);

  This subroutine is internal use to m/ /, s/ / /, split / / and qr/ /.

=item Make character

  $chr = Char::Eutf2::chr($code);
  $chr = Char::Eutf2::chr_;

  This subroutine returns a programmer-visible character, character represented by
  that $code in the character set. For example, Char::Eutf2::chr(65) is "A" in either
  ASCII or UTF-8, not Unicode. For the reverse of Char::Eutf2::chr, use Char::UTF2::ord.

=item Filename expansion (globbing)

  @glob = Char::Eutf2::glob($string);
  @glob = Char::Eutf2::glob_;

  This subroutine returns the value of $string with filename expansions the way a
  DOS-like shell would expand them, returning the next successive name on each
  call. If $string is omitted, $_ is globbed instead. This is the internal
  subroutine implementing the <*> and glob operator.
  This subroutine function when the pathname ends with chr(0x5C) on MSWin32.

  For ease of use, the algorithm matches the DOS-like shell's style of expansion,
  not the UNIX-like shell's. An asterisk ("*") matches any sequence of any
  character (including none). A question mark ("?") matches any one character or
  none. A tilde ("~") expands to a home directory, as in "~/.*rc" for all the
  current user's "rc" files, or "~jane/Mail/*" for all of Jane's mail files.

  Note that all path components are case-insensitive, and that backslashes and
  forward slashes are both accepted, and preserved. You may have to double the
  backslashes if you are putting them in literally, due to double-quotish parsing
  of the pattern by perl.

  The Char::Eutf2::glob subroutine grandfathers the use of whitespace to separate multiple
  patterns such as <*.c *.h>. If you want to glob filenames that might contain
  whitespace, you'll have to use extra quotes around the spacy filename to protect
  it. For example, to glob filenames that have an "e" followed by a space followed
  by an "f", use either of:

  @spacies = <"*e f*">;
  @spacies = Char::Eutf2::glob('"*e f*"');
  @spacies = Char::Eutf2::glob(q("*e f*"));

  If you had to get a variable through, you could do this:

  @spacies = Char::Eutf2::glob("'*${var}e f*'");
  @spacies = Char::Eutf2::glob(qq("*${var}e f*"));

  Another way on MSWin32

  # relative path
  @relpath_file = split(/\n/,`dir /b wildcard\\here*.txt 2>NUL`);

  # absolute path
  @abspath_file = split(/\n/,`dir /s /b wildcard\\here*.txt 2>NUL`);

  # on COMMAND.COM
  @relpath_file = split(/\n/,`dir /b wildcard\\here*.txt`);
  @abspath_file = split(/\n/,`dir /s /b wildcard\\here*.txt`);

=cut

