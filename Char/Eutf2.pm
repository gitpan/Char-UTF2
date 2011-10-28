package Char::Eutf2;
######################################################################
#
# Char::Eutf2 - Run-time routines for Char/UTF2.pm
#
# Copyright (c) 2008, 2009, 2010, 2011 INABA Hitoshi <ina@cpan.org>
#
######################################################################

use 5.00503;
use strict qw(subs vars);

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 0.78 $ =~ m/(\d+)/xmsg;

BEGIN {
    my $PERL5LIB = __FILE__;

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        $PERL5LIB =~ s{[^\x80-\xFF/]*$}{Char::UTF2};
    }

    # UNIX-like system
    else {
        $PERL5LIB =~ s{[^\x80-\xFF/]*$}{Char::UTF2};
    }

    my @inc = ();
    my %inc = ();
    for my $path ($PERL5LIB, @INC) {
        if (not exists $inc{$path}) {
            push @inc, $path;
            $inc{$path} = 1;
        }
    }
    @INC = @inc;
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

# poor Symbol.pm - substitute of real Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;

    sub gensym () {
        my $name = "GEN" . $genseq++;
        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }

    sub qualify ($;$) {
        my ($name) = @_;
        if (!ref($name) && (Char::Eutf2::index($name, '::') == -1) && (Char::Eutf2::index($name, "'") == -1)) {
            my $pkg;
            my %global = map {$_ => 1} qw(ARGV ARGVOUT ENV INC SIG STDERR STDIN STDOUT);

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
        $name;
    }

    sub qualify_to_ref ($;$) {
        no strict qw(refs);
        return \*{ qualify $_[0], @_ > 1 ? $_[1] : caller };
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

# instead of Carp.pm
sub carp(@);
sub croak(@);
sub cluck(@);
sub confess(@);

my $__FILE__ = __FILE__;

BEGIN {
    if ($^X =~ m/ jperl /oxmsi) {
        die "$0 need perl(not jperl) 5.00503 or later. (\$^X==$^X)";
    }
}

my $your_char = q{(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[\x00-\xFF]};

# regexp of character
my $q_char = qr/$your_char/oxms;

#
# UTF-2 character range per length
#
my %range_tr = ();
my $is_shiftjis_family = 0;
my $is_eucjp_family    = 0;

#
# alias of encoding name
#

BEGIN { eval q{ use vars qw($encoding_alias) } }

if (0) {
}

# US-ASCII
elsif (__PACKAGE__ =~ m/ \b Eusascii \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: (?:us-?)?ascii ) \b /oxmsi;
}

# Latin-1
elsif (__PACKAGE__ =~ m/ \b Elatin1 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-1 | iec[- ]?8859-1 | latin-?1 ) \b /oxmsi;
}

# Latin-2
elsif (__PACKAGE__ =~ m/ \b Elatin2 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-2 | iec[- ]?8859-2 | latin-?2 ) \b /oxmsi;
}

# Latin-3
elsif (__PACKAGE__ =~ m/ \b Elatin3 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-3 | iec[- ]?8859-3 | latin-?3 ) \b /oxmsi;
}

# Latin-4
elsif (__PACKAGE__ =~ m/ \b Elatin4 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-4 | iec[- ]?8859-4 | latin-?4 ) \b /oxmsi;
}

# Cyrillic
elsif (__PACKAGE__ =~ m/ \b Ecyrillic \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-5 | iec[- ]?8859-5 | cyrillic ) \b /oxmsi;
}

# KOI8-R
elsif (__PACKAGE__ =~ m/ \b Ekoi8r \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: koi8-?r ) \b /oxmsi;
}

# KOI8-U
elsif (__PACKAGE__ =~ m/ \b Ekoi8u \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: koi8-?u ) \b /oxmsi;
}

# Greek
elsif (__PACKAGE__ =~ m/ \b Egreek \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-7 | iec[- ]?8859-7 | greek ) \b /oxmsi;
}

# Latin-5
elsif (__PACKAGE__ =~ m/ \b Elatin5 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-9 | iec[- ]?8859-9 | latin-?5 ) \b /oxmsi;
}

# Latin-6
elsif (__PACKAGE__ =~ m/ \b Elatin6 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-10 | iec[- ]?8859-10 | latin-?6 ) \b /oxmsi;
}

# Latin-7
elsif (__PACKAGE__ =~ m/ \b Elatin7 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-13 | iec[- ]?8859-13 | latin-?7 ) \b /oxmsi;
}

# Latin-8
elsif (__PACKAGE__ =~ m/ \b Elatin8 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-14 | iec[- ]?8859-14 | latin-?8 ) \b /oxmsi;
}

# Latin-9
elsif (__PACKAGE__ =~ m/ \b Elatin9 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-15 | iec[- ]?8859-15 | latin-?9 ) \b /oxmsi;
}

# Latin-10
elsif (__PACKAGE__ =~ m/ \b Elatin10 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: iso[-_ ]?8859-16 | iec[- ]?8859-16 | latin-?10 ) \b /oxmsi;
}

# Windows-1252
elsif (__PACKAGE__ =~ m/ \b Ewindows1252 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: windows-?1252 ) \b /oxmsi;
}

# Windows-1258
elsif (__PACKAGE__ =~ m/ \b Ewindows1258 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0xFF],
             ],
    );
    $encoding_alias = qr/ \b (?: windows-?1258 ) \b /oxmsi;
}

# EUC-JP
elsif (__PACKAGE__ =~ m/ \b Eeucjp \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x8D,0x90..0xA0,0xFF],
             ],
        2 => [ [0x8E..0x8E],[0xA1..0xDF],
               [0xA1..0xFE],[0xA1..0xFE],
             ],
        3 => [ [0x8F..0x8F],[0xA1..0xFE],[0xA1..0xFE],
             ],
    );
    $is_eucjp_family = 1;
    $encoding_alias = qr/ \b (?: euc.*jp | jp.*euc | ujis ) \b /oxmsi;
}

# UTF-2
elsif (__PACKAGE__ =~ m/ \b Eutf2 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F],
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
}

# Old UTF-8
elsif (__PACKAGE__ =~ m/ \b Eoldutf8 \z/oxms) {
    %range_tr = (
        1 => [ [0x00..0x7F],
             ],
        2 => [ [0xC0..0xDF],[0x80..0xBF],
             ],
        3 => [ [0xE0..0xEF],[0x80..0xBF],[0x80..0xBF],
             ],
        4 => [ [0xF0..0xF4],[0x80..0xBF],[0x80..0xBF],[0x80..0xBF],
             ],
    );
    $encoding_alias = qr/ \b (?: utf8 | cesu-?8 | modified[ ]?utf-?8 | old[ ]?utf-?8 ) \b /oxmsi;
}

else {
    croak "$0 don't know my package name '" . __PACKAGE__ . "'";
}

#
# Prototypes of subroutines
#
sub import() {}
sub unimport() {}
sub Char::Eutf2::split(;$$$);
sub Char::Eutf2::tr($$$$;$);
sub Char::Eutf2::chop(@);
sub Char::Eutf2::index($$;$);
sub Char::Eutf2::rindex($$;$);
sub Char::Eutf2::classic_character_class($);
sub Char::Eutf2::capture($);
sub Char::Eutf2::chr(;$);
sub Char::Eutf2::chr_();
sub Char::Eutf2::glob($);
sub Char::Eutf2::glob_();

sub Char::UTF2::ord(;$);
sub Char::UTF2::ord_();
sub Char::UTF2::reverse(@);
sub Char::UTF2::length(;$);
sub Char::UTF2::substr($$;$$);
sub Char::UTF2::index($$;$);
sub Char::UTF2::rindex($$;$);

#
# @ARGV wildcard globbing
#
if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    if ($ENV{'ComSpec'} =~ / (?: COMMAND\.COM | CMD\.EXE ) \z /oxmsi) {
        my @argv = ();
        for (@ARGV) {
            if (m/\A ' ((?:$q_char)*) ' \z/oxms) {
                push @argv, $1;
            }
            elsif (m/\A (?:$q_char)*? [*?] /oxms and (my @glob = Char::Eutf2::glob($_))) {
                push @argv, @glob;
            }
            else {
                push @argv, $_;
            }
        }
        @ARGV = @argv;
    }
}

#
# UTF-2 split
#
sub Char::Eutf2::split(;$$$) {

    # P.794 29.2.161. split
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    my $pattern = $_[0];
    my $string  = $_[1];
    my $limit   = $_[2];

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
            carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
            @_ = @split;
            return scalar @_;
        }
    }

    # if $limit is negative, it is treated as if an arbitrarily large $limit has been specified
    if ((not defined $limit) or ($limit <= 0)) {

        # if $pattern is also omitted or is the literal space, " ", the function splits
        # on whitespace, /\s+/, after skipping any leading whitespace
        # (and so on)

        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;

            # P.1024 Appendix W.10 Multibyte Processing
            # of ISBN 1-56592-224-7 CJKV Information Processing
            # (and so on)

            # the //m modifier is assumed when you split on the pattern /^/
            # (and so on)

            while ($string =~ s/\A((?:$q_char)*?)\s+//m) {

                # if the $pattern contains parentheses, then the substring matched by each pair of parentheses
                # is included in the resulting list, interspersed with the fields that are ordinarily returned
                # (and so on)

                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        # a pattern capable of matching either the null string or something longer than the
        # null string will split the value of $string into separate characters wherever it
        # matches the null string between characters
        # (and so on)

        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }

        else {
            while ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                local $@;
                for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                    push @split, eval '$' . $digit;
                }
            }
        }
    }

    else {
        if ((not defined $pattern) or ($pattern eq ' ')) {
            $string =~ s/ \A \s+ //oxms;
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)\s+//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        elsif ('' =~ m/ \A $pattern \z /xms) {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)+?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
        else {
            while ((--$limit > 0) and (CORE::length($string) > 0)) {
                if ($string =~ s/\A((?:$q_char)*?)$pattern//m) {
                    local $@;
                    for (my $digit=1; eval "defined(\$$digit)"; $digit++) {
                        push @split, eval '$' . $digit;
                    }
                }
            }
        }
    }

    push @split, $string;

    # if $limit is omitted or zero, trailing null fields are stripped from the result
    if ((not defined $limit) or ($limit == 0)) {
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
        carp "$0: Use of implicit split to \@_ is deprecated" if $^W;
        @_ = @split;
        return scalar @_;
    }
}

#
# UTF-2 transliteration (tr///)
#
sub Char::Eutf2::tr($$$$;$) {

    my $bind_operator   = $_[1];
    my $searchlist      = $_[2];
    my $replacementlist = $_[3];
    my $modifier        = $_[4] || '';

    if ($modifier =~ m/r/oxms) {
        if ($bind_operator =~ m/ !~ /oxms) {
            croak "$0: Using !~ with tr///r doesn't make sense";
        }
    }

    my @char            = $_[0] =~ m/\G ($q_char) /oxmsg;
    my @searchlist      = _charlist_tr($searchlist);
    my @replacementlist = _charlist_tr($replacementlist);

    my %tr = ();
    for (my $i=0; $i <= $#searchlist; $i++) {
        if (not exists $tr{$searchlist[$i]}) {
            if (defined $replacementlist[$i] and ($replacementlist[$i] ne '')) {
                $tr{$searchlist[$i]} = $replacementlist[$i];
            }
            elsif ($modifier =~ m/d/oxms) {
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
    if ($modifier =~ m/c/oxms) {
        while (defined(my $char = shift @char)) {
            if (not exists $tr{$char}) {
                if (defined $replacementlist[0]) {
                    $replaced .= $replacementlist[0];
                }
                $tr++;
                if ($modifier =~ m/s/oxms) {
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
                if ($modifier =~ m/s/oxms) {
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

    if ($modifier =~ m/r/oxms) {
        return $replaced;
    }
    else {
        $_[0] = $replaced;
        if ($bind_operator =~ m/ !~ /oxms) {
            return not $tr;
        }
        else {
            return $tr;
        }
    }
}

#
# UTF-2 chop
#
sub Char::Eutf2::chop(@) {

    my $chop;
    if (@_ == 0) {
        my @char = m/\G ($q_char) /oxmsg;
        $chop = pop @char;
        $_ = join '', @char;
    }
    else {
        for (@_) {
            my @char = m/\G ($q_char) /oxmsg;
            $chop = pop @char;
            $_ = join '', @char;
        }
    }
    return $chop;
}

#
# UTF-2 index by octet
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
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return -1;
}

#
# UTF-2 reverse index
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
        if (CORE::substr($str,$pos) =~ m/\A ($q_char) /oxms) {
            $pos += CORE::length($1);
        }
        else {
            $pos += 1;
        }
    }
    return $rindex;
}

#
# UTF-2 regexp capture
#
{
    sub Char::Eutf2::capture($) {
        return $_[0];
    }
}

#
# classic character class ( \D \S \W \d \s \w \C \X \H \V \h \v \R \N \b \B )
#
sub classic_character_class($) {
    my($char) = @_;

    return {
        '\D' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF0-9])',
        '\S' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x0A\x0C\x0D\x20])',
        '\W' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF0-9A-Z_a-z])',
        '\d' => '[0-9]',
                 # \t  \n  \f  \r space
        '\s' => '[\x09\x0A\x0C\x0D\x20]',
        '\w' => '[0-9A-Z_a-z]',
        '\C' => '[\x00-\xFF]',
        '\X' => 'X',

        # \h \v \H \V
        #
        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        '\H' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x20])',
        '\V' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x0C\x0A\x0D])',
        '\h' => '[\x09\x20]',
        '\v' => '[\x0C\x0A\x0D]',
        '\R' => '(?:\x0D\x0A|[\x0A\x0D])',

        # \N
        #
        # http://perldoc.perl.org/perlre.html
        # Character Classes and other Special Escapes
        # Any character but \n (experimental). Not affected by /s modifier

        '\N' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x0A])',

        # \b \B
        #
        # P.131 Word boundaries: \b, \B, \<, \>, ...
        # in Chapter 3: Overview of Regular Expression Features and Flavors
        # of ISBN 0-596-00289-0 Mastering Regular Expressions, Second edition

        # '\b' => '(?:(?<=\A|\W)(?=\w)|(?<=\w)(?=\W|\z))',
        '\b' => '(?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))',

        # '\B' => '(?:(?<=\w)(?=\w)|(?<=\W)(?=\W))',
        '\B' => '(?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))',

    }->{$char} || '';
}

#
# prepare UTF-2 characters per length
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

# minimum value of each octet
my @minchar = ();
sub minchar {
    if (defined $minchar[$_[0]]) {
        return $minchar[$_[0]];
    }
    $minchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[0];
}

# maximum value of each octet
my @maxchar = ();
sub maxchar {
    if (defined $maxchar[$_[0]]) {
        return $maxchar[$_[0]];
    }
    $maxchar[$_[0]] = (&{(sub {}, \&chars1, \&chars2, \&chars3, \&chars4)[$_[0]]})[-1];
}

#
# UTF-2 open character list for tr
#
sub _charlist_tr {

    local $_ = shift @_;

    # unescape character
    my @char = ();
    while (not m/\G \z/oxmsgc) {
        if (m/\G (\\0?55|\\x2[Dd]|\\-) /oxmsgc) {
            push @char, '\-';
        }
        elsif (m/\G \\ ([0-7]{2,3}) /oxmsgc) {
            push @char, CORE::chr(oct $1);
        }
        elsif (m/\G \\x ([0-9A-Fa-f]{1,2}) /oxmsgc) {
            push @char, CORE::chr(hex $1);
        }
        elsif (m/\G \\c ([\x40-\x5F]) /oxmsgc) {
            push @char, CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif (m/\G (\\ [0nrtfbae]) /oxmsgc) {
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
        elsif (m/\G \\ ($q_char) /oxmsgc) {
            push @char, $1;
        }
        elsif (m/\G ($q_char) /oxmsgc) {
            push @char, $1;
        }
    }

    # join separated multiple octet
    @char = join('',@char) =~ m/\G (\\-|$q_char) /oxmsg;

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
        if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
            croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
        }

        # range of multiple octet code
        if (length($char[$i-1]) == 1) {
            if (length($char[$i+1]) == 1) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars1();
            }
            elsif (length($char[$i+1]) == 2) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range, grep {$_ le $char[$i+1]}                           &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars1();
                push @range,                                                    &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 2) {
            if (length($char[$i+1]) == 2) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars2();
            }
            elsif (length($char[$i+1]) == 3) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range, grep {$_ le $char[$i+1]}                           &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars2();
                push @range,                                                    &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 3) {
            if (length($char[$i+1]) == 3) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars3();
            }
            elsif (length($char[$i+1]) == 4) {
                push @range, grep {$char[$i-1] le $_}                           &chars3();
                push @range, grep {$_ le $char[$i+1]}                           &chars4();
            }
        }
        elsif (length($char[$i-1]) == 4) {
            if (length($char[$i+1]) == 4) {
                push @range, grep {($char[$i-1] le $_) and ($_ le $char[$i+1])} &chars4();
            }
        }

        splice @char, $i-1, 3, @range;
    }

    return @char;
}

#
# UTF-2 octet range
#
sub _octets {

    my $modifier = pop @_;
    my $length = shift;

    my($a) = unpack 'C', $_[0];
    my($z) = unpack 'C', $_[1];

    # single octet code
    if ($length == 1) {

        # single octet and ignore case
        if (((caller(1))[3] ne 'Char::Eutf2::_octets') and ($modifier =~ m/i/oxms)) {
            if ($a == $z) {
                return sprintf('(?i:\x%02X)',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('(?i:[\x%02X\x%02X])',  $a, $z);
            }
            else {
                return sprintf('(?i:[\x%02X-\x%02X])', $a, $z);
            }
        }

        # not ignore case or one of multiple octet
        else {
            if ($a == $z) {
                return sprintf('\x%02X',          $a);
            }
            elsif (($a+1) == $z) {
                return sprintf('[\x%02X\x%02X]',  $a, $z);
            }
            else {
                return sprintf('[\x%02X-\x%02X]', $a, $z);
            }
        }
    }

    # double octet code of Shift_JIS family
    elsif (($length == 2) and $is_shiftjis_family and ($a <= 0x9F) and (0xE0 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        if ($a == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                            0x9F,$a2);
        }
        elsif (($a+1) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|\x%02X[\x00-\xFF]',          $a,  $a2,$a+1);
        }
        elsif (($a+2) == 0x9F) {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X\x%02X][\x00-\xFF]',  $a,  $a2,$a+1,$a+2);
        }
        else {
            $octets1 = sprintf('\x%02X[\x%02X-\xFF]|[\x%02X-\x%02X][\x00-\xFF]', $a,  $a2,$a+1,$a+2);
        }

        if ($z == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xE0) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xE0) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xE0,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # double octet code of EUC-JP family
    elsif (($length == 2) and $is_eucjp_family and ($a == 0x8E) and (0xA1 <= $z)) {
        my(undef,$a2) = unpack 'CC', $_[0];
        my(undef,$z2) = unpack 'CC', $_[1];
        my $octets1;
        my $octets2;

        $octets1 = sprintf('\x%02X[\x%02X-\xFF]',                                0x8E,$a2);

        if ($z == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\x%02X]',                                      $z,$z2);
        }
        elsif (($z-1) == 0xA1) {
            $octets2 = sprintf('\x%02X[\x00-\xFF]|\x%02X[\x00-\x%02X]',               $z-1,$z,$z2);
        }
        elsif (($z-2) == 0xA1) {
            $octets2 = sprintf('[\x%02X\x%02X][\x00-\xFF]|\x%02X[\x00X-\x%02X]', $z-2,$z-1,$z,$z2);
        }
        else {
            $octets2 = sprintf('[\x%02X-\x%02X][\x00-\xFF]|\x%02X[\x00-\x%02X]', 0xA1,$z-1,$z,$z2);
        }

        return "(?:$octets1|$octets2)";
    }

    # multiple octet code
    else {
        my(undef,$aa) = unpack 'Ca*', $_[0];
        my(undef,$zz) = unpack 'Ca*', $_[1];

        if ($a == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                $zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+1) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+2) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',         $a+1,       _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        elsif (($a+3) == $z) {
            return '(?:' . join('|',
                sprintf('\x%02X%s',         $a,         _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X\x%02X]%s', $a+1,$z-1,  _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',              $z,    _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
        else {
            return '(?:' . join('|',
                sprintf('\x%02X%s',          $a,        _octets($length-1,$aa,                &maxchar($length-1),$modifier)),
                sprintf('[\x%02X-\x%02X]%s', $a+1,$z-1, _octets($length-1,&minchar($length-1),&maxchar($length-1),$modifier)),
                sprintf('\x%02X%s',               $z,   _octets($length-1,&minchar($length-1),$zz,                $modifier)),
            ) . ')';
        }
    }
}

#
# UTF-2 open character list for qr and not qr
#
sub _charlist {

    my $modifier = pop @_;
    my @char = @_;

    # unescape character
    for (my $i=0; $i <= $#char; $i++) {

        # escape - to ...
        if ($char[$i] eq '-') {
            if ((0 < $i) and ($i < $#char)) {
                $char[$i] = '...';
            }
        }

        # octal escape sequence
        elsif ($char[$i] =~ m/\A \\o \{ ([0-7]+) \} \z/oxms) {
            $char[$i] = octchr($1);
        }

        # hexadecimal escape sequence
        elsif ($char[$i] =~ m/\A \\x \{ ([0-9A-Fa-f]+) \} \z/oxms) {
            $char[$i] = hexchr($1);
        }

        # \N{CHARNAME} --> N{CHARNAME}
        elsif ($char[$i] =~ m/\A \\ ( N\{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p{PROPERTY} --> p{PROPERTY}
        elsif ($char[$i] =~ m/\A \\ ( p\{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \P{PROPERTY} --> P{PROPERTY}
        elsif ($char[$i] =~ m/\A \\ ( P\{ ([^\x80-\xFF0-9\}][^\x80-\xFF\}]*) \} ) \z/oxms) {
            $char[$i] = $1;
        }

        # \p, \P, \X --> p, P, X
        elsif ($char[$i] =~ m/\A \\ ( [pPX] ) \z/oxms) {
            $char[$i] = $1;
        }

        elsif ($char[$i] =~ m/\A \\ ([0-7]{2,3}) \z/oxms) {
            $char[$i] = CORE::chr oct $1;
        }
        elsif ($char[$i] =~ m/\A \\x ([0-9A-Fa-f]{1,2}) \z/oxms) {
            $char[$i] = CORE::chr hex $1;
        }
        elsif ($char[$i] =~ m/\A \\c ([\x40-\x5F]) \z/oxms) {
            $char[$i] = CORE::chr(CORE::ord($1) & 0x1F);
        }
        elsif ($char[$i] =~ m/\A (\\ [0nrtfbaedswDSWHVhvR]) \z/oxms) {
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
                '\s' => '[\x09\x0A\x0C\x0D\x20]',
                '\w' => '[0-9A-Z_a-z]',
                '\D' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF0-9])',
                '\S' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x0A\x0C\x0D\x20])',
                '\W' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF0-9A-Z_a-z])',

                '\H' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x20])',
                '\V' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x0C\x0A\x0D])',
                '\h' => '[\x09\x20]',
                '\v' => '[\x0C\x0A\x0D]',
                '\R' => '(?:\x0D\x0A|[\x0A\x0D])',

            }->{$1};
        }

        # POSIX-style character classes
        elsif ($char[$i] =~ m/\A ( \[\: \^? (?:alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|punct|space|upper|word|xdigit) :\] ) \z/oxms) {
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
                '[:space:]'   => '[\x09\x0A\x0B\x0C\x0D\x20]',
                '[:upper:]'   => '[\x41-\x5A]',
                '[:word:]'    => '[\x30-\x39\x41-\x5A\x5F\x61-\x7A]',
                '[:xdigit:]'  => '[\x30-\x39\x41-\x46\x61-\x66]',

                '[:^alnum:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x61-\x7A])',
                '[:^alpha:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x41-\x5A\x61-\x7A])',
                '[:^ascii:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x00-\x7F])',
                '[:^blank:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x20])',
                '[:^cntrl:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x00-\x1F\x7F])',
                '[:^digit:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x30-\x39])',
                '[:^graph:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x21-\x7F])',
                '[:^lower:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x61-\x7A])',
                '[:^print:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x20-\x7F])',
                '[:^punct:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])',
                '[:^space:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x09\x0A\x0B\x0C\x0D\x20])',
                '[:^upper:]'  => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x41-\x5A])',
                '[:^word:]'   => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x5F\x61-\x7A])',
                '[:^xdigit:]' => '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF\x30-\x39\x41-\x46\x61-\x66])',

            }->{$1};
        }
        elsif ($char[$i] =~ m/\A \\ ($q_char) \z/oxms) {
            $char[$i] = $1;
        }
    }

    # open character list
    my @singleoctet = ();
    my @charlist    = ();
    for (my $i=0; $i <= $#char; ) {

        # escaped -
        if (defined($char[$i+1]) and ($char[$i+1] eq '...')) {
            $i += 1;
            next;
        }
        elsif ($char[$i] eq '...') {

            # range error
            if ((length($char[$i-1]) > length($char[$i+1])) or ($char[$i-1] gt $char[$i+1])) {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            # range of single octet code and not ignore case
            if ((length($char[$i-1]) == 1) and (length($char[$i+1]) == 1) and ($modifier !~ m/i/oxms)) {
                my $a = unpack 'C', $char[$i-1];
                my $z = unpack 'C', $char[$i+1];

                if ($a == $z) {
                    push @singleoctet, sprintf('\x%02X',        $a);
                }
                elsif (($a+1) == $z) {
                    push @singleoctet, sprintf('\x%02X\x%02X',  $a, $z);
                }
                else {
                    push @singleoctet, sprintf('\x%02X-\x%02X', $a, $z);
                }
            }

            # range of multiple octet code
            elsif (length($char[$i-1]) == length($char[$i+1])) {
                push @charlist, _octets(length($char[$i-1]), $char[$i-1], $char[$i+1], $modifier);
            }
            elsif (length($char[$i-1]) == 1) {
                if (length($char[$i+1]) == 2) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(1, $char[$i-1], &maxchar(1), $modifier),
                        _octets(2, &minchar(2), &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 2) {
                if (length($char[$i+1]) == 3) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), $char[$i+1], $modifier);
                }
                elsif (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(2, $char[$i-1], &maxchar(2), $modifier),
                        _octets(3, &minchar(3), &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            elsif (length($char[$i-1]) == 3) {
                if (length($char[$i+1]) == 4) {
                    push @charlist,
                        _octets(3, $char[$i-1], &maxchar(3), $modifier),
                        _octets(4, &minchar(4), $char[$i+1], $modifier);
                }
            }
            else {
                croak "$0: invalid [] range \"\\x" . unpack('H*',$char[$i-1]) . '-\\x' . unpack('H*',$char[$i+1]) . '" in regexp';
            }

            $i += 2;
        }

        # /i modifier
        elsif ($char[$i] =~ m/\A [\x00-\xFF] \z/oxms) {
            if ($modifier =~ m/i/oxms) {
                my $uc = uc($char[$i]);
                my $lc = lc($char[$i]);
                if ($uc ne $lc) {
                    push @singleoctet, $uc, $lc;
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

        # \h \v
        #
        # P.114 Character Class Shortcuts
        # in Chapter 7: In the World of Regular Expressions
        # of ISBN 978-0-596-52010-6 Learning Perl, Fifth Edition

        elsif ($char[$i] =~ m/\A (?: \\h ) \z/oxms) {
            push @singleoctet, "\t", "\x20";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: \\v ) \z/oxms) {
            push @singleoctet, "\f","\n","\r";
            $i += 1;
        }
        elsif ($char[$i] =~ m/\A (?: \\d | \\s | \\w ) \z/oxms) {
            push @singleoctet, $char[$i];
            $i += 1;
        }

        # single character of multiple octet code
        else {
            push @charlist, $char[$i];
            $i += 1;
        }
    }

    # quote metachar
    for (@singleoctet) {
        if (m/\A \n \z/oxms) {
            $_ = '\n';
        }
        elsif (m/\A \r \z/oxms) {
            $_ = '\r';
        }
        elsif (m/\A ([\x00-\x20\x7F-\xFF]) \z/oxms) {
            $_ = sprintf('\x%02X', CORE::ord $1);
        }
        elsif (m/\A [\x00-\xFF] \z/oxms) {
            $_ = quotemeta $_;
        }
    }

    # return character list
    return \@singleoctet, \@charlist;
}

#
# UTF-2 octal escape sequence
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
# UTF-2 hexadecimal escape sequence
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
# UTF-2 open character list for qr
#
sub charlist_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@singleoctet) == 0) {
    }
    elsif (scalar(@singleoctet) >= 2) {
        push @charlist, '[' . join('',@singleoctet) . ']';
    }
    elsif ($singleoctet[0] =~ m/ . - . /oxms) {
        push @charlist, '[' . $singleoctet[0] . ']';
    }
    else {
        push @charlist, $singleoctet[0];
    }
    if (scalar(@charlist) >= 2) {
        return '(?:' . join('|', @charlist) . ')';
    }
    else {
        return $charlist[0];
    }
}

#
# UTF-2 open character list for not qr
#
sub charlist_not_qr {

    my $modifier = pop @_;
    my @char = @_;

    my($singleoctet, $charlist) = _charlist(@char, $modifier);
    my @singleoctet = @$singleoctet;
    my @charlist    = @$charlist;

    # return character list
    if (scalar(@charlist) >= 1) {
        if (scalar(@singleoctet) >= 1) {

            # any character other than multiple octet and single octet character class
            return '(?!' . join('|', @charlist) . ')(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF'. join('', @singleoctet) . '])';
        }
        else {

            # any character other than multiple octet character class
            return '(?!' . join('|', @charlist) . ")(?:$your_char)";
        }
    }
    else {
        if (scalar(@singleoctet) >= 1) {

            # any character other than single octet character class
            return                                 '(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF'. join('', @singleoctet) . '])';
        }
        else {

            # any character
            return                                 "(?:$your_char)";
        }
    }
}

#
# UTF-2 order to character (with parameter)
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
# UTF-2 order to character (without parameter)
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
# UTF-2 path globbing (with parameter)
#
sub Char::Eutf2::glob($) {

    return _dosglob(@_);
}

#
# UTF-2 path globbing (without parameter)
#
sub Char::Eutf2::glob_() {

    return _dosglob();
}

#
# UTF-2 path globbing from File::DosGlob module
#
my %iter;
my %entries;
sub _dosglob {

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
                  { $ENV{'HOME'} || $ENV{'USERPROFILE'} || "$ENV{'HOMEDRIVE'}$ENV{'HOMEPATH'}" }oxmse;
    }

    # UNIX-like system
    else {
        $expr =~ s{ \A ~ ( (?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF/])* ) }
                  { $1 ? (getpwnam($1))[7] : ($ENV{'HOME'} || $ENV{'LOGDIR'} || (getpwuid($<))[7]) }oxmse;
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
# UTF-2 path globbing subroutine
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
        if ($expr =~ m/\A " ((?:$q_char)*) " \z/oxms) {
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
            if ($expr =~ s# \A ((?:[A-Za-z]:)?) ((?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF/\\]) #$1./$2#oxms) {
                $fix_drive_relative_paths = 1;
            }
        }

        if (($head, $tail) = _parse_path($expr,$pathsep)) {
            if ($tail eq '') {
                push @glob, $expr;
                next OUTER;
            }
            if ($head =~ m/ \A (?:$q_char)*? [*?] /oxms) {
                if (@globdir = _do_glob('d', $head)) {
                    push @glob, _do_glob($cond, map {"$_$pathsep$tail"} @globdir);
                    next OUTER;
                }
            }
            if ($head eq '' or $head =~ m/\A [A-Za-z]: \z/oxms) {
                $head .= $pathsep;
            }
            $expr = $tail;
        }

        # If file component has no wildcards, we can avoid opendir
        if ($expr !~ m/ \A (?:$q_char)*? [*?] /oxms) {
            if ($head eq '.') {
                $head = '';
            }
            if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
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
        if ($head ne '' and ($head =~ m/ \G ($q_char) /oxmsg)[-1] ne $pathsep) {
            $head .= $pathsep;
        }

        my $pattern = '';
        while ($expr =~ m/ \G ($q_char) /oxgc) {
            my $char = $1;
            if ($char eq '*') {
                $pattern .= "(?:$your_char)*",
            }
            elsif ($char eq '?') {
                $pattern .= "(?:$your_char)?",  # DOS style
#               $pattern .= "(?:$your_char)",   # UNIX style
            }
            elsif ((my $uc = uc($char)) ne $char) {
                $pattern .= $uc;
            }
            else {
                $pattern .= quotemeta $char;
            }
        }
        my $matchsub = sub { uc($_[0]) =~ m{\A $pattern \z}xms };

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
# UTF-2 parse line
#
sub _parse_line {

    my($line) = @_;

    $line .= ' ';
    my @piece = ();
    while ($line =~ m{
        " ( (?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF"]   )*  ) " \s+ |
          ( (?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF"\s] )*  )   \s+
        }oxmsg
    ) {
        push @piece, defined($1) ? $1 : $2;
    }
    return @piece;
}

#
# UTF-2 parse path
#
sub _parse_path {

    my($path,$pathsep) = @_;

    $path .= '/';
    my @subpath = ();
    while ($path =~ m{
        ((?: (?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x00-\xFF]|[^\x80-\xFF/\\] )+?) [/\\] }oxmsg
    ) {
        push @subpath, $1;
    }

    my $tail = pop @subpath;
    my $head = join $pathsep, @subpath;
    return $head, $tail;
}

#
# UTF-2 character to order (with parameter)
#
sub Char::UTF2::ord(;$) {

    local $_ = shift if @_;

    if (m/\A ($q_char) /oxms) {
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
# UTF-2 character to order (without parameter)
#
sub Char::UTF2::ord_() {

    if (m/\A ($q_char) /oxms) {
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
# UTF-2 reverse
#
sub Char::UTF2::reverse(@) {

    if (wantarray) {
        return CORE::reverse @_;
    }
    else {
        return join '', CORE::reverse(join('',@_) =~ m/\G ($q_char) /oxmsg);
    }
}

#
# UTF-2 length by character
#
sub Char::UTF2::length(;$) {

    local $_ = shift if @_;

    local @_ = m/\G ($q_char) /oxmsg;
    return scalar @_;
}

#
# UTF-2 substr by character
#
sub Char::UTF2::substr($$;$$) {

    my @char = $_[0] =~ m/\G ($q_char) /oxmsg;

    # substr($string,$offset,$length,$replacement)
    if (@_ == 4) {
        my(undef,$offset,$length,$replacement) = @_;
        my $substr = join '', splice(@char, $offset, $length, $replacement);
        $_[0] = join '', @char;
        return $substr;
    }

    # substr($string,$offset,$length)
    elsif (@_ == 3) {
        my(undef,$offset,$length) = @_;
        if ($length == 0) {
            return '';
        }
        if ($offset >= 0) {
            return join '', (@char[$offset            .. $#char])[0 .. $length-1];
        }
        else {
            return join '', (@char[($#char+$offset+1) .. $#char])[0 .. $length-1];
        }
    }

    # substr($string,$offset)
    else {
        my(undef,$offset) = @_;
        if ($offset >= 0) {
            return join '', @char[$offset            .. $#char];
        }
        else {
            return join '', @char[($#char+$offset+1) .. $#char];
        }
    }
}

#
# UTF-2 index by character
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
# UTF-2 rindex by character
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
sub carp(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
}

#
# instead of Carp::croak
#
sub croak(@) {
    my($package,$filename,$line) = caller(1);
    print STDERR "@_ at $filename line $line.\n";
    die "\n";
}

#
# instead of Carp::cluck
#
sub cluck(@) {
    my $i = 0;
    my @cluck = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @cluck, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @cluck;
    print STDERR "\n";
    carp @_;
}

#
# instead of Carp::confess
#
sub confess(@) {
    my $i = 0;
    my @confess = ();
    while (my($package,$filename,$line,$subroutine) = caller($i)) {
        push @confess, "[$i] $filename($line) $package::$subroutine\n";
        $i++;
    }
    print STDERR reverse @confess;
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

Please patches and report problems to author are welcome.

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

  Scans a UTF-2 $string for delimiters that match pattern and splits the UTF-2
  $string into a list of substrings, returning the resulting list value in list
  context, or the count of substrings in scalar context. The delimiters are
  determined by repeated pattern matching, using the regular expression given in
  pattern, so the delimiters may be of any size and need not be the same UTF-2
  $string on every match. If the pattern doesn't match at all, Char::Eutf2::split returns
  the original UTF-2 $string as a single substring. If it matches once, you get
  two substrings, and so on.
  If $limit is specified and is not negative, the function splits into no more than
  that many fields. If $limit is negative, it is treated as if an arbitrarily large
  $limit has been specified. If $limit is omitted, trailing null fields are stripped
  from the result (which potential users of pop would do well to remember).
  If UTF-2 $string is omitted, the function splits the $_ UTF-2 string.
  If $patten is also omitted, the function splits on whitespace, /\s+/, after
  skipping any leading whitespace.
  If the pattern contains parentheses, then the substring matched by each pair of
  parentheses is included in the resulting list, interspersed with the fields that
  are ordinarily returned.

=item Transliteration

  $tr = Char::Eutf2::tr($variable,$bind_operator,$searchlist,$replacementlist,$modifier);
  $tr = Char::Eutf2::tr($variable,$bind_operator,$searchlist,$replacementlist);

  This function scans a UTF-2 string character by character and replaces all
  occurrences of the characters found in $searchlist with the corresponding character
  in $replacementlist. It returns the number of characters replaced or deleted.
  If no UTF-2 string is specified via =~ operator, the $_ variable is translated.
  $modifier are:

  Modifier   Meaning
  ------------------------------------------------------
  c          Complement $searchlist
  d          Delete found but unreplaced characters
  s          Squash duplicate replaced characters
  ------------------------------------------------------

=item Chop string

  $chop = Char::Eutf2::chop(@list);
  $chop = Char::Eutf2::chop();
  $chop = Char::Eutf2::chop;

  Chops off the last character of a UTF-2 string contained in the variable (or
  UTF-2 strings in each element of a @list) and returns the character chopped.
  The Char::Eutf2::chop operator is used primarily to remove the newline from the end of
  an input record but is more efficient than s/\n$//. If no argument is given, the
  function chops the $_ variable.

=item Index string

  $pos = Char::Eutf2::index($string,$substr,$position);
  $pos = Char::Eutf2::index($string,$substr);

  Returns the position of the first occurrence of $substr in UTF-2 $string.
  The start, if specified, specifies the $position to start looking in the UTF-2
  $string. Positions are integer numbers based at 0. If the substring is not found,
  the Char::Eutf2::index function returns -1.

=item Reverse index string

  $pos = Char::Eutf2::rindex($string,$substr,$position);
  $pos = Char::Eutf2::rindex($string,$substr);

  Works just like Char::Eutf2::index except that it returns the position of the last
  occurence of $substr in UTF-2 $string (a reverse index). The function returns
  -1 if not found. $position, if specified, is the rightmost position that may be
  returned, i.e., how far in the UTF-2 string the function can search.

=item Make capture number

  $capturenumber = Char::Eutf2::capture($string);

  This function is internal use to m/ /, s/ / /, split / / and qr/ /.

=item Make character

  $chr = Char::Eutf2::chr($code);
  $chr = Char::Eutf2::chr_;

  This function returns the character represented by that $code in the character
  set. For example, Char::Eutf2::chr(65) is "A" in either ASCII or UTF-2, not Unicode,
  and Char::Eutf2::chr(0x82a0) is a UTF-2 HIRAGANA LETTER A. For the reverse of
  Char::Eutf2::chr, use Char::UTF2::ord.

=item Filename expansion (globbing)

  @glob = Char::Eutf2::glob($string);
  @glob = Char::Eutf2::glob_;

  Performs filename expansion (DOS-like globbing) on $string, returning the next
  successive name on each call. If $string is omitted, $_ is globbed instead.
  This function function when the pathname ends with chr(0x5C) on MSWin32.

  For example, C<<..\\l*b\\file/*glob.p?>> on MSWin32 or UNIX will work as
  expected (in that it will find something like '..\lib\File/DosGlob.pm'
  alright).
  Note that all path components are
  case-insensitive, and that backslashes and forward slashes are both accepted,
  and preserved. You may have to double the backslashes if you are putting them in
  literally, due to double-quotish parsing of the pattern by perl.
  A tilde ("~") expands to the current user's home directory.

  Spaces in the argument delimit distinct patterns, so C<glob('*.exe *.dll')> globs
  all filenames that end in C<.exe> or C<.dll>. If you want to put in literal spaces
  in the glob pattern, you can escape them with either double quotes.
  e.g. C<glob('c:/"Program Files"/*/*.dll')>.

=cut

