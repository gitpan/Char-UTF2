package UTF2;
######################################################################
#
# UTF2 - Source code filter to escape UTF-8 script
#
# http://search.cpan.org/dist/Char-UTF2/
#
# Copyright (c) 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015 INABA Hitoshi <ina@cpan.org>
######################################################################

use 5.00503;    # Galapagos Consensus 1998 for primetools
# use 5.008001; # Lancaster Consensus 2013 for toolchains

# 12.3. Delaying use Until Runtime
# in Chapter 12. Packages, Libraries, and Modules
# of ISBN 0-596-00313-7 Perl Cookbook, 2nd Edition.
# (and so on)

BEGIN { CORE::eval q{ use vars qw($VERSION) } }
$VERSION = sprintf '%d.%02d', q$Revision: 1.02 $ =~ /(\d+)/oxmsg;

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

BEGIN { CORE::require Eutf2; }

# instead of Symbol.pm
BEGIN {
    my $genpkg = "Symbol::";
    my $genseq = 0;
    sub gensym () {
        my $name = "GEN" . $genseq++;

        # here, no strict qw(refs); if strict.pm exists

        my $ref = \*{$genpkg . $name};
        delete $$genpkg{$name};
        $ref;
    }
}

# Column: local $@
# in Chapter 9. Osaete okitai Perl no kiso
# of ISBN 10: 4798119172 | ISBN 13: 978-4798119175 MODAN Perl NYUMON
# (and so on)

# use strict; if strict.pm exists
BEGIN {
    if (CORE::eval { local $@; CORE::require strict }) {
        strict::->import;
    }
}

# P.714 29.2.39. flock
# in Chapter 29: Functions
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# P.863 flock
# in Chapter 27: Functions
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

# P.228 Inlining Constant Functions
# in Chapter 6: Subroutines
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# P.331 Inlining Constant Functions
# in Chapter 7: Subroutines
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

sub LOCK_SH() {1}
sub LOCK_EX() {2}
sub LOCK_UN() {8}
sub LOCK_NB() {4}

sub unimport {}
sub UTF2::escape_script;

# 6.18. Matching Multiple-Byte Characters
# in Chapter 6. Pattern Matching
# of ISBN 978-1-56592-243-3 Perl Perl Cookbook.
# (and so on)

# regexp of character
my $qq_char   = qr/\\c[\x40-\x5F]|\\?(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[\x00-\x7F\xF5-\xFF])/oxms;
my  $q_char   = qr/(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[\x00-\x7F\xF5-\xFF]/oxms;

# when this script is main program
if ($0 eq __FILE__) {

    # show usage
    unless (@ARGV) {
        die <<END;
$0: usage

perl $0 UTF-8_script.pl > Escaped_script.pl.e
END
    }

    print UTF2::escape_script($ARGV[0]);
    exit 0;
}

my($package,$filename,$line,$subroutine,$hasargs,$wantarray,$evaltext,$is_require,$hints,$bitmask) = caller 0;

# called any package not main
if ($package ne 'main') {
    die <<END;
@{[__FILE__]}: escape by manually command '$^X @{[__FILE__]} "$filename" > "@{[__PACKAGE__]}::$filename"'
and rewrite "use $package;" to "use @{[__PACKAGE__]}::$package;" of script "$0".
END
}

# P.302 Module Privacy and the Exporter
# in Chapter 11: Modules
# of ISBN 0-596-00027-8 Programming Perl Third Edition.
#
# A module can do anything it jolly well pleases when it's used, since use just
# calls the ordinary import method for the module, and you can define that
# method to do anything you like.

# P.406 Module Privacy and the Exporter
# in Chapter 11: Modules
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.
#
# A module can do anything it jolly well pleases when it's used, since use just
# calls the ordinary import method for the module, and you can define that
# method to do anything you like.

sub import {

    if (-e("$filename.e")) {
        if (exists $ENV{'SJIS_DEBUG'}) {
            unlink "$filename.e";
        }
        elsif (-z("$filename.e")) {
            unlink "$filename.e";
        }
        else {

            #----------------------------------------------------
            #  older >
            #  newer >>>>>
            #----------------------------------------------------
            # Filter >
            # Source >>>>>
            # Escape >>>   needs re-escape (Source was changed)
            #
            # Filter >>>
            # Source >>>>>
            # Escape >     needs re-escape (Source was changed)
            #
            # Filter >>>>>
            # Source >>>
            # Escape >     needs re-escape (Source was changed)
            #
            # Filter >>>>>
            # Source >
            # Escape >>>   needs re-escape (Filter was changed)
            #
            # Filter >
            # Source >>>
            # Escape >>>>> executable without re-escape
            #
            # Filter >>>
            # Source >
            # Escape >>>>> executable without re-escape
            #----------------------------------------------------

            my $mtime_filter = (stat(__FILE__     ))[9];
            my $mtime_source = (stat($filename    ))[9];
            my $mtime_escape = (stat("$filename.e"))[9];
            if (($mtime_escape < $mtime_source) or ($mtime_escape < $mtime_filter)) {
                unlink "$filename.e";
            }
        }
    }

    if (not -e("$filename.e")) {
        my $fh = gensym();

        if (CORE::eval q{ use Fcntl qw(O_WRONLY O_APPEND O_CREAT); 1 } and CORE::sysopen($fh,"$filename.e",&O_WRONLY|&O_APPEND|&O_CREAT)) {
        }
        else {
            Eutf2::_open_a($fh, "$filename.e") or die __FILE__, ": Can't write open file: $filename.e";
        }

        if (0) {
        }
        elsif (exists $ENV{'SJIS_NONBLOCK'}) {

            # P.419 File Locking
            # in Chapter 16: Interprocess Communication
            # of ISBN 0-596-00027-8 Programming Perl Third Edition.

            # P.524 File Locking
            # in Chapter 15: Interprocess Communication
            # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

            # P.571 Handling Race Conditions
            # in Chapter 23: Security
            # of ISBN 0-596-00027-8 Programming Perl Third Edition.

            # P.663 Handling Race Conditions
            # in Chapter 20: Security
            # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

            # (and so on)

            CORE::eval q{
                unless (flock($fh, LOCK_EX | LOCK_NB)) {
                    warn __FILE__, ": Can't immediately write-lock the file: $filename.e";
                    exit;
                }
            };
        }
        else {
            CORE::eval q{ flock($fh, LOCK_EX) };
        }

        CORE::eval q{ truncate($fh, 0) };
        seek($fh, 0, 0) or die __FILE__, ": Can't seek file: $filename.e";

        my $e_script = UTF2::escape_script($filename);
        print {$fh} $e_script;

        my $mode = (stat($filename))[2] & 0777;
        chmod $mode, "$filename.e";

        close($fh) or die __FILE__, ": Can't close file: $filename.e";
    }

    my $fh = gensym();
    Eutf2::_open_r($fh, "$filename.e") or die __FILE__, ": Can't read open file: $filename.e";

    if (0) {
    }
    elsif (exists $ENV{'SJIS_NONBLOCK'}) {
        CORE::eval q{
            unless (flock($fh, LOCK_SH | LOCK_NB)) {
                warn __FILE__, ": Can't immediately read-lock the file: $filename.e";
                exit;
            }
        };
    }
    else {
        CORE::eval q{ flock($fh, LOCK_SH) };
    }

    my @switch = ();
    if ($^W) {
        push @switch, '-w';
    }

    # P.707 29.2.33. exec
    # in Chapter 29: Functions
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.
    #
    # If there is more than one argument in LIST, or if LIST is an array with more
    # than one value, the system shell will never be used. This also bypasses any
    # shell processing of the command. The presence or absence of metacharacters in
    # the arguments doesn't affect this list-triggered behavior, which makes it the
    # preferred from in security-conscious programs that do not with to expose
    # themselves to potential shell escapes.
    # Environment variable PERL5SHELL(Microsoft ports only) will never be used, too.

    # P.855 exec
    # in Chapter 27: Functions
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.
    #
    # If there is more than one argument in LIST, or if LIST is an array with more
    # than one value, the system shell will never be used. This also bypasses any
    # shell processing of the command. The presence or absence of metacharacters in
    # the arguments doesn't affect this list-triggered behavior, which makes it the
    # preferred from in security-conscious programs that do not wish to expose
    # themselves to injection attacks via shell escapes.
    # Environment variable PERL5SHELL(Microsoft ports only) will never be used, too.

    # P.489 #! and Quoting on Non-Unix Systems
    # in Chapter 19: The Command-Line Interface
    # of ISBN 0-596-00027-8 Programming Perl Third Edition.

    # P.578 #! and Quoting on Non-Unix Systems
    # in Chapter 17: The Command-Line Interface
    # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

    # DOS-like system
    if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
        exit Eutf2::_systemx(
            _escapeshellcmd_MSWin32($^X),

        # -I switch can not treat space included path
        #   (map { '-I' . _escapeshellcmd_MSWin32($_) } @INC),
            (map { '-I' .                         $_  } @INC),

            @switch,
            '--',
            map { _escapeshellcmd_MSWin32($_) } "$filename.e", @ARGV
        );
    }

    # UNIX-like system
    else {
        exit Eutf2::_systemx(
            _escapeshellcmd($^X),
            (map { '-I' . _escapeshellcmd($_) } @INC),
            @switch,
            '--',
            map { _escapeshellcmd($_) } "$filename.e", @ARGV
        );
    }
}

# escape shell command line on DOS-like system
sub _escapeshellcmd_MSWin32 {
    my($word) = @_;
    if ($word =~ / [ ] /oxms) {
        return qq{"$word"};
    }
    else {
        return $word;
    }
}

# escape shell command line on UNIX-like system
sub _escapeshellcmd {
    my($word) = @_;
    return $word;
}

# P.619 Source Filters
# in Chapter 24: Common Practices
# of ISBN 0-596-00027-8 Programming Perl Third Edition.

# P.718 Source Filters
# in Chapter 21: Common Practices
# of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

# escape UTF-8 script
sub UTF2::escape_script {
    my($script) = @_;
    my $e_script = '';

    # read UTF-8 script
    my $fh = gensym();
    Eutf2::_open_r($fh, $script) or die __FILE__, ": Can't open file: $script";
    local $/ = undef; # slurp mode
    $_ = <$fh>;
    close($fh) or die __FILE__, ": Can't close file: $script";

    if (/^ use Eutf2(?:\s+[0-9\.]*)?\s*; $/oxms) {
        return $_;
    }
    else {

        # #! shebang line
        if (s/\A(#!.+?\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # DOS-like system header
        if (s/\A(\@rem\s*=\s*'.*?'\s*;\s*\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # P.618 Generating Perl in Other Languages
        # in Chapter 24: Common Practices
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        # P.717 Generating Perl in Other Languages
        # in Chapter 21: Common Practices
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        if (s/(.*^#\s*line\s+\d+(?:\s+"(?:$q_char)+?")?\s*\n)//oms) {
            my $head = $1;
            $head =~ s/\bjperl\b/perl/gi;
            $e_script .= $head;
        }

        # P.210 5.10.3.3. Match-time code evaluation
        # in Chapter 5: Pattern Matching
        # of ISBN 0-596-00027-8 Programming Perl Third Edition.

        # P.255 Match-time code evaluation
        # in Chapter 5: Pattern Matching
        # of ISBN 978-0-596-00492-7 Programming Perl 4th Edition.

        # '...' quote to avoid "Octal number in vector unsupported" on perl 5.6

        $e_script .= sprintf("use Eutf2 '%s.0'; # 'quote' for perl5.6\n", $UTF2::VERSION); # require run-time routines version

        # use UTF2 version qw(ord reverse getc);
        if (s/^ \s* use \s+ UTF2 \s* ([^\x80-\xFF;]*) ; \s* \n? $//oxms) {

            # require version
            my $list = $1;
            if ($list =~ s/\A ([0-9]+\.[0-9]+) \.0 \s* //oxms) {
                my $version = $1;
                if ($version ne $UTF2::VERSION) {
                    my @file = grep -e, map {qq{$_/UTF2.pm}} @INC;
                    my %file = map { $_ => 1 } @file;
                    if (scalar(keys %file) >= 2) {
                        my $file = join "\n", sort keys %file;
                        warn <<END;
****************************************************
                   C A U T I O N

              CONFLICT UTF2.pm FILE

$file
****************************************************

END
                    }
                    die "Script $0 expects UTF2.pm $version, but @{[__FILE__]} is version $UTF2::VERSION\n";
                }
                $e_script .= qq{die "Script \$0 expects Eutf2.pm $version, but \\\$Eutf2::VERSION is \$Eutf2::VERSION" if \$Eutf2::VERSION ne '$version';\n};
            }
            elsif ($list =~ s/\A ([0-9]+(?:\.[0-9]*)) \s* //oxms) {
                my $version = $1;
                if ($version > $UTF2::VERSION) {
                    die "Script $0 required UTF2.pm $version, but @{[__FILE__]} is only version $UTF2::VERSION\n";
                }
            }

            # demand ord, reverse, and getc
            if ($list !~ /\A \s* \z/oxms) {
                local $@;
                my @list = CORE::eval $list;
                for (@list) {
                    $Eutf2::function_ord     = 'UTF2::ord'     if /\A ord \z/oxms;
                    $Eutf2::function_ord_    = 'UTF2::ord_'    if /\A ord \z/oxms;
                    $Eutf2::function_reverse = 'UTF2::reverse' if /\A reverse \z/oxms;
                    $Eutf2::function_getc    = 'UTF2::getc'    if /\A getc \z/oxms;
                }
            }
        }
    }

    $e_script .= UTF2::escape();

    return $e_script;
}

1;

__END__

=pod

=head1 NAME

UTF2 - Source code filter to escape UTF-8 script

=head1 Install and Usage

There are two steps there:

=over 2

=item * You'll have to download UTF2.pm and Eutf2.pm and put it in your perl lib directory.

=item * You'll need to write "use UTF2;" at head of the script.

=back

=head1 SYNOPSIS

  use UTF2;
  use UTF2 ver.sion;             --- require minimum version
  use UTF2 ver.sion.0;           --- expects version (match or die)
  use UTF2 qw(ord reverse getc); --- demand enhanced feature of ord, reverse, and getc
  use UTF2 ver.sion qw(ord reverse getc);
  use UTF2 ver.sion.0 qw(ord reverse getc);

  # "no UTF2;" not supported

  or

  $ perl UTF2.pm UTF-8_script.pl > Escaped_script.pl.e

  then

  $ perl Escaped_script.pl.e

  UTF-8_script.pl  --- script written in UTF-8
  Escaped_script.pl.e --- escaped script

  subroutines:
    UTF2::ord(...);
    UTF2::reverse(...);
    UTF2::getc(...);
    UTF2::length(...);
    UTF2::substr(...);
    UTF2::index(...);
    UTF2::eval(...);

  functions:
    <*>
    glob(...);
    CORE::chop(...);
    CORE::ord(...);
    CORE::reverse(...);
    CORE::getc(...);
    CORE::index(...);
    CORE::rindex(...);

  dummy functions:
    utf8::upgrade(...);
    utf8::downgrade(...);
    utf8::encode(...);
    utf8::decode(...);
    utf8::is_utf8(...);
    utf8::valid(...);
    bytes::chr(...);
    bytes::index(...);
    bytes::length(...);
    bytes::ord(...);
    bytes::rindex(...);
    bytes::substr(...);

=head1 ABSTRACT

UTF2 software is "middleware" between perl interpreter and your Perl script
written in UTF-8.

Perl is optimized for problems which are about 90% working with text and about
10% everything else. Even if this "text" doesn't contain UTF-8, Perl3 or later
can treat UTF-8 as binary data.

By "use UTF2;", it automatically interpret your script as UTF-8. The various
functions of perl including a regular expression can treat UTF-8 now.
The function length treats length per byte. This software does not use UTF8
flag.

=head1 Yet Another Future Of

JPerl is very useful software. -- Oops, note, this "JPerl" means "Japanized Perl"
or "Japanese Perl". Therefore, it is unrelated to JPerl of the following.

 JPerl is an implementation of Perl written in Java.
 http://www.javainc.com/projects/jperl/
 
 jPerl - Perl on the JVM
 http://www.dzone.com/links/175948.html
 
 Jamie's PERL scripts for bioinformatics
 http://code.google.com/p/jperl/
 
 jperl (Jonathan Perl)
 https://github.com/jperl

Now, the last version of JPerl is 5.005_04 and is not maintained now.

Japanization modifier WATANABE Hirofumi said,

  "Because WATANABE am tired I give over maintaing JPerl."

at Slide #15: "The future of JPerl" of

L<ftp://ftp.oreilly.co.jp/pcjp98/watanabe/jperlconf.ppt>

in The Perl Confernce Japan 1998.

When I heard it, I thought that someone excluding me would maintain JPerl.
And I slept every night hanging a sock. Night and day, I kept having hope.
After 10 years, I noticed that white beard exists in the sock :-)

This software is a source code filter to escape Perl script encoded by UTF-8
given from STDIN or command line parameter. The character code is never converted
by escaping the script. Neither the value of the character nor the length of the
character string change even if it escapes.

I learned the following things from the successful software.

=over 2

=item * Upper Compatibility like Perl4 to Perl5

=item * Maximum Portability like jcode.pl

=item * Remains One Language Handling Raw UTF-8, Doesn't Use UTF8 flag like JPerl

=item * Remains One Interpreter like Encode module

=item * Code Set Independent like Ruby

=item * Monolithic Script like cpanminus

=item * There's more than one way to do it like Perl itself

=back

I am excited about this software and Perl's future --- I hope you are too.

=head1 JRE: JPerl Runtime Environment

  +---------------------------------------+
  |        JPerl Application Script       | Your Script
  +---------------------------------------+
  |  Source Code Filter, Runtime Routine  | ex. UTF2.pm, Eutf2.pm
  +---------------------------------------+
  |          PVM 5.00503 or later         | ex. perl 5.00503
  +---------------------------------------+

A Perl Virtual Machine (PVM) enables a set of computer software programs and
data structures to use a virtual machine model for the execution of other
computer programs and scripts. The model used by a PVM accepts a form of
computer intermediate language commonly referred to as Perl byteorientedcode.
This language conceptually represents the instruction set of a byte-oriented,
capability architecture.

=head1 Basic Idea of Source Code Filter

I discovered this mail again recently.

[Tokyo.pm] jus Benkyoukai

http://mail.pm.org/pipermail/tokyo-pm/1999-September/001854.html

save as: SJIS.pm

  package SJIS;
  use Filter::Util::Call;
  sub multibyte_filter {
      my $status;
      if (($status = filter_read()) > 0 ) {
          s/([\x81-\x9f\xe0-\xef])([\x40-\x7e\x80-\xfc])/
              sprintf("\\x%02x\\x%02x",ord($1),ord($2))
          /eg;
      }
      $status;
  }
  sub import {
      filter_add(\&multibyte_filter);
  }
  1;

I am glad that I could confirm my idea is not so wrong.

=head1 Command-line Wildcard Expansion on DOS-like Systems

The default command shells on DOS-like systems (COMMAND.COM or cmd.exe or
Win95Cmd.exe) do not expand wildcard arguments supplied to programs. Instead,
import of Eutf2.pm works well.

   in Eutf2.pm
   #
   # @ARGV wildcard globbing
   #
   sub import {

       if ($^O =~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
           my @argv = ();
           for (@ARGV) {

               # has space
               if (/\A (?:$q_char)*? [ ] /oxms) {
                   if (my @glob = Eutf2::glob(qq{"$_"})) {
                       push @argv, @glob;
                   }
                   else {
                       push @argv, $_;
                   }
               }

               # has wildcard metachar
               elsif (/\A (?:$q_char)*? [*?] /oxms) {
                   if (my @glob = Eutf2::glob($_)) {
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

=head1 Software Composition

   UTF2.pm               --- source code filter to escape UTF-8
   Eutf2.pm              --- run-time routines for UTF2.pm

=head1 Upper Compatibility by Escaping

This software adds the function by 'Escaping' it always, and nothing of the
past is broken. Therefore, 'Possible job' never becomes 'Impossible job'.
This approach is effective in the field where the retreat is never permitted.
It means incompatible upgrade of Perl should be rewound.

=head1 Escaping Your Script (You do)

You need write 'use UTF2;' in your script.

  ---------------------------------
  Before      You do
  ---------------------------------
  (nothing)   use UTF2;
  ---------------------------------

=head1 Multiple-Octet Anchoring of Regular Expression

This software requires valid UTF8-encoded Unicode instead of using a
multi-octet anchoring.

=head1 Multiple-Octet Character Regular Expression (UTF2 software provides)

UTF2 software clusters multiple-octet character with quantifier, makes cluster from
multiple-octet custom character classes. And makes multiple-octet version metasymbol
from classic Perl character class shortcuts and POSIX-style character classes.

  --------------------------------------------------------------------------------
  Before                  After
  --------------------------------------------------------------------------------
  m/...MULTIOCT+.../      m/...(?:MULTIOCT)+.../
  m/...[AN-EM].../        m/...(?:A[N-Z]|[B-D][A-Z]|E[A-M]).../
  m/...\D.../             m/...(?:${Eutf2::eD}).../
  m/...[[:^digit:]].../   m/...(?:${Eutf2::not_digit}).../
  --------------------------------------------------------------------------------

=head1 Calling 'Eutf2::ignorecase()' (UTF2 software provides)

UTF2 software applies calling 'Eutf2::ignorecase()' instead of /i modifier.

  --------------------------------------------------------------------------------
  Before                  After
  --------------------------------------------------------------------------------
  m/...$var.../i          m/...@{[Eutf2::ignorecase($var)]}.../
  --------------------------------------------------------------------------------

=head1 Character-Oriented Regular Expression

Regular expression works as character-oriented that has no /b modifier.

  --------------------------------------------------------------------------------
  Before                  After
  --------------------------------------------------------------------------------
  /regexp/                /ditto$Eutf2::matched/
  m/regexp/               m/ditto$Eutf2::matched/
  ?regexp?                m?ditto$Eutf2::matched?
  m?regexp?               m?ditto$Eutf2::matched?
 
  $_ =~                   ($_ =~ m/ditto$Eutf2::matched/) ?
  s/regexp/replacement/   CORE::eval{ Eutf2::s_matched(); local $^W=0; my $__r=qq/replacement/; $_="${1}$__r$'"; 1 } :
                          undef
 
  $_ !~                   ($_ !~ m/ditto$Eutf2::matched/) ?
  s/regexp/replacement/   1 :
                          CORE::eval{ Eutf2::s_matched(); local $^W=0; my $__r=qq/replacement/; $_="${1}$__r$'"; undef }
 
  split(/regexp/)         Eutf2::split(qr/regexp/)
  split(m/regexp/)        Eutf2::split(qr/regexp/)
  split(qr/regexp/)       Eutf2::split(qr/regexp/)
  qr/regexp/              qr/ditto$Eutf2::matched/
  --------------------------------------------------------------------------------

=head1 Byte-Oriented Regular Expression

Regular expression works as byte-oriented that has /b modifier.

  --------------------------------------------------------------------------------
  Before                  After
  --------------------------------------------------------------------------------
  /regexp/b               /(?:regexp)$Eutf2::matched/
  m/regexp/b              m/(?:regexp)$Eutf2::matched/
  ?regexp?b               m?regexp$Eutf2::matched?
  m?regexp?b              m?regexp$Eutf2::matched?
 
  $_ =~                   ($_ =~ m/(\G[\x00-\xFF]*?)(?:regexp)$Eutf2::matched/) ?
  s/regexp/replacement/b  CORE::eval{ Eutf2::s_matched(); local $^W=0; my $__r=qq/replacement/; $_="${1}$__r$'"; 1 } :
                          undef
 
  $_ !~                   ($_ !~ m/(\G[\x00-\xFF]*?)(?:regexp)$Eutf2::matched/) ?
  s/regexp/replacement/b  1 :
                          CORE::eval{ Eutf2::s_matched(); local $^W=0; my $__r=qq/replacement/; $_="${1}$__r$'"; undef }
 
  split(/regexp/b)        split(qr/regexp/)
  split(m/regexp/b)       split(qr/regexp/)
  split(qr/regexp/b)      split(qr/regexp/)
  qr/regexp/b             qr/(?:regexp)$Eutf2::matched/
  --------------------------------------------------------------------------------

=head1 Escaping Character Classes (Eutf2.pm provides)

The character classes are redefined as follows to backward compatibility.

  ---------------------------------------------------------------
  Before        After
  ---------------------------------------------------------------
   .            ${Eutf2::dot}
                ${Eutf2::dot_s}    (/s modifier)
  \d            [0-9]              (universally)
  \s            \s
  \w            [0-9A-Z_a-z]       (universally)
  \D            ${Eutf2::eD}
  \S            ${Eutf2::eS}
  \W            ${Eutf2::eW}
  \h            [\x09\x20]
  \v            [\x0A\x0B\x0C\x0D]
  \H            ${Eutf2::eH}
  \V            ${Eutf2::eV}
  \C            [\x00-\xFF]
  \X            X                  (so, just 'X')
  \R            ${Eutf2::eR}
  \N            ${Eutf2::eN}
  ---------------------------------------------------------------

Also POSIX-style character classes.

  ---------------------------------------------------------------
  Before        After
  ---------------------------------------------------------------
  [:alnum:]     [\x30-\x39\x41-\x5A\x61-\x7A]
  [:alpha:]     [\x41-\x5A\x61-\x7A]
  [:ascii:]     [\x00-\x7F]
  [:blank:]     [\x09\x20]
  [:cntrl:]     [\x00-\x1F\x7F]
  [:digit:]     [\x30-\x39]
  [:graph:]     [\x21-\x7F]
  [:lower:]     [\x61-\x7A]
                [\x41-\x5A\x61-\x7A]     (/i modifier)
  [:print:]     [\x20-\x7F]
  [:punct:]     [\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E]
  [:space:]     [\s\x0B]
  [:upper:]     [\x41-\x5A]
                [\x41-\x5A\x61-\x7A]     (/i modifier)
  [:word:]      [\x30-\x39\x41-\x5A\x5F\x61-\x7A]
  [:xdigit:]    [\x30-\x39\x41-\x46\x61-\x66]
  [:^alnum:]    ${Eutf2::not_alnum}
  [:^alpha:]    ${Eutf2::not_alpha}
  [:^ascii:]    ${Eutf2::not_ascii}
  [:^blank:]    ${Eutf2::not_blank}
  [:^cntrl:]    ${Eutf2::not_cntrl}
  [:^digit:]    ${Eutf2::not_digit}
  [:^graph:]    ${Eutf2::not_graph}
  [:^lower:]    ${Eutf2::not_lower}
                ${Eutf2::not_lower_i}    (/i modifier)
  [:^print:]    ${Eutf2::not_print}
  [:^punct:]    ${Eutf2::not_punct}
  [:^space:]    ${Eutf2::not_space}
  [:^upper:]    ${Eutf2::not_upper}
                ${Eutf2::not_upper_i}    (/i modifier)
  [:^word:]     ${Eutf2::not_word}
  [:^xdigit:]   ${Eutf2::not_xdigit}
  ---------------------------------------------------------------

\b and \B are redefined as follows to backward compatibility.

  ---------------------------------------------------------------
  Before      After
  ---------------------------------------------------------------
  \b          ${Eutf2::eb}
  \B          ${Eutf2::eB}
  ---------------------------------------------------------------

Definitions in Eutf2.pm.

  ---------------------------------------------------------------------------------------------------------------------------------------------------------
  After                    Definition
  ---------------------------------------------------------------------------------------------------------------------------------------------------------
  ${Eutf2::dot}            qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A])}
  ${Eutf2::dot_s}          qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])}
  ${Eutf2::eD}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF0-9])}
  ${Eutf2::eS}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\s])}
  ${Eutf2::eW}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF0-9A-Z_a-z])}
  ${Eutf2::eH}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x20])}
  ${Eutf2::eV}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A\x0B\x0C\x0D])}
  ${Eutf2::eR}             qr{(?:\x0D\x0A|[\x0A\x0D])}
  ${Eutf2::eN}             qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x0A])}
  ${Eutf2::not_alnum}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x61-\x7A])}
  ${Eutf2::not_alpha}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x41-\x5A\x61-\x7A])}
  ${Eutf2::not_ascii}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x00-\x7F])}
  ${Eutf2::not_blank}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x09\x20])}
  ${Eutf2::not_cntrl}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x00-\x1F\x7F])}
  ${Eutf2::not_digit}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39])}
  ${Eutf2::not_graph}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x21-\x7F])}
  ${Eutf2::not_lower}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x61-\x7A])}
  ${Eutf2::not_lower_i}    qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])}
  ${Eutf2::not_print}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x20-\x7F])}
  ${Eutf2::not_punct}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x21-\x2F\x3A-\x3F\x40\x5B-\x5F\x60\x7B-\x7E])}
  ${Eutf2::not_space}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\s\x0B])}
  ${Eutf2::not_upper}      qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x41-\x5A])}
  ${Eutf2::not_upper_i}    qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF])}
  ${Eutf2::not_word}       qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x5A\x5F\x61-\x7A])}
  ${Eutf2::not_xdigit}     qr{(?:(?:[\xC2-\xDF]|[\xE0-\xE0][\xA0-\xBF]|[\xE1-\xEC][\x80-\xBF]|[\xED-\xED][\x80-\x9F]|[\xEE-\xEF][\x80-\xBF]|[\xF0-\xF0][\x90-\xBF][\x80-\xBF]|[\xF1-\xF3][\x80-\xBF][\x80-\xBF]|[\xF4-\xF4][\x80-\x8F][\x80-\xBF])[\x80-\xBF]|[^\x80-\xFF\x30-\x39\x41-\x46\x61-\x66])}
  ${Eutf2::eb}             qr{(?:\A(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[0-9A-Z_a-z])|(?<=[0-9A-Z_a-z])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]|\z))}
  ${Eutf2::eB}             qr{(?:(?<=[0-9A-Z_a-z])(?=[0-9A-Z_a-z])|(?<=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF])(?=[\x00-\x2F\x40\x5B-\x5E\x60\x7B-\xFF]))}
  ---------------------------------------------------------------------------------------------------------------------------------------------------------

=head1 Un-Escaping \ of \N, \p, \P, and \X (UTF2 software provides)

UTF2 software removes '\' at head of alphanumeric regexp metasymbols \N, \p, \P
and \X. By this method, you can avoid the trap of the abstraction.

See also,
Deprecate literal unescaped "{" in regexes.
http://perl5.git.perl.org/perl.git/commit/2a53d3314d380af5ab5283758219417c6dfa36e9

  ------------------------------------
  Before           After
  ------------------------------------
  \N{CHARNAME}     N\{CHARNAME}
  \p{L}            p\{L}
  \p{^L}           p\{^L}
  \p{\^L}          p\{\^L}
  \pL              pL
  \P{L}            P\{L}
  \P{^L}           P\{^L}
  \P{\^L}          P\{\^L}
  \PL              PL
  \X               X
  ------------------------------------

=head1 Escaping Built-in Functions (UTF2 software provide)

Insert 'Eutf2::' at head of function name. Eutf2.pm provides your script Eutf2::*
subroutines.

  -------------------------------------------
  Before      After            Works as
  -------------------------------------------
  length      length           Byte
  substr      substr           Byte
  pos         pos              Byte
  split       Eutf2::split     Character
  tr///       Eutf2::tr        Character
  tr///b      tr///            Byte
  tr///B      tr///            Byte
  y///        Eutf2::tr        Character
  y///b       tr///            Byte
  y///B       tr///            Byte
  chop        Eutf2::chop      Character
  lc          Eutf2::lc        Character
  lcfirst     Eutf2::lcfirst   Character
  uc          Eutf2::uc        Character
  ucfirst     Eutf2::ucfirst   Character
  fc          Eutf2::fc        Character
  chr         Eutf2::chr       Character
  glob        Eutf2::glob      Character
  -------------------------------------------

  ------------------------------------------------------------------------------------------------------------------------
  Before                   After
  ------------------------------------------------------------------------------------------------------------------------
  use Perl::Module;        BEGIN { require 'Perl/Module.pm'; Perl::Module->import() if Perl::Module->can('import'); }
  use Perl::Module @list;  BEGIN { require 'Perl/Module.pm'; Perl::Module->import(@list) if Perl::Module->can('import'); }
  use Perl::Module ();     BEGIN { require 'Perl/Module.pm'; }
  no Perl::Module;         BEGIN { require 'Perl/Module.pm'; Perl::Module->unimport() if Perl::Module->can('unimport'); }
  no Perl::Module @list;   BEGIN { require 'Perl/Module.pm'; Perl::Module->unimport(@list) if Perl::Module->can('unimport'); }
  no Perl::Module ();      BEGIN { require 'Perl/Module.pm'; }
  ------------------------------------------------------------------------------------------------------------------------

=head1 Escaping Function Name (You do)

You need write 'UTF2::' at head of function name when you want character-
oriented subroutine. See 'Character-Oriented Subroutines'.

  --------------------------------------------------------
  Function   Character-Oriented   Description
  --------------------------------------------------------
  ord        UTF2::ord
  reverse    UTF2::reverse
  getc       UTF2::getc
  length     UTF2::length
  substr     UTF2::substr
  index      UTF2::index          See 'About Indexes'
  rindex     UTF2::rindex         See 'About Rindexes'
  --------------------------------------------------------

  About Indexes
  -------------------------------------------------------------------------
  Function       Works as    Returns as   Description
  -------------------------------------------------------------------------
  index          Character   Byte         JPerl semantics (most useful)
  (same as Eutf2::index)
  UTF2::index    Character   Character    Character-oriented semantics
  CORE::index    Byte        Byte         Byte-oriented semantics
  (nothing)      Byte        Character    (most useless)
  -------------------------------------------------------------------------

  About Rindexes
  -------------------------------------------------------------------------
  Function       Works as    Returns as   Description
  -------------------------------------------------------------------------
  rindex         Character   Byte         JPerl semantics (most useful)
  (same as Eutf2::rindex)
  UTF2::rindex   Character   Character    Character-oriented semantics
  CORE::rindex   Byte        Byte         Byte-oriented semantics
  (nothing)      Byte        Character    (most useless)
  -------------------------------------------------------------------------

=head1 Character-Oriented Subsroutines

=over 2

=item * Ordinal Value of Character

  $ord = UTF2::ord($string);

  This subroutine returns the numeric value (ASCII or UTF-8 character) of the
  first character of $string, not Unicode. If $string is omitted, it uses $_.
  The return value is always unsigned.

  If you import ord "use UTF2 qw(ord);", ord of your script will be rewritten in
  UTF2::ord. UTF2::ord is not compatible with ord of JPerl.

=item * Reverse List or String

  @reverse = UTF2::reverse(@list);
  $reverse = UTF2::reverse(@list);

  In list context, this subroutine returns a list value consisting of the elements
  of @list in the opposite order.

  In scalar context, the subroutine concatenates all the elements of @list and
  then returns the reverse of that resulting string, character by character.

  If you import reverse "use UTF2 qw(reverse);", reverse of your script will be
  rewritten in UTF2::reverse. UTF2::reverse is not compatible with reverse of
  JPerl.

  Even if you do not know this subroutine, there is no problem. This subroutine
  can be created with

  $rev = join('', reverse(split(//, $jstring)));

  as before.

  See:
  P.558 JPerl (Japanese Perl)
  Appendix C Supplement the Japanese version
  ISBN 4-89052-384-7 PERL PUROGURAMINGU

=item * Returns Next Character

  $getc = UTF2::getc(FILEHANDLE);
  $getc = UTF2::getc($filehandle);
  $getc = UTF2::getc;

  This subroutine returns the next character from the input file attached to
  FILEHANDLE. It returns undef at end-of-file, or if an I/O error was encountered.
  If FILEHANDLE is omitted, the subroutine reads from STDIN.

  This subroutine is somewhat slow, but it's occasionally useful for
  single-character input from the keyboard -- provided you manage to get your
  keyboard input unbuffered. This subroutine requests unbuffered input from the
  standard I/O library. Unfortunately, the standard I/O library is not so standard
  as to provide a portable way to tell the underlying operating system to supply
  unbuffered keyboard input to the standard I/O system. To do that, you have to
  be slightly more clever, and in an operating-system-dependent fashion. Under
  Unix you might say this:

  if ($BSD_STYLE) {
      system "stty cbreak </dev/tty >/dev/tty 2>&1";
  }
  else {
      system "stty", "-icanon", "eol", "\001";
  }

  $key = UTF2::getc;

  if ($BSD_STYLE) {
      system "stty -cbreak </dev/tty >/dev/tty 2>&1";
  }
  else {
      system "stty", "icanon", "eol", "^@"; # ASCII NUL
  }
  print "\n";

  This code puts the next character typed on the terminal in the string $key. If
  your stty program has options like cbreak, you'll need to use the code where
  $BSD_STYLE is true. Otherwise, you'll need to use the code where it is false.

  If you import getc "use UTF2 qw(getc);", getc of your script will be rewritten
  in UTF2::getc. UTF2::getc is not compatible with getc of JPerl.

=item * Length by UTF-8 Character

  $length = UTF2::length($string);
  $length = UTF2::length();

  This subroutine returns the length in characters (programmer-visible characters)
  of the scalar value $string. If $string is omitted, it returns the UTF2::length
  of $_.

  Do not try to use UTF2::length to find the size of an array or hash. Use scalar
  @array for the size of an array, and scalar keys %hash for the number of key/value
  pairs in a hash. (The scalar is typically omitted when redundant.)

  To find the length of a string in bytes rather than characters, say simply:

  $bytes = length($string);

  Even if you do not know this subroutine, there is no problem. This subroutine
  can be created with

  $len = split(//, $jstring);

  as before.

  See:
  P.558 JPerl (Japanese Perl)
  Appendix C Supplement the Japanese version
  ISBN 4-89052-384-7 PERL PUROGURAMINGU

=item * Substr by UTF-8 Character

  $substr = UTF2::substr($string,$offset,$length,$replacement);
  $substr = UTF2::substr($string,$offset,$length);
  $substr = UTF2::substr($string,$offset);

  This subroutine extracts a substring out of the string given by $string and returns
  it. The substring is extracted starting at $offset characters from the front of
  the string. First character is at offset zero. If $offset is negative, starts that
  far back from the end of the string.
  If $length is omitted, returns everything through the end of the string. If $length
  is negative, leaves that many characters off the end of the string. Otherwise,
  $length indicates the length of the substring to extract, which is sort of what
  you'd expect.

  my $s = "The black cat climbed the green tree";
  my $color  = UTF2::substr $s, 4, 5;      # black
  my $middle = UTF2::substr $s, 4, -11;    # black cat climbed the
  my $end    = UTF2::substr $s, 14;        # climbed the green tree
  my $tail   = UTF2::substr $s, -4;        # tree
  my $z      = UTF2::substr $s, -4, 2;     # tr

  If Perl version 5.14 or later, you can use the UTF2::substr() subroutine as an
  lvalue. In its case $string must itself be an lvalue. If you assign something
  shorter than $length, the string will shrink, and if you assign something longer
  than $length, the string will grow to accommodate it. To keep the string the
  same length, you may need to pad or chop your value using sprintf.

  If $offset and $length specify a substring that is partly outside the string,
  only the part within the string is returned. If the substring is beyond either
  end of the string, UTF2::substr() returns the undefined value and produces a
  warning. When used as an lvalue, specifying a substring that is entirely outside
  the string raises an exception. Here's an example showing the behavior for
  boundary cases:

  my $name = 'fred';
  UTF2::substr($name, 4) = 'dy';         # $name is now 'freddy'
  my $null = UTF2::substr $name, 6, 2;   # returns "" (no warning)
  my $oops = UTF2::substr $name, 7;      # returns undef, with warning
  UTF2::substr($name, 7) = 'gap';        # raises an exception

  An alternative to using UTF2::substr() as an lvalue is to specify the replacement
  string as the 4th argument. This allows you to replace parts of the $string and
  return what was there before in one operation, just as you can with splice().

  my $s = "The black cat climbed the green tree";
  my $z = UTF2::substr $s, 14, 7, "jumped from";    # climbed
  # $s is now "The black cat jumped from the green tree"

  Note that the lvalue returned by the three-argument version of UTF2::substr() acts
  as a 'magic bullet'; each time it is assigned to, it remembers which part of the
  original string is being modified; for example:

  $x = '1234';
  for (UTF2::substr($x,1,2)) {
      $_ = 'a';   print $x,"\n";    # prints 1a4
      $_ = 'xyz'; print $x,"\n";    # prints 1xyz4
      $x = '56789';
      $_ = 'pq';  print $x,"\n";    # prints 5pq9
  }

  With negative offsets, it remembers its position from the end of the string when
  the target string is modified:

  $x = '1234';
  for (UTF2::substr($x, -3, 2)) {
      $_ = 'a';   print $x,"\n";    # prints 1a4, as above
      $x = 'abcdefg';
      print $_,"\n";                # prints f
  }

  Prior to Perl version 5.10, the result of using an lvalue multiple times was
  unspecified. Prior to 5.16, the result with negative offsets was unspecified.

=item * Index by UTF-8 Character

  $index = UTF2::index($string,$substring,$offset);
  $index = UTF2::index($string,$substring);

  This subroutine searches for one string within another. It returns the character
  position of the first occurrence of $substring in $string. The $offset, if
  specified, says how many characters from the start to skip before beginning to
  look. Positions are based at 0. If the substring is not found, the subroutine
  returns one less than the base, ordinarily -1. To work your way through a string,
  you might say:

  $pos = -1;
  while (($pos = UTF2::index($string, $lookfor, $pos)) > -1) {
      print "Found at $pos\n";
      $pos++;
  }

=item * Rindex by UTF-8 Character

  $rindex = UTF2::rindex($string,$substring,$offset);
  $rindex = UTF2::rindex($string,$substring);

  This subroutine works just like UTF2::index except that it returns the character
  position of the last occurrence of $substring in $string (a reverse UTF2::index).
  The subroutine returns -1 if $substring is not found. $offset, if specified, is
  the rightmost character position that may be returned. To work your way through
  a string backward, say:

  $pos = UTF2::length($string);
  while (($pos = UTF2::rindex($string, $lookfor, $pos)) >= 0) {
      print "Found at $pos\n";
      $pos--;
  }

=item * Eval UTF-8 Script

  $eval = UTF2::eval { block };
  $eval = UTF2::eval $expr;
  $eval = UTF2::eval;

  The UTF2::eval keyword serves two distinct but related purposes in JPerl.
  These purposes are represented by two forms of syntax, UTF2::eval { block }
  and UTF2::eval $expr. The first form traps runtime exceptions (errors)
  that would otherwise prove fatal, similar to the "try block" construct in
  C++ or Java. The second form compiles and executes little bits of code on
  the fly at runtime, and also (conveniently) traps any exceptions just like
  the first form. But the second form runs much slower than the first form,
  since it must parse the string every time. On the other hand, it is also
  more general. Whichever form you use, UTF2::eval is the preferred way to do
  all exception handling in JPerl.

  For either form of UTF2::eval, the value returned from an UTF2::eval is
  the value of the last expression evaluated, just as with subroutines.
  Similarly, you may use the return operator to return a value from the
  middle of the eval. The expression providing the return value is evaluated
  in void, scalar, or list context, depending on the context of the
  UTF2::eval itself. See wantarray for more on how the evaluation context
  can be determined.

  If there is a trappable error (including any produced by the die operator),
  UTF2::eval returns undef and puts the error message (or object) in $@. If
  there is no error, $@ is guaranteed to be set to the null string, so you
  can test it reliably afterward for errors. A simple Boolean test suffices:

      UTF2::eval { ... }; # trap runtime errors
      if ($@) { ... }     # handle error

  (Prior to Perl 5.16, a bug caused undef to be returned in list context for
  syntax errors, but not for runtime errors.)

  The UTF2::eval { block } form is syntax checked and compiled at compile time,
  so it is just as efficient at runtime as any other block. (People familiar
  with the slow UTF2::eval $expr form are occasionally confused on this issue.)
  Because the { block } is compiled when the surrounding code is, this form of
  UTF2::eval cannot trap syntax errors.

  The UTF2::eval $expr form can trap syntax errors because it parses the code
  at runtime. (If the parse is unsuccessful, it places the parse error in $@,
  as usual.) If $expr is omitted, evaluates $_ .

  Otherwise, it executes the value of $expr as though it were a little JPerl
  script. The code is executed in the context of the current of the current
  JPerl script, which means that it can see any enclosing lexicals from a
  surrounding scope, and that any nonlocal variable settings remain in effect
  after the UTF2::eval is complete, as do any subroutine or format definitions.
  The code of the UTF2::eval is treated as a block, so any locally scoped
  variables declared within the UTF2::eval last only until the UTF2::eval is
  done. (See my and local.) As with any code in a block, a final semicolon is
  not required.

  UTF2::eval will be escaped as follows:

  -------------------------------------------------
  Before                  After
  -------------------------------------------------
  UTF2::eval { block }    eval { block }
  UTF2::eval $expr        eval UTF2::escape $expr
  UTF2::eval              eval UTF2::escape
  -------------------------------------------------

  To tell the truth, the subroutine UTF2::eval does not exist. If it exists,
  you will troubled, when UTF2::eval has a parameter that is single quoted
  string included my variables. UTF2::escape is a subroutine that makes Perl
  script from JPerl script.

  Here is a simple JPerl shell. It prompts the user to enter a string of
  arbitrary JPerl code, compiles and executes that string, and prints whatever
  error occurred:

      #!/usr/bin/perl
      # jperlshell.pl - simple JPerl shell
      use UTF2;
      print "\nEnter some JPerl code: ";
      while (<STDIN>) {
          UTF2::eval;
          print $@;
          print "\nEnter some more JPerl code: ";
      }

  Here is a rename.pl script to do a mass renaming of files using a JPerl
  expression:

      #!/usr/bin/perl
      # rename.pl - change filenames
      use UTF2;
      $op = shift;
      for (@ARGV) {
          $was = $_;
          UTF2::eval $op;
          die if $@;
          # next line calls the built-in function, not
          # the script by the same name
          if ($was ne $_) {
              print STDERR "rename $was --> $_\n";
              rename($was,$_);
          }
      }

  You'd use that script like this:

      C:\WINDOWS> perl rename.pl 's/\.orig$//' *.orig
      C:\WINDOWS> perl rename.pl 'y/A-Z/a-z/ unless /^Make/' *
      C:\WINDOWS> perl rename.pl '$_ .= ".bad"' *.f

  Since UTF2::eval traps errors that would otherwise prove fatal, it is useful
  for determining whether particular features (such as fork or symlink) are
  implemented.

  Because UTF2::eval { block } is syntax checked at compile time, any syntax
  error is reported earlier. Therefore, if your code is invariant and both
  UTF2::eval $expr and UTF2::eval { block } will suit your purposes equally
  well, the { block } form is preferred. For example:

      # make divide-by-zero nonfatal
      UTF2::eval { $answer = $a / $b; };
      warn $@ if $@;

      # same thing, but less efficient if run multiple times
      UTF2::eval '$answer = $a / $b';
      warn $@ if $@;

      # a compile-time syntax error (not trapped)
      UTF2::eval { $answer = }; # WRONG

      # a runtime syntax error
      UTF2::eval '$answer =';   # sets $@

  Here, the code in the { block } has to be valid JPerl code to make it past
  the compile phase. The code in the $expr doesn't get examined until runtime,
  so it doesn't cause an error until runtime.

  Using the UTF2::eval { block } form as an exception trap in libraries does
  have some issues. Due to the current arguably broken state of __DIE__ hooks,
  you may wish not to trigger any __DIE__ hooks that user code may have
  installed. You can use the local $SIG{__DIE__} construct for this purpose,
  as this example shows:

      # a private exception trap for divide-by-zero
      UTF2::eval { local $SIG{'__DIE__'}; $answer = $a / $b; };
      warn $@ if $@;

  This is especially significant, given that __DIE__ hooks can call die again,
  which has the effect of changing their error messages:

      # __DIE__ hooks may modify error messages
      {
          local $SIG{'__DIE__'} =
              sub { (my $x = $_[0]) =~ s/foo/bar/g; die $x };
          UTF2::eval { die "foo lives here" };
          print $@ if $@;                # prints "bar lives here"
      }

  Because this promotes action at a distance, this counterintuitive behavior
  may be fixed in a future release.

  With an UTF2::eval, you should be especially careful to remember what's being
  looked at when:

      UTF2::eval $x;        # CASE 1
      UTF2::eval "$x";      # CASE 2

      UTF2::eval '$x';      # CASE 3
      UTF2::eval { $x };    # CASE 4

      UTF2::eval "\$$x++";  # CASE 5
      $$x++;                # CASE 6

  CASEs 1 and 2 above behave identically: they run the code contained in the
  variable $x. (Although CASE 2 has misleading double quotes making the reader
  wonder what else might be happening (nothing is).) CASEs 3 and 4 likewise
  behave in the same way: they run the code '$x' , which does nothing but return
  the value of $x. (CASE 4 is preferred for purely visual reasons, but it also
  has the advantage of compiling at compile-time instead of at run-time.) CASE 5
  is a place where normally you would like to use double quotes, except that in
  this particular situation, you can just use symbolic references instead, as
  in CASE 6.

  Before Perl 5.14, the assignment to $@ occurred before restoration of
  localized variables, which means that for your code to run on older versions,
  a temporary is required if you want to mask some but not all errors:

      # alter $@ on nefarious repugnancy only
      {
          my $e;
          {
              local $@; # protect existing $@
              UTF2::eval { test_repugnancy() };
              # $@ =~ /nefarious/ and die $@; # Perl 5.14 and higher only
              $@ =~ /nefarious/ and $e = $@;
          }
          die $e if defined $e
      }

  The block of UTF2::eval { block } does not count as a loop, so the loop
  control statements next, last, or redo cannot be used to leave or restart the
  block.

=item * Filename Globbing

  @glob = glob($expr);
  $glob = glob($expr);
  @glob = glob;
  $glob = glob;
  @glob = <*>;
  $glob = <*>;

  Performs filename expansion (globbing) on $expr, returning the next successive
  name on each call. If $expr is omitted, $_ is globbed instead.

  This operator is implemented via the Eutf2::glob() subroutine. See Eutf2::glob
  of Eutf2.pm for details.

=back

=head1 Byte-Oriented Functions

=over 2

=item * Chop Byte String

  $byte = CORE::chop($string);
  $byte = CORE::chop(@list);
  $byte = CORE::chop;

  This function chops off the last byte of a string variable and returns the
  byte chopped. The CORE::chop operator is used primarily to remove the newline
  from the end of an input record, and is more efficient than using a
  substitution (s/\n$//). If that's all you're doing, then it would be safer to
  use chomp, since CORE::chop always shortens the string no matter what's there,
  and chomp is more selective.

  You cannot CORE::chop a literal, only a variable.

  If you CORE::chop a @list of variables, each string in the list is chopped:

  @lines = `cat myfile`;
  CORE::chop @lines;

  You can CORE::chop anything that is an lvalue, including an assignment:

  CORE::chop($cwd = `pwd`);
  CORE::chop($answer = <STDIN>);

  This is different from:

  $answer = CORE::chop($temp = <STDIN>); # WRONG

  which puts a newline into $answer because CORE::chop returns the byte chopped,
  not the remaining string (which is in $tmp). One way to get the result
  intended here is with substr:

  $answer = substr <STDIN>, 0, -1;

  But this is more commonly written as:

  CORE::chop($answer = <STDIN>);

  In the most general case, CORE::chop can be expressed in terms of substr:

  $last_byte = CORE::chop($var);
  $last_byte = substr($var, -1, 1, ""); # same thing

  Once you understand this equivalence, you can use it to do bigger chops. To
  CORE::chop more than one byte, use substr as an lvalue, assigning a null
  string. The following removes the last five bytes of $caravan:

  substr($caravan, -5) = "";

  The negative subscript causes substr to count from the end of the string
  instead of the beginning. If you wanted to save the bytes so removed, you
  could use the four-argument form of substr, creating something of a quintuple
  CORE::chop:

  $tail = substr($caravan, -5, 5, "");

  If no argument is given, the function chops the $_ variable.

=item * Ordinal Value of Byte

  $ord = CORE::ord($expr);

  This function returns the numeric value of the first byte of $expr, regardless
  of "use UTF2 qw(ord);" exists or not. If $expr is omitted, it uses $_.
  The return value is always unsigned.

  If you want a signed value, use unpack('c',$expr). If you want all the bytes of
  the string converted to a list of numbers, use unpack('C*',$expr) instead.

=item * Reverse List or Byte String

  @reverse = CORE::reverse(@list);
  $reverse = CORE::reverse(@list);

  In list context, this function returns a list value consisting of the elements
  of @list in the opposite order.

  In scalar context, the function concatenates all the elements of @list and then
  returns the reverse of that resulting string, byte by byte, regardless of
  "use UTF2 qw(reverse);" exists or not.

=item * Returns Next Byte

  $getc = CORE::getc(FILEHANDLE);
  $getc = CORE::getc($filehandle);
  $getc = CORE::getc;

  This function returns the next byte from the input file attached to FILEHANDLE.
  It returns undef at end-of-file, or if an I/O error was encountered. If
  FILEHANDLE is omitted, the function reads from STDIN.

  This function is somewhat slow, but it's occasionally useful for single-byte
  input from the keyboard -- provided you manage to get your keyboard input
  unbuffered. This function requests unbuffered input from the standard I/O library.
  Unfortunately, the standard I/O library is not so standard as to provide a portable
  way to tell the underlying operating system to supply unbuffered keyboard input to
  the standard I/O system. To do that, you have to be slightly more clever, and in
  an operating-system-dependent fashion. Under Unix you might say this:

  if ($BSD_STYLE) {
      system "stty cbreak </dev/tty >/dev/tty 2>&1";
  }
  else {
      system "stty", "-icanon", "eol", "\001";
  }

  $key = CORE::getc;

  if ($BSD_STYLE) {
      system "stty -cbreak </dev/tty >/dev/tty 2>&1";
  }
  else {
      system "stty", "icanon", "eol", "^@"; # ASCII NUL
  }
  print "\n";

  This code puts the next single-byte typed on the terminal in the string $key.
  If your stty program has options like cbreak, you'll need to use the code where
  $BSD_STYLE is true. Otherwise, you'll need to use the code where it is false.

=item * Index by Byte String

  $index = CORE::index($string,$substring,$offset);
  $index = CORE::index($string,$substring);

  This function searches for one byte string within another. It returns the position
  of the first occurrence of $substring in $string. The $offset, if specified, says
  how many bytes from the start to skip before beginning to look. Positions are based
  at 0. If the substring is not found, the function returns one less than the base,
  ordinarily -1. To work your way through a string, you might say:

  $pos = -1;
  while (($pos = CORE::index($string, $lookfor, $pos)) > -1) {
      print "Found at $pos\n";
      $pos++;
  }

=item * Rindex by Byte String

  $rindex = CORE::rindex($string,$substring,$offset);
  $rindex = CORE::rindex($string,$substring);

  This function works just like CORE::index except that it returns the position of
  the last occurrence of $substring in $string (a reverse CORE::index). The function
  returns -1 if not $substring is found. $offset, if specified, is the rightmost
  position that may be returned. To work your way through a string backward, say:

  $pos = CORE::length($string);
  while (($pos = CORE::rindex($string, $lookfor, $pos)) >= 0) {
      print "Found at $pos\n";
      $pos--;
  }

=back

=head1 Un-Escaping bytes::* Subroutines (UTF2 software provides)

UTF2 software removes 'bytes::' at head of subroutine name.

  ---------------------------------------
  Before           After     Works as
  ---------------------------------------
  bytes::chr       chr       Byte
  bytes::index     index     Byte
  bytes::length    length    Byte
  bytes::ord       ord       Byte
  bytes::rindex    rindex    Byte
  bytes::substr    substr    Byte
  ---------------------------------------

=head1 Ignore Pragmas and Modules

  -----------------------------------------------------------
  Before                    After
  -----------------------------------------------------------
  use strict;               use strict; no strict qw(refs);
  use 5.12.0;               use 5.12.0; no strict qw(refs);
  require utf8;             # require utf8;
  require bytes;            # require bytes;
  require charnames;        # require charnames;
  require I18N::Japanese;   # require I18N::Japanese;
  require I18N::Collate;    # require I18N::Collate;
  require I18N::JExt;       # require I18N::JExt;
  require File::DosGlob;    # require File::DosGlob;
  require Wild;             # require Wild;
  require Wildcard;         # require Wildcard;
  require Japanese;         # require Japanese;
  use utf8;                 # use utf8;
  use bytes;                # use bytes;
  use charnames;            # use charnames;
  use I18N::Japanese;       # use I18N::Japanese;
  use I18N::Collate;        # use I18N::Collate;
  use I18N::JExt;           # use I18N::JExt;
  use File::DosGlob;        # use File::DosGlob;
  use Wild;                 # use Wild;
  use Wildcard;             # use Wildcard;
  use Japanese;             # use Japanese;
  no utf8;                  # no utf8;
  no bytes;                 # no bytes;
  no charnames;             # no charnames;
  no I18N::Japanese;        # no I18N::Japanese;
  no I18N::Collate;         # no I18N::Collate;
  no I18N::JExt;            # no I18N::JExt;
  no File::DosGlob;         # no File::DosGlob;
  no Wild;                  # no Wild;
  no Wildcard;              # no Wildcard;
  no Japanese;              # no Japanese;
  -----------------------------------------------------------

  Comment out pragma to ignore utf8 environment, and Eutf2.pm provides these
  functions.

=over 2

=item * Dummy utf8::upgrade

  $num_octets = utf8::upgrade($string);

  Returns the number of octets necessary to represent the string.

=item * Dummy utf8::downgrade

  $success = utf8::downgrade($string[, FAIL_OK]);

  Returns true always.

=item * Dummy utf8::encode

  utf8::encode($string);

  Returns nothing.

=item * Dummy utf8::decode

  $success = utf8::decode($string);

  Returns true always.

=item * Dummy utf8::is_utf8

  $flag = utf8::is_utf8(STRING);

  Returns false always.

=item * Dummy utf8::valid

  $flag = utf8::valid(STRING);

  Returns true always.

=item * Dummy bytes::chr

  This subroutine is same as chr.

=item * Dummy bytes::index

  This subroutine is same as index.

=item * Dummy bytes::length

  This subroutine is same as length.

=item * Dummy bytes::ord

  This subroutine is same as ord.

=item * Dummy bytes::rindex

  This subroutine is same as rindex.

=item * Dummy bytes::substr

  This subroutine is same as substr.

=back

=head1 Environment Variable

 This software uses the flock function for exclusive control. The execution of the
 program is blocked until it becomes possible to read or write the file.
 You can have it not block in the flock function by defining environment variable
 SJIS_NONBLOCK.
 
 Example:
 
   SET SJIS_NONBLOCK=1
 
 (The value '1' doesn't have the meaning)

=head1 BUGS, LIMITATIONS, and COMPATIBILITY

I have tested and verified this software using the best of my ability.
However, a software containing much regular expression is bound to contain
some bugs. Thus, if you happen to find a bug that's in UTF2 software and
not your own program, you can try to reduce it to a minimal test case and
then report it to the following author's address. If you have an idea that
could make this a more useful tool, please let everyone share it.

=over 2

=item * format

Function "format" can't handle multiple-octet code same as original Perl.

=item * cloister of regular expression

The cloister (?s) and (?i) of a regular expression will not be implemented for
the time being. Cloister (?s) can be substituted with the .(dot) and \N on /s
modifier. Cloister (?i) can be substituted with \F...\E.

=item * UTF2::substr as Lvalue

If Perl version is older than 5.14, UTF2::substr differs from CORE::substr, and
cannot be used as a lvalue. To change part of a string, you need use the optional
fourth argument which is the replacement string.

UTF2::substr($string, 13, 4, "JPerl");

=item * Empty Variable in Regular Expression

Unlike literal null string, an interpolated variable evaluated to the empty string
can't use the most recent pattern from a previous successful regular expression.

=item * Limitation of ?? and m??

Multibyte character needs ( ) which is before {n,m}, {n,}, {n}, *, and + in ?? or m??.
As a result, you need to rewrite a script about $1,$2,$3,... You cannot use (?: )
?, {n,m}?, {n,}?, and {n}? in ?? and m??, because delimiter of m?? is '?'.

=item * Modifier /a /d /l and /u of Regular Expression

The concept of this software is not to use two or more encoding methods as
literal string and literal of regexp in one Perl script. Therefore, modifier
/a, /d, /l, and /u are not supported.
\d means [0-9] universally.

=item * Named Character

A named character, such \N{GREEK SMALL LETTER EPSILON}, \N{greek:epsilon}, or
\N{epsilon} is not supported.

=item * Unicode Properties (aka Character Properties) of Regular Expression

Unicode properties (aka character properties) of regexp are not available.
Also (?[]) in regexp of Perl 5.18 is not available. There is no plans to currently
support these.

=item * Delimiter of String and Regexp

qq//, q//, qw//, qx//, qr//, m//, s///, tr///, and y/// can't use a wide character
as the delimiter.

=back

=head1 AUTHOR

INABA Hitoshi E<lt>ina@cpan.orgE<gt>

This project was originated by INABA Hitoshi.

=head1 LICENSE AND COPYRIGHT

This software is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

This software is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=head1 My Goal

P.401 See chapter 15: Unicode
of ISBN 0-596-00027-8 Programming Perl Third Edition.

Before the introduction of Unicode support in perl, The eq operator
just compared the byte-strings represented by two scalars. Beginning
with perl 5.8, eq compares two byte-strings with simultaneous
consideration of the UTF8 flag.

/* You are not expected to understand this */

  Information processing model beginning with perl 5.8
 
    +----------------------+---------------------+
    |     Text strings     |                     |
    +----------+-----------|    Binary strings   |
    |  UTF-8   |  Latin-1  |                     |
    +----------+-----------+---------------------+
    | UTF8     |            Not UTF8             |
    | Flagged  |            Flagged              |
    +--------------------------------------------+
    http://perl-users.jp/articles/advent-calendar/2010/casual/4
 
    (Why is only Latin-1 special?)

This change consequentially made a big gap between a past script and
new script. Both scripts cannot re-use the code mutually any longer.
Because a new method puts a strain in the programmer, it will still take
time to replace all the in existence scripts.

The biggest problem of new method is that the UTF8 flag can't synchronize
to real encode of string. Thus you must debug about UTF8 flag, before
your script. How to solve it by returning to a this method, let's drag out
page 402 of the old dusty Programming Perl, 3rd ed. again.

  Information processing model beginning with perl3 or this software of
  UNIX/C-ism.

    +--------------------------------------------+
    |       Text strings as Binary strings       |
    |       Binary strings as Text strings       |
    +--------------------------------------------+
    |        Not UTF8 Flagged, UNIX/C-ism        |
    +--------------------------------------------+

  Script could be written in native encoding of operating systems.
  - Like contents of a file
  - Like a file name on the file systems
  - Like command lines
  - Like environment variables
  - Like parameters of API

  In UNIX Everything is a File
  - In UNIX everything is a stream of bytes
  - In UNIX the filesystem is used as a universal name space

Ideally, I'd like to achieve these five Goals:

=over 2

=item * Goal #1:

Old byte-oriented programs should not spontaneously break on the old
byte-oriented data they used to work on.

This goal has been achieved by that this software is additional code
for perl like utf8 pragma. Perl should work same as past Perl if added
nothing.

=item * Goal #2:

Old byte-oriented programs should magically start working on the new
character-oriented data when appropriate.

Still now, 1 octet is counted with 1 by built-in functions length,
substr, index, rindex, and pos that handle length and position of string.
In this part, there is no change. The length of 1 character of 2 octet
code is 2.

On the other hand, the regular expression in the script is added the
multibyte anchoring processing with this software, instead of you.

figure of Goal #1 and Goal #2.

                               GOAL#1  GOAL#2
                        (a)     (b)     (c)     (d)     (e)
      +--------------+-------+-------+-------+-------+-------+
      | data         |  Old  |  Old  |  New  |  Old  |  New  |
      +--------------+-------+-------+-------+-------+-------+
      | script       |  Old  |      Old      |      New      |
      +--------------+-------+---------------+---------------+
      | interpreter  |  Old  |              New              |
      +--------------+-------+-------------------------------+
      Old --- Old byte-oriented
      New --- New character-oriented

There is a combination from (a) to (e) in data, script, and interpreter
of old and new. Let's add the Encode module and this software did not
exist at time of be written this document and JPerl did exist.

                        (a)     (b)     (c)     (d)     (e)
                                      JPerl,japerl    Encode,UTF2
      +--------------+-------+-------+-------+-------+-------+
      | data         |  Old  |  Old  |  New  |  Old  |  New  |
      +--------------+-------+-------+-------+-------+-------+
      | script       |  Old  |      Old      |      New      |
      +--------------+-------+---------------+---------------+
      | interpreter  |  Old  |              New              |
      +--------------+-------+-------------------------------+
      Old --- Old byte-oriented
      New --- New character-oriented

The reason why JPerl is very excellent is that it is at the position of
(c). That is, it is not necessary to do a special description to the
script to process new character-oriented string.
(May the japerl take over JPerl!)

=item * Goal #3:

Programs should run just as fast in the new character-oriented mode
as in the old byte-oriented mode.

It is impossible. Because the following time is necessary.

(1) Time of escape script for old byte-oriented perl.

Someday, I want to ask Larry Wall about this goal in the elevator.

=item * Goal #4:

Perl should remain one language, rather than forking into a
byte-oriented Perl and a character-oriented Perl.

JPerl remains one Perl language by forking to two interpreters.
However, the Perl core team did not desire fork of the interpreter.
As a result, Perl language forked contrary to goal #4.

A character-oriented perl is not necessary to make it specially,
because a byte-oriented perl can already treat the binary data.
This software is only an application program of byte-oriented Perl,
a filter program.

And you will get support from the Perl community, when you solve the
problem by the Perl script.

UTF2 software remains one language and one interpreter.

=item * Goal #5:

JPerl users will be able to maintain JPerl by Perl.

May the JPerl be with you, always.

=back

Back when Programming Perl, 3rd ed. was written, UTF8 flag was not born
and Perl is designed to make the easy jobs easy. This software provide
programming environment like at that time.

=head1 Perl's motto

   Some computer scientists (the reductionists, in particular) would
  like to deny it, but people have funny-shaped minds. Mental geography
  is not linear, and cannot be mapped onto a flat surface without
  severe distortion. But for the last score years or so, computer
  reductionists have been first bowing down at the Temple of Orthogonality,
  then rising up to preach their ideas of ascetic rectitude to any who
  would listen.
 
   Their fervent but misguided desire was simply to squash your mind to
  fit their mindset, to smush your patterns of thought into some sort of
  Hyperdimensional Flatland. It's a joyless existence, being smushed.
  --- Learning Perl on Win32 Systems

  If you think this is a big headache, you're right. No one likes
  this situation, but Perl does the best it can with the input and
  encodings it has to deal with. If only we could reset history and
  not make so many mistakes next time.
  --- Learning Perl 6th Edition

   The most important thing for most people to know about handling
  Unicode data in Perl, however, is that if you don't ever use any Uni-
  code data -- if none of your files are marked as UTF-8 and you don't
  use UTF-8 locales -- then you can happily pretend that you're back in
  Perl 5.005_03 land; the Unicode features will in no way interfere with
  your code unless you're explicitly using them. Sometimes the twin
  goals of embracing Unicode but not disturbing old-style byte-oriented
  scripts has led to compromise and confusion, but it's the Perl way to
  silently do the right thing, which is what Perl ends up doing.
  --- Advanced Perl Programming, 2nd Edition

=head1 SEE ALSO

 PERL PUROGURAMINGU
 Larry Wall, Randal L.Schwartz, Yoshiyuki Kondo
 December 1997
 ISBN 4-89052-384-7
 http://www.context.co.jp/~cond/books/old-books.html

 Programming Perl, Second Edition
 By Larry Wall, Tom Christiansen, Randal L. Schwartz
 October 1996
 Pages: 670
 ISBN 10: 1-56592-149-6 | ISBN 13: 9781565921498
 http://shop.oreilly.com/product/9781565921498.do

 Programming Perl, Third Edition
 By Larry Wall, Tom Christiansen, Jon Orwant
 Third Edition  July 2000
 Pages: 1104
 ISBN 10: 0-596-00027-8 | ISBN 13: 9780596000271
 http://shop.oreilly.com/product/9780596000271.do

 The Perl Language Reference Manual (for Perl version 5.12.1)
 by Larry Wall and others
 Paperback (6"x9"), 724 pages
 Retail Price: $39.95 (pound 29.95 in UK)
 ISBN-13: 978-1-906966-02-7
 http://www.network-theory.co.uk/perl/language/

 Perl Pocket Reference, 5th Edition
 By Johan Vromans
 Publisher: O'Reilly Media
 Released: July 2011
 Pages: 102
 http://shop.oreilly.com/product/0636920018476.do

 Programming Perl, 4th Edition
 By: Tom Christiansen, brian d foy, Larry Wall, Jon Orwant
 Publisher: O'Reilly Media
 Formats: Print, Ebook, Safari Books Online
 Released: March 2012
 Pages: 1130
 Print ISBN: 978-0-596-00492-7 | ISBN 10: 0-596-00492-3
 Ebook ISBN: 978-1-4493-9890-3 | ISBN 10: 1-4493-9890-1
 http://shop.oreilly.com/product/9780596004927.do

 Perl Cookbook
 By Tom Christiansen, Nathan Torkington
 August 1998
 Pages: 800
 ISBN 10: 1-56592-243-3 | ISBN 13: 978-1-56592-243-3
 http://shop.oreilly.com/product/9781565922433.do

 Perl Cookbook, Second Edition
 By Tom Christiansen, Nathan Torkington
 Second Edition  August 2003
 Pages: 964
 ISBN 10: 0-596-00313-7 | ISBN 13: 9780596003135
 http://shop.oreilly.com/product/9780596003135.do

 Perl in a Nutshell, Second Edition
 By Stephen Spainhour, Ellen Siever, Nathan Patwardhan
 Second Edition  June 2002
 Pages: 760
 Series: In a Nutshell
 ISBN 10: 0-596-00241-6 | ISBN 13: 9780596002411
 http://shop.oreilly.com/product/9780596002411.do

 Learning Perl on Win32 Systems
 By Randal L. Schwartz, Erik Olson, Tom Christiansen
 August 1997
 Pages: 306
 ISBN 10: 1-56592-324-3 | ISBN 13: 9781565923249
 http://shop.oreilly.com/product/9781565923249.do

 Learning Perl, Fifth Edition
 By Randal L. Schwartz, Tom Phoenix, brian d foy
 June 2008
 Pages: 352
 Print ISBN:978-0-596-52010-6 | ISBN 10: 0-596-52010-7
 Ebook ISBN:978-0-596-10316-3 | ISBN 10: 0-596-10316-6
 http://shop.oreilly.com/product/9780596520113.do

 Learning Perl, 6th Edition
 By Randal L. Schwartz, brian d foy, Tom Phoenix
 June 2011
 Pages: 390
 ISBN-10: 1449303587 | ISBN-13: 978-1449303587
 http://shop.oreilly.com/product/0636920018452.do

 Advanced Perl Programming, 2nd Edition
 By Simon Cozens
 June 2005
 Pages: 300
 ISBN-10: 0-596-00456-7 | ISBN-13: 978-0-596-00456-9
 http://shop.oreilly.com/product/9780596004569.do

 Perl RESOURCE KIT UNIX EDITION
 Futato, Irving, Jepson, Patwardhan, Siever
 ISBN 10: 1-56592-370-7
 http://shop.oreilly.com/product/9781565923706.do

 MODAN Perl NYUMON
 By Daisuke Maki
 2009/2/10
 Pages: 344
 ISBN 10: 4798119172 | ISBN 13: 978-4798119175
 http://www.seshop.com/product/detail/10250/

 Understanding Japanese Information Processing
 By Ken Lunde
 January 1900
 Pages: 470
 ISBN 10: 1-56592-043-0 | ISBN 13: 9781565920439
 http://shop.oreilly.com/product/9781565920439.do

 CJKV Information Processing
 Chinese, Japanese, Korean & Vietnamese Computing
 By Ken Lunde
 First Edition  January 1999
 Pages: 1128
 ISBN 10: 1-56592-224-7 | ISBN 13: 9781565922242
 http://shop.oreilly.com/product/9781565922242.do

 Mastering Regular Expressions, Second Edition
 By Jeffrey E. F. Friedl
 Second Edition  July 2002
 Pages: 484
 ISBN 10: 0-596-00289-0 | ISBN 13: 9780596002893
 http://shop.oreilly.com/product/9780596002893.do

 Mastering Regular Expressions, Third Edition
 By Jeffrey E. F. Friedl
 Third Edition  August 2006
 Pages: 542
 ISBN 10: 0-596-52812-4 | ISBN 13:9780596528126
 http://shop.oreilly.com/product/9780596528126.do

 Regular Expressions Cookbook
 By Jan Goyvaerts, Steven Levithan
 May 2009
 Pages: 512
 ISBN 10:0-596-52068-9 | ISBN 13: 978-0-596-52068-7
 http://shop.oreilly.com/product/9780596520694.do

 JIS KANJI JITEN
 By Kouji Shibano
 Pages: 1456
 ISBN 4-542-20129-5
 http://www.webstore.jsa.or.jp/lib/lib.asp?fn=/manual/mnl01_12.htm

 UNIX MAGAZINE
 1993 Aug
 Pages: 172
 T1008901080816 ZASSHI 08901-8
 http://ascii.asciimw.jp/books/books/detail/978-4-7561-5008-0.shtml

 LINUX NIHONGO KANKYO
 By YAMAGATA Hiroo, Stephen J. Turnbull, Craig Oda, Robert J. Bickel
 June, 2000
 Pages: 376
 ISBN 4-87311-016-5
 http://www.oreilly.co.jp/books/4873110165/

 MacPerl Power and Ease
 By Vicki Brown, Chris Nandor
 April 1998
 Pages: 350
 ISBN 10: 1881957322 | ISBN 13: 978-1881957324
 http://www.amazon.com/Macperl-Power-Ease-Vicki-Brown/dp/1881957322

 Windows NT Shell Scripting
 By Timothy Hill
 April 27, 1998
 Pages: 400
 ISBN 10: 1578700477 | ISBN 13: 9781578700479
 http://search.barnesandnoble.com/Windows-NT-Shell-Scripting/Timothy-Hill/e/9781578700479/

 Windows(R) Command-Line Administrators Pocket Consultant, 2nd Edition
 By William R. Stanek
 February 2009
 Pages: 594
 ISBN 10: 0-7356-2262-0 | ISBN 13: 978-0-7356-2262-3
 http://shop.oreilly.com/product/9780735622623.do

 Kaoru Maeda, Perl's history Perl 1,2,3,4
 http://www.slideshare.net/KaoruMaeda/perl-perl-1234

 nurse, What is "string"
 http://d.hatena.ne.jp/nurse/20141107#1415355181

 NISHIO Hirokazu, What's meant "string as a sequence of characters"?
 http://d.hatena.ne.jp/nishiohirokazu/20141107/1415286729

 nurse, History of Japanese EUC 22:00
 http://d.hatena.ne.jp/nurse/20090308/1236517235

 Mike Whitaker, Perl And Unicode
 http://www.slideshare.net/Penfold/perl-and-unicode

 Ricardo Signes, Perl 5.14 for Pragmatists
 http://www.slideshare.net/rjbs/perl-514-8809465

 Ricardo Signes, What's New in Perl? v5.10 - v5.16
 http://www.slideshare.net/rjbs/whats-new-in-perl-v510-v516

 CPAN Directory INABA Hitoshi
 http://search.cpan.org/~ina/

 BackPAN
 http://backpan.perl.org/authors/id/I/IN/INA/

 Recent Perl packages by "INABA Hitoshi"
 http://code.activestate.com/ppm/author:INABA-Hitoshi/

=head1 ACKNOWLEDGEMENTS

This software was made referring to software and the document that the
following hackers or persons had made. 
I am thankful to all persons.

 Rick Yamashita, Shift_JIS
 ttp://furukawablog.spaces.live.com/Blog/cns!1pmWgsL289nm7Shn7cS0jHzA!2225.entry (dead link)
 ttp://shino.tumblr.com/post/116166805/1981-us-jis
 (add 'h' at head)
 http://www.wdic.org/w/WDIC/%E3%82%B7%E3%83%95%E3%83%88JIS

 Larry Wall, Perl
 http://www.perl.org/

 Kazumasa Utashiro, jcode.pl
 ftp://ftp.iij.ad.jp/pub/IIJ/dist/utashiro/perl/
 http://log.utashiro.com/pub/2006/07/jkondo_a580.html

 Jeffrey E. F. Friedl, Mastering Regular Expressions
 http://regex.info/

 SADAHIRO Tomoyuki, The right way of using Shift_JIS
 http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
 http://search.cpan.org/~sadahiro/

 Yukihiro "Matz" Matsumoto, YAPC::Asia2006 Ruby on Perl(s)
 http://www.rubyist.net/~matz/slides/yapc2006/

 jscripter, For jperl users
 http://homepage1.nifty.com/kazuf/jperl.html

 Bruce., Unicode in Perl
 http://www.rakunet.org/tsnet/TSabc/18/546.html

 Hiroaki Izumi, Perl5.8/Perl5.10 is not useful on the Windows.
 http://dl.dropbox.com/u/23756062/perlwin.html
 https://sites.google.com/site/hiroa63iz/perlwin

 TSUKAMOTO Makio, Perl memo/file path of Windows
 http://digit.que.ne.jp/work/wiki.cgi?Perl%E3%83%A1%E3%83%A2%2FWindows%E3%81%A7%E3%81%AE%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%83%91%E3%82%B9

 chaichanPaPa, Matching Shift_JIS file name
 http://d.hatena.ne.jp/chaichanPaPa/20080802/1217660826

 SUZUKI Norio, Jperl
 http://homepage2.nifty.com/kipp/perl/jperl/

 WATANABE Hirofumi, Jperl
 http://www.cpan.org/src/5.0/jperl/
 http://search.cpan.org/~watanabe/
 ftp://ftp.oreilly.co.jp/pcjp98/watanabe/jperlconf.ppt

 Chuck Houpt, Michiko Nozu, MacJPerl
 http://habilis.net/macjperl/index.j.html

 Kenichi Ishigaki, Pod-PerldocJp, Welcome to modern Perl world
 http://search.cpan.org/dist/Pod-PerldocJp/
 http://gihyo.jp/dev/serial/01/modern-perl/0031
 http://gihyo.jp/dev/serial/01/modern-perl/0032
 http://gihyo.jp/dev/serial/01/modern-perl/0033

 Fuji, Goro (gfx), Perl Hackers Hub No.16
 http://gihyo.jp/dev/serial/01/perl-hackers-hub/001602

 Dan Kogai, Encode module
 http://search.cpan.org/dist/Encode/
 http://www.archive.org/details/YAPCAsia2006TokyoPerl58andUnicodeMythsFactsandChanges (video)
 http://yapc.g.hatena.ne.jp/jkondo/ (audio)

 Takahashi Masatuyo, JPerl Wiki
 http://ja.jperl.wikia.com/wiki/JPerl_Wiki

 Juerd, Perl Unicode Advice
 http://juerd.nl/site.plp/perluniadvice

 daily dayflower, 2008-06-25 perluniadvice
 http://d.hatena.ne.jp/dayflower/20080625/1214374293

 Jesse Vincent, Compatibility is a virtue
 http://www.nntp.perl.org/group/perl.perl5.porters/2010/05/msg159825.html

 Tokyo-pm archive
 http://mail.pm.org/pipermail/tokyo-pm/
 http://mail.pm.org/pipermail/tokyo-pm/1999-September/001844.html
 http://mail.pm.org/pipermail/tokyo-pm/1999-September/001854.html

 Error: Runtime exception on jperl 5.005_03
 http://www.rakunet.org/tsnet/TSperl/12/374.html
 http://www.rakunet.org/tsnet/TSperl/12/375.html
 http://www.rakunet.org/tsnet/TSperl/12/376.html
 http://www.rakunet.org/tsnet/TSperl/12/377.html
 http://www.rakunet.org/tsnet/TSperl/12/378.html
 http://www.rakunet.org/tsnet/TSperl/12/379.html
 http://www.rakunet.org/tsnet/TSperl/12/380.html
 http://www.rakunet.org/tsnet/TSperl/12/382.html

 ruby-list
 http://blade.nagaokaut.ac.jp/ruby/ruby-list/index.shtml
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2440
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2446
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/2569
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/9427
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/9431
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10500
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10501
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/10502
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12385
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12392
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/12393
 http://blade.nagaokaut.ac.jp/cgi-bin/scat.rb/ruby/ruby-list/19156

 Object-oriented with Perl
 http://www.freeml.com/perl-oo/486
 http://www.freeml.com/perl-oo/487
 http://www.freeml.com/perl-oo/490
 http://www.freeml.com/perl-oo/491
 http://www.freeml.com/perl-oo/492
 http://www.freeml.com/perl-oo/494
 http://www.freeml.com/perl-oo/514

=cut

# Did you fun enough? ^o^)
