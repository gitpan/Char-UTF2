# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..1\n";

my $__FILE__ = __FILE__;

my $anchor1 = q{\G(?:[\x81-\x9F\xE0-\xFC][\x00-\xFF]|[\x00-\xFF])*?};
my $anchor2 = q{\G(?(?!.{32766})(?:[\x81-\x9F\xE0-\xFC][\x00-\xFF]|[\x00-\xFF])*?|(?(?=[\x00-\x7F]+\z).*?|.*?[^\x81-\x9F\xE0-\xFC](?:[\x81-\x9F\xE0-\xFC][\x00-\xFF])*?))};

if (($] >= 5.010001) or
    (($] >= 5.008) and ($^O eq 'MSWin32') and (defined($ActivePerl::VERSION) and ($ActivePerl::VERSION > 800))) or
    (($] =~ /\A 5\.006/oxms) and ($^O eq 'MSWin32'))
) {
    # avoid: Complex regular subexpression recursion limit (32766) exceeded at here.
    local $^W = 0;

    if (((('A' x 32768).'B') !~ /${anchor1}B/b) and
        ((('A' x 32768).'B') =~ /${anchor2}B/b)
    ) {
        # do test
    }
    else {
        for my $tno (1..1) {
            print "ok - $tno # SKIP $^X $0\n";
        }
        exit;
    }
}
else {
    for my $tno (1..1) {
        print "ok - $tno # SKIP $^X $0\n";
    }
    exit;
}

my $data = <<END;
<dl>
<td>aaa</td>
<dd>12345ソ</dd>
</dl>
END
$data = $data x int(40000 / length($data));

my $bbb = <<END;
<dl>
<td>bbb</td>
<dd>6789</dd>
</dl>
END

my $ccc = <<END;
<dl>
<td>ccc</td>
<dd>6789</dd>
</dl>
END

my $data2 = "$data$bbb";
$data2 =~ s|<td>bbb</td>|<td>ccc</td>|;

if ($data2 eq "$data$ccc") {
    print "ok - 1 $^X $__FILE__\n";
}
else {
    print "not ok - 1 $^X $__FILE__\n";
}

__END__

http://okwave.jp/qa/q6674287.html
Perl ファイル一気読み後の正規表現について
Perlで以下の通り、
htmlファイルを全て読み込んだ後に正規表現を当てたいのですが、うまくいきません。
恐縮ですが、原因をご存知の方いらっしゃいましたら教えて頂けませんでしょうか。
また、他に良い解決方法がありましたら教えて頂けると幸いです。
※PC環境はwindows7, perl5.12です。情報に不足がございましたらご指摘下さい。

-----
#--test.html(左側の数字は行数)
000001 <dl>
000002 <dt>aaa</dt>
000003 <dd>12345</dd>
000004 </dl>

（中略）

120001 <dl>
120002 <dt>bbb</dt>
120003 <dd>6789</dd>
120004 </dl>

#--test.pl
open IN , "test.html";
local $/ = undef;
$data = <IN>;
close IN;

$data =~ s|<td>bbb</td>|<td>ccc</td>|;
print "$data\n";
-----

ファイルの始めの方だと当たるのに、後半では当たりません。
正規表現の対象として大きすぎるんでしょうか・・・。

$data =~ s|<td>aaa</td>|<td>ccc</td>|;
は、当たりますが

$data =~ s|<td>bbb</td>|<td>ccc</td>|;
だと当たりません。

どうぞよろしくお願いいたします。
