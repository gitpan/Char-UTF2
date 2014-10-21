# This file is encoded in UTF-2.
die "This file is not encoded in UTF-2.\n" if q{あ} ne "\xe3\x81\x82";

use Char::UTF2;
print "1..2\n";

my $__FILE__ = __FILE__;

my $input = '  My name is Yamada Taro';

my $space = ' ';
my $a = join '_', split $space, $input;
if ($a eq 'My_name_is_Yamada_Taro') {
    print qq{ok - 1 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 $^X $__FILE__\n};
}

my $b = join '_', split ' ', $input;
if ($b eq 'My_name_is_Yamada_Taro') {
    print qq{ok - 2 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 $^X $__FILE__\n};
}

__END__
http://d.hatena.ne.jp/syohex/20130613/1371103504

変更点

splitの第一引数に空白一つの文字列リテラルを与えたときと

空白一つが代入された変数を指定したときの挙動が今まで違って

いたのが同じになったようです。

Perl 5.16.3での結果
  a = __My_name_is_Yamada_Taro
  b = My_name_is_Yamada_Taro

Perl 5.18.0での結果
  a = My_name_is_Yamada_Taro
  b = My_name_is_Yamada_Taro

むしろ 5.18.0より前はそんな挙動だったのかという感じですが、

一応知っておいた方が良さそうです。
