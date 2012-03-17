# install: apt-get install rabbit
# view: rabbit --use-druby --public-level=all 20110522_kernelvm6.rd
# wiimote: rabbit-wiictl.rb 20110522_kernelvm6.rd
# control: rabbit-command --next
# or: $ irb1.9.1 -r drb/drb
#     irb> rabbit = DRbObject.new_with_uri("druby://localhost:10101")
#     irb> rabbit.send(:toggle_index_mode)
# print: rabbit --print -o 20110522_kernelvm6.pdf 20110522_kernelvm6.rd

= NetBSD manを翻訳しよう!

: subtitle
   契約なんていらないよ!
: author
   Kiwamu Okabe
: theme
   debian-and-tortoise

= ぼくのスペック

  * twitter: ((<@master_q|URL:http://twitter/master_q>))
  * ふだんはDebian
  * 前の仕事でNetBSD使ってた
  * 今プロニート
  * お姉さんHaskell教えてー

== プロパティ

: background-image
   enjoy.jpg
: background-image-relative-width
   100

= Love NetBSD?

  # image
  # src = NetBSD_logo.png
  # relative_height = 85
  # reflect_ratio = 0.8

(('tag:center'))Of course! But...

= 困ってませんか？

  * キレイなソースって((*どのへん？*))
  * そもそもNetBSD kernel((*よく知らない*))
  * ((*日本語の情報*))がまとまってない

== プロパティ

: background-image
   trouble.jpg
: background-image-relative-width
   120

= どうすれば...

  * 仕様書があればそれを読むべき
  * NetBSDの仕様書って何？
  * そりゃあ((*man*))でしょう
  * でも((*英語のmanしかない*))ので情弱(('note:(つまり僕)'))にツラい

== プロパティ

: background-image
   specpaper.jpg
: background-image-relative-width
   150

= 悩みよさようなら!

翻訳プロジェクトを作ったよ!

== プロパティ

: background-image
   gitorious.png
: background-image-relative-width
   90
: background-image-relative-margin-top
   40

= 翻訳manを読むには

UNIXっぽい環境で

  $ git clone git@gitorious.org:netbsd-man-translate\
  /netbsd-man-translate.git
  $ cd netbsd-man-translate
  $ ./tools/man_utf8 translated/src/share/man/ja/man9/cpu_number.9

皆groff 1.21使ってるよね!
"-K"オプション使うよ!

== プロパティ

: background-image
   files.jpg
: background-image-relative-width
   150

= *NIX環境がない人

((<netbsdman.masterq.net|URL:http://netbsdman.masterq.net/>))
どぞ

== プロパティ

: background-image
   netbsdman.png
: background-image-relative-width
   90
: background-image-relative-margin-top
   20

= HTML化手順

  $ pwd
  /home/hoge/doc/netbsd-man-translate
  $ make htmlize
  。。。warning出まくり。。。
  $ chromium html/index.html

((<haml|URL:http://haml.ursm.jp/>))とか使うから入れといてネ!

== プロパティ

: background-image
   haml.jpg
: background-image-relative-width
   140
: background-image-relative-margin-top
   -15

= 翻訳するだけだと

  # blockquote
  # title = http://d.hatena.ne.jp/naruoga/20110305/1302188484
  "「man とかどうせ翻訳なんか付いて来られないんだからおまいら英語見ろ」といって本家の各国語版リポジトリパージされちゃった"

== プロパティ

: background-image
   rain3.jpg
: background-image-relative-width
   150

= 再びどうすれば...

  * オリジナル更新差分管理
  * 翻訳マンパワー確保
  * ブランチ追従

すれば受け入れてもらえる？

== プロパティ

: background-image
   kuma.jpg
: background-image-relative-width
   80
: background-image-relative-margin-top
   10

= 更新差分管理

manpageをgettext化しよう!

== プロパティ

: background-image
   roffflow.png
: background-image-relative-width
   65
: background-image-relative-margin-top
   10

= roffxgettext動作#1

".Sh"とか".Pp"とか出てきたら...

== プロパティ

: background-image
   uvm_9.png
: background-image-relative-width
   70
: background-image-relative-margin-top
   17

= roffxgettext動作#2

msgidに切り取るだけ

== プロパティ

: background-image
   9-uvm_pot.png
: background-image-relative-width
   70
: background-image-relative-margin-top
   17

= とかやってたら、、

po4aなるものがあるらしいじゃないですか!
車輪の再発明してた。。。

po4aでバックエンドを書き直しました。
現在移行作業ちぅ。

== プロパティ

: background-image
   orz.jpg
: background-image-relative-width
   100
: background-image-relative-margin-top
   -10

= po4aで翻訳したい

NetBSD manをpo4aに食わせるにはpatchあてないとダメだった。
後でBTS予定。

でもroffには方言が多いので、Perlのパーサだといたちごっこかも。。。

= 翻訳マンパワー確保

((<www.transifex.net|URL:https://www.transifex.net/projects/p/netbsd-man-translate>))
で翻訳!(('note:(運用まだ開始してないけど)'))

もしくはgithubの方がナウい？

== プロパティ

: background-image
   transifrex_screenshot.png
: background-image-relative-width
   70
: background-image-relative-margin-top
   5

= ブランチ追従

  * まだ考えてない。。。
  * けれど、((*msgmerge*))を使えばtransifexでの翻訳結果を活用できるのではないか

== プロパティ

: background-image
   branch.jpg
: background-image-relative-width
   100

= 今後の展望

  * transifexでの翻訳運用開始
  * transifexでの翻訳の査読方法
  * man9を翻訳完了(('note:(man4も？)'))
  * 用語辞書による翻訳支援
  * NetBSD標準にねじ込む!!!(('note:(といいな)'))

== プロパティ

: background-image
   future2.jpg
: background-image-relative-width
   100

= まとめ

  * NetBSD manを翻訳する(('note:と'))
  * (('note:なぜか'))ソースをgrepするはめに
  * (('note:なしくずし的に'))キレイなソース読める!
  * (('note:そのうち'))manの間違いに気付く!
  * ((*女子力アップ!*))

== プロパティ

: background-image
   riceomelet.jpg
: background-image-relative-width
   130
