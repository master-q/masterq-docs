# install: apt-get install rabbit
# view: rabbit 20110522_kernelvm6.rd
# print: rabbit --print -o 20110522_kernelvm6.pdf 20110522_kernelvm6.rd

= NetBSD manを翻訳してみた

: subtitle
   NetBSD manを翻訳しよう。契約なんていらないよ!
: author
   Kiwamu Okabe
: theme
   debian-and-tortoise

= ぼくのスペック

  * twitter: ((<@master_q|URL:http://twitter/master_q>))
  * ふだんはDebian
  * 前の仕事でNetBSD使ってた
  * 今はプロニート
  * お姉さんHaskell教えてー

= Love NetBSD?

  # image
  # src = NetBSD_logo.png
  # relative_height = 85
  # reflect_ratio = 0.8

(('tag:center'))Of course! But...

= 困ってませんか？

  * 普段の開発環境に採用できない
  * なんとなくマイナー
  * キレイなソースってどのへん？
  * そもそもkernelよく知らない
  * 日本語の情報がまとまってない

= どうすれば...

  * 仕様書があればそれを読むべき
  * NetBSDの仕様書って何？
  * そりゃあmanでしょう
  * でもmanが英語しかないので情弱(つまり僕)にツラい

= 悩みよさようなら!

日本語翻訳プロジェクトを作った

  # image
  # src = gitorious.png
  # relative_height = 200

= 翻訳manを読むには

UNIXっぽい環境で

  $ git clone git@gitorious.org:netbsd-man-translate\
  /netbsd-man-translate.git
  $ cd netbsd-man-translate
  $ ./tools/man_utf8 ja/src/share/man/man9/uvm.9.ja

皆groff 1.21使ってるよね!
"-K"オプション使うよ!

= 翻訳するだけだと

  # blockquote
  # title = http://d.hatena.ne.jp/naruoga/20110305/1302188484
  "どうせ翻訳なんか付いて来られないんだからおまいら英語見ろ"と本家の各国語版リポジトリパージ

= ワークフロー決め

  # image
  # src = workflow.png
  # relative_height = 130

= 翻訳手順(現状)

https://gitorious.org/netbsd-man-translate/netbsd-man-translate
の"Clone repository"ボタンを押してclone作成。

  $ どうしよう？ xxxxxxxxxxxxxxxxxxxxxxxxxxxx

= *NIX環境がない...

((<netbsdman.masterq.net|URL:http://netbsdman.masterq.net/>))
どぞ

  # image
  # src = netbsdman.png
  # relative_height = 200

= 今後の展望

  * man9を翻訳完了(man4も？)
  * 最新版追従できるように差分管理
  * webから翻訳できるように
  * NetBSD標準にねじ込む!!!といいな
