# To run with it without system install.
#  % ruby -I./lib bin/rabbit -f sample/rabbit.rd

= NetBSD manを翻訳してみた

: subtitle
   NetBSD manを翻訳しよう。契約なんていらないよ!
: author
   Kiwamu Okabe
: institution
   プロニート
: theme
   rabbit-and-tortoise
: allotted-time
   8m

= ぼくのスペック

  * twitter: ((<@master_q|URL:http://twitter/master_q>))
  * ふだんはDebian
  * 前の仕事でNetBSD使ってた
  * 今はプロニート
  * Haskell教えてくれる優しいお姉さん募集中

= NetBSD愛してますか？

(('tag:center'))(('tag:x-large:もちろんです!'))

= でもこんなことに困ってませんか？

  * 普段の開発環境に採用できない
  * キレイなソースってどのへん？
  * そもそもkernelよく知らない
  * 日本語の情報がまとまってない
  * なんとなくマイナー

= この状況を変えるにはどうすれば...

  * 仕様書があればそれを読むべき
  * NetBSDの仕様書って何？
  * そりゃあmanでしょう
  * でもmanが英語しかないので情弱にツラい

= そんな悩みも今日で終わりです!

  * 日本語翻訳プロジェクトを作ってみた

      xxx スクリーンショット xxx

= ただ翻訳するだけだと...

  # blockquote
  # title = http://d.hatena.ne.jp/naruoga/20110305/1302188484
  どうせ翻訳なんか付いて来られないんだからおまいら英語見ろと本家の各国語版リポジトリパージ

= ワークフローを決めてみた


= でもgroffとか知らないヨ？

((<netbsdman.masterq.net|URL:http://netbsdman.masterq.net/>))
をどうぞ。

      xxx スクリーンショット xxx

= 今後の展望

  * man9を完全に翻訳完了
  * man4に手をつける
  * man4,9について最新版に追従できるように差分管理
  * NetBSD標準にねじ込む!!!
