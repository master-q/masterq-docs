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
  * Haskell教えてくれる優しいお姉さん募集中

= NetBSD愛してますか？

  # image
  # src = NetBSD_logo.png
  # relative_height = 70
  # reflect_ratio = 0.6

(('tag:center'))もちろんです!

= でも困ってませんか？

  * 普段の開発環境に採用できない
  * キレイなソースってどのへん？
  * そもそもkernelよく知らない
  * 日本語の情報がまとまってない
  * なんとなくマイナー

= どうすれば...

  * 仕様書があればそれを読むべき
  * NetBSDの仕様書って何？
  * そりゃあmanでしょう
  * でもmanが英語しかないので情弱にツラい

= 悩みよさようなら!

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

  * man9を翻訳完了(man4も？)
  * 最新版に追従できるように差分管理
  * webから翻訳できるように
  * NetBSD標準にねじ込む!!!
