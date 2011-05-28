# install: apt-get install rabbit
# view: rabbit --use-druby --public-level=all hogehoge.rd
# wiimote: rabbit-wiictl.rb hogehoge.rd
# control: rabbit-command --next
# or: $ irb1.9.1 -r drb/drb
#     irb> rabbit = DRbObject.new_with_uri("druby://localhost:10101")
#     irb> rabbit.send(:toggle_index_mode)
# print: rabbit --print -o hogehoge.pdf hogehoge.rd

= Wikiを設置するならgitit!

: subtitle
   とっても簡単だよ!こわくないよ!
: author
   Kiwamu Okabe
: theme
   debian-and-tortoise

= ぼくのスペック

  * twitter: ((<@master_q|URL:http://twitter/master_q>))
  * ふだんはDebian
  * 前の仕事でNetBSD使ってた
  * 今プロニート

== プロパティ

: background-image
   enjoy.jpg
: background-image-relative-width
   100

= Wiki何使ってます？

  * pukiwiki?
  * hiki?
  * github?
  * google code?
  * redmine?

= gititはいかが？

  http://gitit.net/
がデモサイト。

  https://github.com/jgm/gitit
がソースコード置き場。

== プロパティ

: background-image
   gitit.png
: background-image-relative-width
   60
: background-image-relative-margin-top
   10

= gititのうれしいコト

  * wikiデータがgit管理
  * git cloneして文書更新できる
  * 専用プロセスだからfcgi不要
  * 文法がmarkdown(変更可能)
  * プラグイン拡張できる!

== プロパティ

: background-image
   konnichiwan.jpg
: background-image-relative-width
   120
: background-image-relative-margin-top
   0

= 設置手順

Debian squeezeの場合、、、

  $ sudo apt-get install haskell-platform
  $ cabal update
  $ cabal install --reinstall -fhighlighting pandoc
  $ cabal install gitit

これだけ!(('note:(たぶん。。。)'))

= え、、、Haskell？

とりあえず見なかったことにして、先に進みましょう!

== プロパティ

: background-image
   HaskellLogoStyPreview-1.png
: background-image-relative-width
   65
: background-image-relative-margin-top
   7

= 使い方

  $ mkdir mywiki
  $ cd mywiki
  $ ~/.cabal/bin/gitit
  ...別のコンソールで...
  $ chromium http://localhost:5001/ 

うわー簡単!

= こんな初期ページ

== プロパティ

: background-image
   gitit_initialpage.png
: background-image-relative-width
   90
: background-image-relative-margin-top
   8

= ディレクトリ構成

  - mywiki/static:
      静的データ置き場(cssとか画像ファイルとか)
  - mywiki/templates:
      ~/.cabal/share/gitit-X.X/data/templates
      にある*.stテンプレートファイルの変更
  - mywiki/wikidata:
      Wikiページ内容(git管理)

staticとかtemplatesとか編集すればオシャレくなる(('note:ハズ'))

= プラグイン作る#1

ふつーは((*PageTransform*))で。

== プロパティ

: background-image
   gitit_interface.png
: background-image-relative-width
   45
: background-image-relative-margin-top
   10

= プラグイン作る#2

= 実地:daemon化

  $ cat gitit_daemon.sh
  #!/bin/sh
  --snip--
  start() {
  	echo -n "Starting $NAME: "
  	$SSD --start --pidfile $PIDFILE --make-pidfile --background \
  	  --user hoge --chdir $TOPDIR --exec $PROG -- $OPTION
  	RETVAL=$?
  	echo
  	return $RETVAL
  }
  --snip--

= 実地:VirtualHost

  $ pwd
  /etc/apache2/sites-enabled
  $ cat wiki
  <VirtualHost *:80>
  	ServerName wiki.masterq.net
  --snip--
  	ProxyPassReverse / http://127.0.0.1:5001
  	RewriteRule ^(.*) http://127.0.0.1:5001$1 [P]
  --snip--
  </VirtualHost>

= 最後に宣伝

初心者Haskell勉強会やってます。僕が参加者に教えてもらう会!

  前回議事録: http://bit.ly/jnonaP
  次回: http://partake.in/events/8ccdc151-b758-47af-9ff2-91216bd5d3c4

次回は第四回です。いつも横浜近辺開催。是非!

== プロパティ

: background-image
   haskellstudy3.jpg
: background-image-relative-width
   100
: background-image-relative-margin-top
   7

= 参考

  - daemon化スクリプト:
      http://bit.ly/mFfv6f
  - gititプラグインのしくみ:
      http://bit.ly/khbwwJ
  - 実戦配備詳細:
      http://bit.ly/jXDY8Z
