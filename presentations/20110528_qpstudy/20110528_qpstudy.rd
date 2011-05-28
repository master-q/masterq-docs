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
   簡単だよ!
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

= Wiki何使ってます？

  * pukiwiki?
  * hiki?
  * github?
  * google code?
  * redmine?

= gititはいかが？

* http://gitit.net/
はよく落ちてるから
* ((*https://github.com/jgm/gitit*))
こっちかな？

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
  * 文法がmarkdown
  * プラグイン拡張できる!

= 設置手順

Debian squeezeの場合、、、

  $ sudo apt-get install haskell-platform
  $ cabal update
  $ cabal install gitit

これだけ!(('note:(たぶん。。。)'))

= え、、、Haskell？

とりあえず見なかったことにして、先に進みましょう!

= 使い方

  $ mkdir mywiki
  $ cd mywiki
  $ ~/.cabal/bin/gitit
  ...別のコンソールで...
  $ chromium  http://localhost:5001/ 

うわー簡単!

= こんな初期ページ



= 実戦投入

スクリプト書いてdaemon化。

  #!/bin/sh
  TOPDIR="/home/hoge/wiki/gitit"
  NAME=gitit
  PIDFILE=$TOPDIR/gitit_daemon.pid
  PROG="/home/hoge/.cabal/bin/gitit"
  OPTION="-f gitit_daemon.conf"
  SSD=/sbin/start-stop-daemon
  start() {
  	echo -n "Starting $NAME: "
  	$SSD --start --pidfile $PIDFILE --make-pidfile --background \
  	  --user hoge --chdir $TOPDIR --exec $PROG -- $OPTION
  	RETVAL=$?
  	echo
  	return $RETVAL
  }
  ...

= 参考

* daemon化スクリプト xxxxxxxx

