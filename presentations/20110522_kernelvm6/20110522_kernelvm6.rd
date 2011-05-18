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
  $ ./tools/man_utf8 ja/src/share/man/man9/uvm.9.ja

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
  $ ./tools/output_htmls
  。。。warning出まくり。。。
  $ chromium html/index.html

((<haml|URL:http://haml.ursm.jp/>))とか使うから入れといてネ!

== プロパティ

: background-image
   haml.jpg
: background-image-relative-width
   150

= 翻訳するだけだと

  # blockquote
  # title = http://d.hatena.ne.jp/naruoga/20110305/1302188484
  "「man とかどうせ翻訳なんか付いて来られないんだからおまいら英語見ろ」といって本家の各国語版リポジトリパージされちゃった"

== プロパティ

: background-image
   rain3.jpg
: background-image-relative-width
   150

= ワークフロー決め

== プロパティ

: background-image
   workflow.png
: background-image-relative-width
   80
: background-image-relative-margin-top
   6

= ディレクトリ構造

== プロパティ

: background-image
   dirtree.png
: background-image-relative-width
   80
: background-image-relative-margin-top
   6

= 翻訳手順(暫定) #1

gitoriousで"Clone repository"してからgit clone;git flow init((-git-flowが必要-))

  $ git clone git@gitorious.org:~masterq/netbsd-man-translate\
  /masterqs-netbsd-man-translate.git
  $ cd masterqs-netbsd-man-translate
  $ git remote add upstream git@gitorious.org:netbsd-man-translate/netbsd-man-translate.git
  $ git flow init
  。。。リターン押しまくる

= 翻訳手順(暫定) #2

トピックブランチを作って翻訳

  $ git checkout develop
  $ git pull upstream master
  $ xxx ローカルのmasterブランチはどうする？
  $ git flow feature start cpu_initclocks.9
  $ cp ja/src/share/man/man9/cpu_initclocks.9 \
  ja/src/share/man/man9/cpu_initclocks.9.ja
  $ vi ja/src/share/man/man9/cpu_initclocks.9.ja # 翻訳
  $ ./tools/man_utf8 ja/src/share/man/man9/cpu_initclocks.9.ja
  $ git add ja/src/share/man/man9/cpu_initclocks.9.ja
  $ git commit -m "complete translate cpu_initclocks.9.ja"
  $ git flow feature publish cpu_initclocks.9

= 翻訳手順(暫定) #3

"Request merge"ボタン押下

== プロパティ

: background-image
   gitorious_mergereq.png
: background-image-relative-width
   50
: background-image-relative-margin-top
   13

= 査読手順(暫定)

  $ cd netbsd-man-translate
  $ git checkout -b merge-requests/1
  $ git pull git://gitorious.org/netbsd-man-translate\
  /netbsd-man-translate.git refs/merge-requests/1
  $ ./tools/man_utf8 ja/src/share/man/man9/cpu_initclocks.9.ja
  $ git log --pretty=oneline --abbrev-commit \
  master..merge-requests/1
  $ git checkout master
  $ git merge merge-requests/1
  $ git push origin master

査読者募集中!

= 今後の展望

  * man9を翻訳完了(('note:(man4も？)'))
  * 最新版追従できるように差分管理
  * 用語辞書による翻訳支援
  * Webから翻訳できるように
  * NetBSD標準にねじ込む!!!(('note:(といいな)'))

== プロパティ

: background-image
   future2.jpg
: background-image-relative-width
   100

= まとめ

  * NetBSD manを翻訳する(('note:と'))
  * (('note:なぜか'))ソースをgrepするはめに
  * (('note:なしくずし的に'))キレイなソースを読める!
  * (('note:そのうち'))manの間違いに気付く!
  * ((*女子力アップ!*))

== プロパティ

: background-image
   riceomelet.jpg
: background-image-relative-width
   130
