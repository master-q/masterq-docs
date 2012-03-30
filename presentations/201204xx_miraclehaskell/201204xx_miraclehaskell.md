# ミラクルはすける勉強会 #0
![background](debian.png)

Kiwamu Okabe

# この勉強会でやること

* Haskellってなんだか知る
* プログラミングができるようになる
* 型について知る
* Hackageを使ってみる
* Hackageに自分でパッケージ登録
* ＼（＾ｏ＾）／ 卒業！

# Haskellって何ですか？

関数型言語です。

。。。などということはどーでも良くて、

使ってみましょう!

# どうしてHaskellなんですか？

それをこの勉強会で実感するんですよ!

利点としては、、、

* コンパイラで実行速度速い
* ソースコードの行数が短かくなる
* コンパイルエラーが増えて
* ランタイムエラーが少なくなる # <= 重要!

# Haskell製プログラムあるの？

ありますってば!

~~~
* Attoparsec - 高速パーサコンビネータ
* Gitit - VCSバックエンドWiki
* Gtk2hs - Haskellから使えるGtkラッパ
* Hakyll - 静的Webサイト生成
* House - Haskell製OS
* Mighttpd - nginxぐらい速いWebサーバ
* Monadius - グラディウスっぽいゲーム
* Pandoc - ドキュメントフォーマット変換
* Yesod - RailsっぽいWebフレームワーク

その他↓にたくさん登録されてます
http://hackage.haskell.org/packages/archive/pkg-list.html
~~~

# 使ってる会社あるの？

ありますってば!

~~~
* Bluespec: SystemVerilogとSystemCのツール実装に使ってる
* Bump Technologies: モバイル向け連絡先交換のサーバサイド
* Microsoft Research: GHC(Haskellコンパイラ)の研究開発
* Qualcomm: LuaのBREWバインディングジェネレータ
* Standard Chartered Bank: デリバティブリスク分析
* Tsuru Capital: 株式トレーディング

その他:
 http://www.haskell.org/haskellwiki/Haskell_in_industry
~~~

# とりあえず本で勉強しましょう

![background](learnyou.png)

「すごいHaskellたのしく学ぼう！」

~~~
http://www.ohmsha.co.jp/kaihatsu/archive/\
2012/03/21160230.html

"Learn You a Haskell for Great Good!: A Beginner's Guide"
の翻訳書。2012年5月新刊予定。
~~~

あれ？まだ売ってないの？？？

# じゃ5月まで何すんの？

お絵描きして遊んでみましょう!

http://gloss.ouroborus.net/

~~~
Gloss hides the pain of drawing simple vector graphics
behind a nice data type and a few display functions.
Gloss uses OpenGL under the hood, but you won't need
to worry about any of that.
Get something cool on the screen in under 10 minutes.
~~~

なにやらOpenGLでお絵描きできるそうです。

# Haskellのインストール

Debian GNU/Linux sidをお使いの方

~~~
$ sudo apt-get install haskell-platform
~~~

それ以外のディストリビューション/OSの方は

~~~
http://hackage.haskell.org/platform/linux.html
http://wiki.haskell.jp/Workshop/StartHaskell/0
~~~

から入れてみてください。

# Glossのインストール

~~~
$ cabal update
$ cabal install gloss
$ cabal install gloss-examples
~~~

cabalというコマンドが

http://hackage.haskell.org/

からGlossが依存するパッケージも取ってきて

くれます。Rubyのgemみたいな感じです。

# エディタの設定

# Gloss最初の一歩

~~~
$ ghci
GHCi, version 7.4.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> import Graphics.Gloss
Prelude Graphics.Gloss> display (InWindow "Hoge" \
                       (200, 200) (10, 10)) white (Circle 80)
~~~

さて。。。何が表示されるでしょうか？
