# HaskellとDebianの辛くて甘い関係
![background](debian.png)

Kiwamu Okabe

# 自己紹介
![background](enjoy.png)

* twitter: @master_q
* 職業: コピペプログラマ
* Haskell NINJAになるべく修行中
* (NINJA:=No Income No Job or Asset)

# Haskell言語を知っていますか？

# 静的型付け
# 型推論
# パターンマッチ
# 遅延評価
# コンパイルして実行
# 読みやすく、書きやすい文法

# ghciでインタラクティブ ラブ

~~~
$ sudo apt-get install haskell-platform
$ rehash
$ ghci
GHCi, version 7.0.4: http://www.haskell.org/ghc/ :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> print $ fmap \
               (foldr (++) "" . flip replicate "hoge") [1..3]
["hoge","hogehoge","hogehogehoge"]
~~~

# cabalを使えば選り取り緑

* Ruby gemみたいな感じ
* 使い方: "cabal install ライブラリ名"
* 依存したHackageを芋蔓式にインストール
* Hackage := Haskellのライブラリ

# Debianなら簡単!

~~~
$ sudo apt-get install cabal-install
$ rehash
$ cabal update
$ cabal install carettah
$ ~/.cabal/bin/carettah
carettah version 0.0.4
~~~

haskell-platformをインストールすれば

cabal-installは自動でインストールされるけど

# でもcabalには色々不都合が...

Ruby gemとか使ってればよくある日常

~~~
$ sudo gem update
$ sudo gem install earchquake
# 月日は流れ、 、そしてある日、 、
$ sudo gem update
~~~

これで以前インストールしていたearchquake

パッケージは依存ライブラリを含めて最新版に

# yesod Hackageのあるある (続く)

~~~
$ cabal update # ローカルの Hackage データベースを更新
$ cabal install yesod
# 後日 yesod を最新版に更新しようと思いたつ
$ cabal upgrade
--snip--
The 'cabal upgrade' command has been removed
because people found it confusing and it often
led to broken packages.
--snip-- # なにこれーーーーー!？
~~~

# yesod Hackageのあるある (完)

~~~
# しょうがない、必要なパッケージだけ更新しよう
$ cabal install yesod
# yesod が動作しなかったり、依存関係を cabal が自動解決しない
# とりあえず cabal でインストールした Hackage を全部消そう
$ rm -rf ~/.ghc ~/.cabal
$ cabal update
$ cabal install yesod
# さっきの yesod のバグが再現しない。ふつーに動いとる。
# なぜだーーーーーーーーーーーーーーーー!?
~~~

# これじゃあ、、、 #orz

せっかくセットアップしても

経年変化で削除するハメに。。。

* orz
* orz orz
* orz orz orz
* orz orz orz orz
* orz orz orz orz orz

# どーしてcabalはこんななの？

それはそれはいくつもの問題があるんじゃよ。

* cabalのしくみの問題
* Hackage作者達の文化の問題

の2つに大別されます。

# Hackage 作成の文化的問題

# cabal の実装上の問題

# Haskellの外をcabalは感知しない

# Hackage群全てを最新にはできない

# 妄想: @khibinoさん最強cabal

可能性の中から最新を選択してくれたらイイナ

* 図

# そこでDebian DEATHヨ!

最強のcabalができるまでどうすれば、、、

cabalがダメならdeb化しちゃえばイイじゃない

# Hackageのdeb化 #1

Haskellパッケージ化環境整備

~~~
$ sudo apt-get install \
   haskell-debian-utils haskell-devscripts
$ rehash
~~~

debhelperななにかがインストールされる。

# Hackageのdeb化 #2

cabal-debianでdebianディレクトリ作成

~~~
$ wget http://hackage.haskell.org/packages/archive/\
   hcwiid/0.0.1/hcwiid-0.0.1.tar.gz
$ tar xfz hcwiid-0.0.1.tar.gz
$ cd hcwiid-0.0.1/
$ cabal-debian --debianize --ghc \
   --maintainer="Kiwamu Okabe <kiwamu@debian.or.jp>"
$ ls debian
changelog compat control copyright rules
~~~

# Hackageのdeb化 #3

~~~
$ debuild -rfakeroot -us -uc
$ ls ../*hcwiid*deb
../libghc-hcwiid-dev_0.0.1-1~hackage1_amd64.deb
../libghc-hcwiid-doc_0.0.1-1~hackage1_all.deb
../libghc-hcwiid-prof_0.0.1-1~hackage1_amd64.deb
~~~

* 通常使用するライブラリ
* Haddockで生成されたドキュメント
* プロファイラ対応ライブラリ　　がでけた!

# どうしてこんなに簡単なの？

debhelperの力です。

# pkg-haskellチームにjoinセヨ!

ボスは

* Joachim Breitner
* nomeata@debian.org

debian-haskell@lists.debian.org 常駐？

# darcsな使い方とか

# 合言葉は

お前の

* ~/.cabal
* ~/.ghc

もカラッポにしてやろか!

* 図「おまえは何をいっているんだ」
