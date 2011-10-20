# HaskellとDebianの辛くて甘い関係
![background](debian.png)

Kiwamu Okabe

# 自己紹介
![background](enjoy.png)

* twitter: @master_q
* http://www.masterq.net/
* 職業: コピペプログラマ
* Haskell忍者になるべく修行中
* (NINJA:=No Income No Job or Asset)
* あとPerl忍者リスペクト

# Haskellを知っていますか？
![background](haskell.png)

いくつもの特徴を持った関数型言語です。

ちょっとだけ解説します。

# 静的型付け
![background](cookie_cutter.png)

* 全てのデータ/関数には型がついている
* 型が合わないとエラー
* 暗黙の型変換なんてもんはない
* "型による設計"
* 今まで動作時エラーだったものが、、、
* コンパイル時エラーになる。やった!

# 型推論
![background](sherlock_holmes.png)

* 全部の関数に型書かなくてもOK
* たまーに推論失敗するけど。。。
* 公開関数には型書こう
* where内の非公開関数は省略がいいかも
* hlintの言うことは聞いとけ

# 型クラス
![background](polymorphism.png)

returnとか作れるよ

~~~ { .haskell }
class Functor f => Applicative f where
  return :: a -> f a
instance Applicative [] where
  return a = [a]
instance Applicative Maybe where
  return a = Just a
~~~

おっしゃれー

# パターンマッチ
![background](match.png)

* 型 + パターンマッチ = 表現力∞

もうこんなの嫌

~~~ { .c }
switch (l->l_stat) {
case LSRUN:
        if (l->l_swtime > outpri2) {
                outl2 = l;
                outpri2 = l->l_swtime;
        }
        break;
~~~

# 遅延評価
![background](lazy.png)

* 本当に必要になるまで評価されない
* 無限再帰構造を持つ純粋世界は作れる
* 現実世界(=IOモナド)が純粋世界を手招き
* 手招きされた分のみ純粋世界が評価される
* 使用上の注意をよく読み用法用量を守って

# コンパイルして実行
![background](cpu.png)

* runhaskellでインタプリタ的にも使える
* でもコンパイルしてしまえば環境を選ばない
* Haskell環境のないサーバに直バイナリOK
* コンパイラだから最適化によっては速いかも

# 読みやすく、書きやすい文法
![background](books.png)

where厨になることうけあいです

~~~ { .haskell }
data Tree a   = Node { rootLabel :: a,
                                   subForest :: Forest a }
type Forest a = [Tree a]
flatten :: Tree a -> [a]
flatten t = squish t []
   where squish (Node x ts) xs = x:Prelude.foldr squish xs ts
~~~

# ghciでインタラクティブ ラブ
![background](interactive.png)

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
![background](cabal.png)

* Hackage := Haskellのライブラリ
* Ruby gemみたいな感じ
* 使い方: "cabal install ライブラリ名"
* 依存したHackageを芋蔓式にインストール

# Debianならcabal使うのも簡単!
![background](apt-get_moo.png)

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
![background](rubygem.png)

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
![background](yesod_logo.png)

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
![background](accident.png)

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
![background](orz.png)

せっかくセットアップしても

経年変化で海のもくずに。。。

* orz
* orz orz
* orz orz orz
* orz orz orz orz
* orz orz orz orz orz

# どーしてcabalはこんななの？
![background](problem.png)

それはそれはいくつもの問題があるんじゃよ

* cabalのしくみの問題
* Hackage作者達の文化の問題

の2つに大別されます。

# Hackage 作成の文化的問題
![background](culture.png)

~~~
$ cabal info yesod
--snip--
    Versions available: 0.6.7, 0.7.2, 0.7.3, 0.8.0, 0.8.1, 0.8.2,
                        0.8.2.1, 0.9.1, 0.9.1.1 (and 35 others)
--snip--
    Dependencies:  yesod-core >=0.9.1.1 && <0.10, yesod-auth ==0.7.*,
                   yesod-json ==0.2.*, yesod-persistent ==0.2.*,
                   yesod-form ==0.3.*, monad-control ==0.2.*,
~~~

上限バージョンを決めてしまうんだ。。。 #orz

# cabal の実装上の問題

# Haskellの外をcabalは感知しない

# Hackage群全てを最新にはできない

# 妄想: @khibinoさん最強cabal
![background](khibino.png)

可能性の中から最新を選択してくれたらイイナ

* 図 xxxxx

# そこでDebian DEATHよ!
![background](marie_antoinette.png)

* 最強のcabalができるまでどうすれば、、、
* cabalダメならdeb化しちゃえばイイじゃない

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
![background](helper.png)

debhelperの力です。

# pkg-haskellチームにjoinセヨ!

ボスは

* Joachim Breitner
* E-mail: nomeata@debian.org

debian-haskell@lists.debian.org 常駐？

# darcsな使い方とか

# 合言葉は
![background](omaeha_naniwo.png)

お前の

* ~/.cabal
* ~/.ghc

もカラッポにしてやろか！？

# 宣伝: プレゼンツール作ってます
![background](turtle.png)

* http://carettah.masterq.net/
* Haskell製
* このプレゼンもCarettah使ってます!
