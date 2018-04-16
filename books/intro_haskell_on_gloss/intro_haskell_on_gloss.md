# お絵描きしがならHaskell入門 2018年夏

# 導入

* xxx 入門のきっかけ
* xxx Haskellの簡単な説明
* xxx Haskellアプリケーションの例
* xxx Haskellを使っている会社の例

# 開発環境構築

GHCはHaskell言語のコンパイラの1つで、現時点でもっとも多くのHaskellプログラマが日常的に使っています。「HaskellコンパイラならGHCで決まり」と思ってしまって問題ありません。

GHC単体でもプログラミングはできますが、近年stack https://www.haskellstack.org/ と呼ばれるクロスプラットフォームな管理ツールが普及しました。先のGHCコンパイラも開発に必要なライブラリ群もこのツールで一括管理できます。

「さっそく使ってみるでゲソ!」

## Windows

インストーラである `stack-1.6.5-windows-x86_64-installer.exe` を https://www.stackage.org/stack/windows-x86_64-installer からダウンロードして、インストーラを起動してください。
インストールが完了したらコマンドプロンプトを開いて以下のようにstackがインストールされていることを確認してください。

```
$ stack --version
Version 1.6.5, Git revision 24ab0d6ff07f28276e082c3ce74dfdeb1a2ca9e9 (5514 commits) x86_64 hpack-0.20.0
```

## macOSもしくはLinux

curlでインストールスクリプトをダウンロードして実行してください。その後`$HOME/.local/bin`にPATHを通してから、以下のようにstackがインストールされていることを確認してください。

```
$ curl -sSL https://get.haskellstack.org/ | sh
$ export PATH=$HOME/.local/bin:$PATH
$ stack --version
Version 1.6.5, Git revision 24ab0d6ff07f28276e082c3ce74dfdeb1a2ca9e9 (5514 commits) x86_64 hpack-0.20.0
```

# 対話環境を使ってみる

最初に告白します。さきほど嘘をついてしまいました。「GHCはコンパイラだ」と書きましたがそれは嘘です。厳密にはghcというコマンドはコンパイラとして振舞い、ghciという別のコマンドは対話環境として振舞います。stackが既にインストールされていれば、すぐにghciを使って対話的にプログラミングできます。

まずは一旦ghciを起動してみましょう:

```
$ stack ghci
Writing implicit global project config file to: /home/user/.stack/global-project/stack.yaml
Note: You can change the snapshot via the resolver field there.
Using latest snapshot resolver: lts-11.5
Downloaded lts-11.5 build plan.    
Preparing to install GHC (nopie) to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-nopie-8.2.2.                                      
Installed GHC.                                                                          
Selected mirror https://s3.amazonaws.com/hackage.fpcomplete.com/                                 
Downloading root                                                                                 
Selected mirror https://s3.amazonaws.com/hackage.fpcomplete.com/                                 
Downloading timestamp                                                                            
Downloading snapshot                                                                             
Downloading mirrors                                                                              
Cannot update index (no local copy)                                                              
Downloading index                                                                                
Updated package index downloaded                                                                 
Update complete                                                                                  
Populated index cache.    

Warning: No local targets specified, so ghci will not use any options from your package.yaml / *.cabal files.
         
         Potential ways to resolve this:
         * If you want to use the package.yaml / *.cabal package in the current directory, use stack init to create a new stack.yaml.
         * Add to the 'packages' field of /home/user/.stack/global-project/stack.yaml
         
Configuring GHCi with the following packages: 
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/ghci20981/ghci-script
Prelude> 
```

なにやら色々ダウンロードされましたがghciのプロンプトが無事表示されました。

このままとりあえず簡単な計算でもしてみましょう:

```
Prelude> 1 + 1
2
Prelude>
```

なるほど簡単な計算はできるようです。

少しここで飛躍してGlossというパッケージをインストールしてみましょう:

```
Prelude> :q
Leaving GHCi.
$ stack install gloss
[1 of 2] Compiling Main             ( /home/user/.stack/setup-exe-src/setup-mPHDZzAJ.hs, /home/user/.stack/setup-exe-src/setup-mPHDZzAJ.o )
[2 of 2] Compiling StackSetupShim   ( /home/user/.stack/setup-exe-src/setup-shim-mPHDZzAJ.hs, /home/user/.stack/setup-exe-src/setup-shim-mPHDZzAJ.o )
Linking /home/user/.stack/setup-exe-cache/x86_64-linux-nopie/tmp-Cabal-simple_mPHDZzAJ_2.0.1.0_ghc-8.2.2 ...
bmp-1.2.6.3: download
fixed-0.2.1.1: download
fixed-0.2.1.1: configure
half-0.2.2.3: download
ObjectName-1.1.0.1: download
fixed-0.2.1.1: build
bmp-1.2.6.3: configure
bmp-1.2.6.3: build
half-0.2.2.3: configure
half-0.2.2.3: build
ObjectName-1.1.0.1: configure
fixed-0.2.1.1: copy/register
ObjectName-1.1.0.1: build
ObjectName-1.1.0.1: copy/register
half-0.2.2.3: copy/register
stm-2.4.5.0: download
stm-2.4.5.0: configure
stm-2.4.5.0: build
bmp-1.2.6.3: copy/register
text-1.2.3.0: download
text-1.2.3.0: configure
stm-2.4.5.0: copy/register
text-1.2.3.0: build
StateVar-1.1.0.4: download
StateVar-1.1.0.4: configure
StateVar-1.1.0.4: build
StateVar-1.1.0.4: copy/register
text-1.2.3.0: copy/register
OpenGLRaw-3.2.7.0: download
OpenGLRaw-3.2.7.0: configure
OpenGLRaw-3.2.7.0: build
OpenGLRaw-3.2.7.0: copy/register
GLURaw-2.0.0.4: download
GLURaw-2.0.0.4: configure
GLURaw-2.0.0.4: build
GLURaw-2.0.0.4: copy/register
OpenGL-3.0.2.1: download
OpenGL-3.0.2.1: configure
OpenGL-3.0.2.1: build
OpenGL-3.0.2.1: copy/register
GLUT-2.7.0.13: download
GLUT-2.7.0.13: configure
GLUT-2.7.0.13: build
GLUT-2.7.0.13: copy/register
gloss-rendering-1.11.1.1: download
gloss-rendering-1.11.1.1: configure
gloss-rendering-1.11.1.1: build
gloss-rendering-1.11.1.1: copy/register
gloss-1.11.1.1: download
gloss-1.11.1.1: configure
gloss-1.11.1.1: build
gloss-1.11.1.1: copy/register
Completed 13 action(s).
```

インストールは無事終わったようです。

再度ghciを起動して、簡単なアプリケーションを作ってみましょう:

```
$ stack ghci
Prelude> import Graphics.Gloss
Prelude Graphics.Gloss> display (InWindow "Hello" (200, 200) (10, 10)) white (Circle 80)
```

新しいウィンドウが開いて、以下のような円が表示されたでしょうか:

![](img/hello_window.png)

これでウィンドウにお絵描きすることを身に付けました。最初の一歩としては上出来でしょう!

* xxx WindowsとmacOSでglossのウィンドウが出るか試す

# 最初のアプリケーション

* xxx スケルトンの生成
* xxx ディレクトリ構造
* xxx ビルド手順
* xxx 実行手順
* xxx REPLと同じことをアプリケーションにまとめる

# Glossライブラリ

* xxx Glossとは

# 型を調べる

* xxx 型とは

## GHCiを使う
## Haddockを使う

# 最初のアプリケーションの構造

![](draw/draw.png)

* xxx ソース解説

# HoogleでAPIを探してみよう

# より凝ったアプリケーション

* xxx Hoogleを使って好みのAPIを発見する
* xxx Glossアニメーション

# 遅延評価

* xxx マウスクリックで遅延評価を一歩一歩進める

# その先へ

* xxx すごいH
* xxx 栄光のグラスゴー
* xxx Preludeを読む
* xxx Preludeを自作する
* xxx 論文
* xxx コミュニティ

# 製品品質のコードを書くには

* xxx 依存関係地獄
* xxx 枯れたライブラリの見分け方
* xxx 凝ったAPIを使うときは慎重に
* xxx 優れたアプリケーションがエレガントなコードな訳ではない(gitit,pandoc,ghc)

# 宿題

* xxx 黄金比とアニメーションスピードの相関
