# お絵描きしがならHaskell入門 2018年夏

# 導入

* xxx 入門のきっかけ
* xxx Haskellの簡単な説明
* xxx Haskellアプリケーションの例
* xxx Haskellを使っている会社の例

# 開発環境構築

* xxx GHCの簡単な説明

https://www.haskellstack.org/ は現時点で多く使われているクロスプラットフォームな管理ツールです。先のGHCコンパイラも開発に必要なライブラリ群もこのツールで一括管理できます。

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

* xxx GHCiの簡単な説明
* xxx Glossのインストール
* xxx 説明なしにいきなりウィンドウを出す

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
