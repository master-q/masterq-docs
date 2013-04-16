# Ajhcコンパイラの押売りに来ました

Kiwamu Okabe

# 1. 自己紹介
![background](img/MIRACLE-CI-base_2.png)

* ミラクル・リナックス勤務
* 前はコピー機のOSをNetBSDで作ってた
* Debianメンテナ
* デジタルサイネージのソフト開発してます
* OSSに興味のあるエンジニア募集中です!

# 経歴

* 2001年: 東京都立大学 修士卒業
* 専攻: 電気・電子工学
* 2001ー2012年: コピー機メーカー勤務
* 2012年ー: 現職

.oO(この大学って確か「ドブスを守る会」では)

# 個人運営のプロジェクト紹介

* Metasepi OS
* NetBSD man translate
* スタート低レイヤー勉強会
* プレゼンツールCarettah
* Ajhcコンパイラ

# Metasepi OS

http://metasepi.org/

* 強い型を持つ言語で、
* UNIXモドキkernelを作るプロジェクト
* まだ何も作っていない
* 設計に必要になる技術の調査と試験中
* プロジェクトの詳細は以下の資料をどうぞ

~~~
http://www.slideshare.net/master_q/what-is-metasepi
~~~

# NetBSD man translate
![background](img/netbsd.png)

http://netbsdman.masterq.net/

* NetBSDのmanpageを翻訳しよう
* gettextで翻訳するワークフロー作った
* でも今バグってる...Ruby+Hamlめ...
* 肝心の翻訳そのものはあんまり進んでない

# スタート低レイヤー勉強会

~~~
http://start_printf.masterq.net/
http://www.slideshare.net/master_q/20130215-start-printf0
~~~

* printfの動作をソースから解析する会

![inline](draw/trace.png)

# プレゼンツールCarettah

http://carettah.masterq.net/

* Haskell製のプレゼンツール
* Wiiリモコンで操作できます
* でも今バグってる < Wiiリモコン操作
* もちろんこのプレゼンもCarettahです

# Ajhcコンパイラ

http://ajhc.metasepi.org/

* http://repetae.net/computer/jhc/
* jhcはフットプリントが小さく、
* 高速な実行バイナリを吐くらしい
* jhcにMetasepiに必要な機能追加をしよう
* Ajhc := A fork of jhc

今日はこのプロジェクトに焦点をあてます

# 宣伝:λカ娘に記事を書きませんか？

http://www.paraiso-lang.org/ikmsm/

* 関数型プログラミングに関する同人誌
* これまで4巻出しました
* 結構好評みたいです
* ネタある人は @xhl_kogitsune さんまで!
* Call For Articlesはイカ

~~~
http://www.paraiso-lang.org/ikmsm/books/c84.html
~~~

# 2. Ajhcコンパイラについて

使い方は以下のような感じ

~~~
$ cabal install ajhc
$ ajhc --version
ajhc 0.8.0.1 (80aa12fb9b57622bba2f0e911d7ebc0c04ddb662)
compiled by ghc-7.4 on a x86_64 running linux
$ echo 'main = print "hoge"' > Hoge.hs
$ ajhc Hoge.hs
$ ./hs.out
"hoge"
~~~

* このままだと面白くない
* クロスコンパイルして、
* マイコンでHaskellコード実行してみる？

# まずはデモでも

~~~
デモ動画: http://www.nicovideo.jp/watch/sm20336813
デモソースコード: https://github.com/ajhc/demo-cortex-m3
~~~

* LEDをぐりんぐりん
* LEDにモールス信号



# デモソースコード

なんじゃこりゃ...

~~~ {.haskell}
foreign import ccall "c_extern.h Delay" c_delay :: Word32 -> IO ()
foreign import ccall "c_extern.h &jhc_zeroAddress" c_jhc_zeroAddress16 :: Ptr Word16

gpioPin8, gpioPin9, gpioPin10, gpioPin11, gpioPin12, gpioPin13, gpioPin14, gpioPin15, led3, led4, led5, led6, led7, led8, led9, led10 :: Word16
gpioPin8  = 0x0100
--snip--
led3  = gpioPin9
--snip--

brrPtr, bsrrPtr :: Ptr Word16
brrPtr  = c_jhc_zeroAddress16 `plusPtr` 0x48001028
bsrrPtr = c_jhc_zeroAddress16 `plusPtr` 0x48001018

ledOff, ledOn :: Word16 -> IO ()
ledOff = poke brrPtr
ledOn  = poke bsrrPtr
~~~

# 図にしましょう

# AjhcはC言語への変換器
# メモリマップ
# クロスコンパイル
# ランタイム
# GC
# デモ: GC頻度をgdbで調べる
# 不足している機能/不具合

# 3. 到達するには

# すべては不幸からはじまる
# 専門領域をいったん忘れる (オブジェクト指向設計における unlearn)
# 自分以外のすべてをコモディティ化する (秋山仁が自分の専門分野を選択した理由)
# 自分を書き換える (mind setとskill set)
# 自分の身の回りにおきる問題集合がユニークであることはトリビアル
