# Ajhcコンパイラの押売りに来ました
![background](img/furoshiki.png)

Kiwamu Okabe

# [1] 自己紹介
![background](img/MIRACLE-CI-base_2.png)

* http://www.masterq.net/
* ミラクル・リナックス勤務
* 前はコピー機のOSをNetBSDで作ってた
* Debian Maintainer
* デジタルサイネージのソフト開発してます
* OSSに興味のあるエンジニア募集中です!

# 経歴
![background](img/san_ai.png)

* 2001年: 東京都立大学 修士卒業
* 専攻: 電気・電子工学
* 2001—2012年: コピー機メーカー勤務
* 2012年—: 現職

# 宣伝:λカ娘に記事を書きませんか？
![background](img/haskell-logo.png)

http://www.paraiso-lang.org/ikmsm/

* 関数型プログラミングに関する同人誌
* これまで4巻出しました
* 結構好評みたいです
* ネタある人は @xhl_kogitsune さんまで!
* Call For Articlesはイカ

~~~
http://www.paraiso-lang.org/ikmsm/books/c84.html
~~~

# おしながき
![background](img/kyoto_night.png)

* [1] 自己紹介
* [2] Ajhcコンパイラとは
* [3] kernelに強い型が必要なのか
* [4] Ajhcコンパイラとマイコン
* [5] Ajhcのすゝめ

# [2] Ajhcコンパイラとは
![background](img/ajhc.png)

http://ajhc.metasepi.org/

* Ajhc := A fork of jhc
* http://repetae.net/computer/jhc/
* jhcはフットプリントが小さく、
* 高速な実行バイナリを吐くらしい
* 組み込みに向いてるかも

# どうしてAjhcコンパイラを作るの？
![background](img/metasepi.png)

* Metasepiというkernelを作るため

http://metasepi.org/

* UNIXモドキkernelを強い型によって設計
* 言語: HaskellとかOCamlとかMLtonとか

# [3] kernelに強い型が必要なのか
![background](img/crash.png)

* LinuxやWindowsが既にあるのでは？
* しかし大規模組込開発は苦しんでいる
* kernelをカスタマイズして使用
* kernelのランタイムエラー
* 再現することでさえ困難

# kernelにこそ型安全が必要
![background](img/inria.png)

* ほとんどのkernelはC言語で設計される
* ユーザー空間でエラー → SEGV
* kernel空間でエラー → システム停止
* kernelの設計には細心の注意が必要
* C言語は安全なのか？
* 参考:数理科学的バグ撲滅方法論のすすめ

~~~
http://itpro.nikkeibp.co.jp/article/COLUMN/20060915/248230/
~~~

# 改造工数を小さくしたい
![background](img/fork.png)

OSSを使った製品開発あるある

![inline](draw/2013-01-18-fork2.png)

# C言語の表現力は非力
![background](img/c.png)

モダンな言語機能を使いたい

* パターンマッチ
* 純粋関数
* 高階関数
* 型推論
* 対話環境
* STM

# 型付けされていれば未来は広がる
![background](img/hope.png)

* 単体テストしやすい
* 型による契約プログラミング
* 検証しやすい
* 証明器による設計

# なぜGHCを使わないのか
![background](img/sumo.png)

GHCの吐くバイナリはサイズが大きい

~~~
$ ajhc --version
ajhc 0.8.0.5 (210da9343b6b7e38a89eb28f85602471a325647a)
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.4.1
$ echo 'main = print "Hello"' > Hello.hs
$ ajhc Hello.hs -o Hello_ajhc
$ ghc Hello.hs -o Hello_ghc
$ size Hello_ajhc Hello_ghc 
   text    data     bss     dec     hex filename
  19440    1324     744   21508    5404 Hello_ajhc
 709612   40840   46776  797228   c2a2c Hello_ghc
$ ldd Hello_ajhc | wc -l
3
$ ldd Hello_ghc | wc -l
9
~~~

ML系はあまり調べきれていません...

# 型で設計されたOSは他にないのか
![background](img/os_boot.png)

デスクトップとして使用可能なレベルではない

* Funk (OCaml製)

~~~
  http://home.gna.org/funk/
~~~

* snowflake-os (OCaml製)

~~~
  http://code.google.com/p/snowflake-os/
~~~

* House (Haskell製)

~~~
  http://programatica.cs.pdx.edu/House/
~~~

* HaLVM (Haskell製)

~~~
  http://corp.galois.com/halvm/
~~~

# 実用化に辿りつくために
![background](img/konami.png)

* ドッグフードを維持しながら開発

![inline](draw/2012-12-27-arafura_design.png)

# [4] Ajhcコンパイラとマイコン
![background](img/cabal.png)

Ajhcの使い方は以下のような感じ

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

# まずはデモでもどうぞ
![background](img/led.png)

~~~
デモ動画: http://www.nicovideo.jp/watch/sm20336813
デモソースコード: https://github.com/ajhc/demo-cortex-m3
~~~

* LEDをぐりんぐりん
* LEDにモールス信号

# デモソースコード
![background](img/cuttlefish_mono.png)

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

# 図にしましょう: 起動シーケンス
![background](img/takeoff.png)

![inline](draw/boot.png)

# 図にしましょう: Haskellコードの動き
![background](img/circuit.png)

![inline](draw/running.png)

# 図にしましょう: もっと簡単に
![background](img/freefall.png)

![inline](draw/running_simple.png)

# AjhcはC言語への変換器
![background](img/john.png)

![inline](draw/2012-12-22-jhc_compile.png)

# クロスコンパイル
![background](img/cross.png)

![inline](draw/cross_compile.png)

# ランタイム
![background](img/wingsuit.png)

* C言語コメント込み3000行 小さい!
* 組み込み用途に向いている

![inline](draw/2012-12-22-jhc_custom_rts.png)

# マイコンデモでのメモリマップ
![background](img/memory_chip.png)

モールス信号デモの場合

![inline](draw/memmap.png)

# GC
![background](img/dec9ue.png)

~~~
http://www.slideshare.net/dec9ue/gc-16298437
~~~

* @dec9ue という人がすごい詳しい
* マークオンリーGCらしい
* GCのくせにmalloc使う
* AjhcではGCを修正して省メモリ化

# デモ: GC頻度をgdbで調べる
![background](img/gdb.png)

* GCを実行する関数にbreak pointはる

(gdb) break gc_perform_gc

(gdb) continue

* どれぐらいGCが起きてるかな？

# 不足している機能/不具合
![background](img/fuji.png)

* Haskell Platform移植
* GHCコンパチ
* 再入
* 並列実行
* GC中の割り込み
* リージョン推論

研究開発テーマの山ですね!

# [5] Ajhcのすゝめ
![background](img/fukuzawa.png)

* Ajhc使ってみませんか？
* マニュアルはイカ

http://ajhc.metasepi.org/manual_ja.html

* バグ/機能要望ありましたらイカまで

https://github.com/ajhc/ajhc/issues

* もちろんpull requestも大歓迎です!

https://github.com/ajhc/ajhc
