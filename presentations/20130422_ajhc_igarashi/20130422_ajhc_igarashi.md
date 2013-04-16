# Ajhcコンパイラの押売りに来ました

Kiwamu Okabe

# [1] 自己紹介
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
![background](img/carettah.png)

http://carettah.masterq.net/

* Haskell製のプレゼンツール
* Wiiリモコンで操作できます
* でも今バグってる < Wiiリモコン操作
* もちろんこのプレゼンもCarettahです

# Ajhcコンパイラ
![background](img/john.png)

http://ajhc.metasepi.org/

* http://repetae.net/computer/jhc/
* jhcはフットプリントが小さく、
* 高速な実行バイナリを吐くらしい
* jhcにMetasepiに必要な機能追加をしよう
* Ajhc := A fork of jhc

今日はこのプロジェクトに焦点をあてます

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

# [2] Ajhcコンパイラについて

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

![inline](img/stm32f3.png)

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

![inline](draw/2012-12-22-jhc_compile.png)

# メモリマップ
# クロスコンパイル
# ランタイム
# GC
# デモ: GC頻度をgdbで調べる
# 不足している機能/不具合

# [3] 到達するために

* みんな研究大変なんじゃないかなぁ...
* 心配です
* 技術者の心構えってなんだろう？
* Ajhcとさっぱり関係ないですが
* スライドに盛り込んでみました
* いわゆるオッサン化ですね #orz

# すべては不幸からはじまる

* 「必要は発明の母ではない」
* 「イシューよりはじめよ」
* 「客の困りごとを解決するのがビジネス」

# 専門領域をいったん忘れる

* オブジェクト指向設計における unlearn
* 問題とはいったいなんだったのかを捉える
* これがむつかしい... #orz

# 自分以外全てをコモディティ化する

秋山仁が自分の専門分野を選択した理由

「んっとね、日本でだーれもその分野を研究している人がいなかったから」

「みんな今わらったよね。でもこれってすごく大事なことなんだ。
だってその分野を選択したその瞬間に日本一の研究者になれるんだ。
こんな楽なことないじゃない。」

# 自分を書き換える

* mind set => skill set
* 逆流はほとんどない
* mind setを変更するのはとっても大変
* それは自分を小さく殺すということ
* 小さく死んだ自分は通れなかった穴を通れるようになる

# 身の回りに起きる問題集合

* 世界は異なる問題に満ちあふれている
* 世界で自分が存在する点はユニーク
* その点の近傍に発生する問題集合はもちろんユニーク
* それらの問題を解決し続けた終着地点もユニーク

世界にひーとつだーけのはーなー

# 侏儒の言葉 - 芥川龍之介

~~~
http://www.aozora.gr.jp/cards/000879/files/158_15132.html
~~~

~~~
もし遊泳を学ばないものに泳げと命ずるものがあれば、何人も無理だと思うであろう。もし又ランニングを学ばないものに駈けろと命ずるものがあれば、やはり理不尽だと思わざるを得まい。しかし我我は生まれた時から、こう云うばかげた命令を負わされているのも同じことである。
　我我は母の胎内にいた時、人生に処する道を学んだであろうか？　しかも胎内を離れるが早いか、兎に角大きい競技場に似た人生の中に踏み入るのである。勿論游泳を学ばないものは満足に泳げる理窟はない。同様にランニングを学ばないものは大抵人後に落ちそうである。すると我我も創痍を負わずに人生の競技場を出られる筈はない。
　成程世人は云うかも知れない。「前人の跡を見るが好い。あそこに君たちの手本がある」と。しかし百の游泳者や千のランナアを眺めたにしろ、たちまち游泳を覚えたり、ランニングに通じたりするものではない。のみならずその游泳者はことごとく水を飲んでおり、その又ランナアは一人残らず競技場の土にまみれている。見給え、世界の名選手さへ大抵は得意の微笑のかげに渋面を隠しているではないか？
　人生は狂人の主催に成ったオリムピック大会に似たものである。我我は人生と闘いながら、人生と闘うことを学ばねばならぬ。こう云うゲエムのばかばかしさに憤慨を禁じ得ないものはさっさとらちがいに歩み去るが好い。自殺も亦確かに一便法である。しかし人生の競技場に踏み止まりたいと思うものは創痍を恐れずに闘わなければならぬ。
~~~
