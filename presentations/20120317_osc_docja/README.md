スライド: NetBSD manを翻訳しよう!
=================================

[第六回 カーネル／VM探検隊](http://atnd.org/events/15330)
向けのスライド。

    ./rabbit-wiictl.rb 20110522_kernelvm6.rd

でWiiリモコンを使ったプレゼンができます。
実行に失敗したら適宜gemを突っ込んでください。

以下はてきとうに作ったブツの解説

rabbit/theme/debian-and-tortoise/debian-and-tortoise.rb
-------------------------------------------------------
今回のプレゼンテーマの親玉。
以下2つのテーマの他に兎と亀テーマなんかもinclude_themeしています。

rabbit/theme/twitter-footer/twitter-footer.rb
---------------------------------------------
twitterのTLをリアルタイム表示するテーマです。
ホームディレクトリに以下のようなファイルが必要です。平文で。

    $ cat ~/.twitter_userpass
    ユーザ:パスワード
    $ chmod 600 ~/.twitter_userpass

プラグインを使う時は

    @twitter_stream_content ='track=#kernelvm,master_q'
    include_theme("twitter-footer")

のようにしてストリーム検索したい単語を","で区切ってプラグインに渡してください。

rabbit/theme/debian-prime/debian-prime.rb
-----------------------------------------
Debianテーマの改造版です。

* 高橋メソッド無効化
* タイトルフォントを小さく
* スライドフッタの描画タイミングをpreに (上記twitter-footer.rbのために必要)

rabbit-wiictl.rb
----------------

wiiリモコンを使ってrabbitプロセスを操作します。
rabbitプロセスを--use-drubyで起動したあと、wiiリモコンの状態を検知して
本プロセスからdrubyでrabbitプロセスにコマンドを投げます。
あとwiiリモコンLEDがナイトライダーっぽくチカチカします。

ライセンス
----------
The author disclaims all copyright. The ruby code is in the public domain.
