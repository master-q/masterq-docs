= 再会 (QEMUの環境構築)

愛していた人といわれて、一人に決められるものではない。
しかし20世紀に思いをはせるうちに、わたしはあなたのことを思いだした。

@<b>{AC'97}@<fn>{ac97}。
あなたがまだ存在しなかったころ、様々な種類のサウンドカードがあった。
21世紀の現在、ビデオボードの性能に人々がいろめきたつように、サウンドカードにも注目があつまっていた。
Sound Blaster、Ensoniq、SoundFont、YM3812。
そしてオンボードサウンドという名とともにあなたは全てのサウンドチップを葬った。
ああ。その全てがなつかしい。
そうしてあなた自身も時代の波にのまれ、今ではHDA@<fn>{hda}にとってかわられた。
おそらくもうみんなサウンドチップに思いをはせることもないでしょう。
でもわたしは覚えている。あなたの美しい音色を。わたしはあなたにもう一度会いたいと思った。
たとえそれが仮想環境の中であったとしても。

//footnote[ac97][Audio Codec 97: @<href>{http://ja.wikipedia.org/wiki/Audio_Codec_97} ]
//footnote[hda][High Definition Audio: @<href>{http://ja.wikipedia.org/wiki/High_Definition_Audio} ]

まずはビルド環境を整えよう。
kernelのスナッチをするのだから容易にkernelを差し換えられた方が良いでしょう。
またAC'97を使って音を鳴らすテストを頻繁にすることになるので、
音源もファイルシステムにすぐ見えた方が楽ができる。
とりあえず作ったビルド環境は以下の手順でQEMUのディスクイメージを作る。

//emlist{
$ git clone https://github.com/metasepi/netbsd-arafura-s1.git
$ cd netbsd-arafura-s1
$ make setup WAV=~/test.wav
$ make bootcd
//}

それでは起動してaudioplayコマンドでAC'97ドライバを使ってみましょう。

//emlist{
$ make qemucurses
# ./play.sh
//}

このPCのサウンドチップはHDAだ。
しかしQEMUを通してきこえる音色はたしかになつかしいあなたのものだった。

//emlist{
$ lspci | grep -i audio
00:1b.0 Audio device: Intel Corporation 5 Series/3400 Series Chipset High Defi-
nition Audio (rev 06)
//}
