= 観察(C言語ソースコード解析)

魂をスナッチするには魂の構造を理解しなければならない。
理解とはなにを意味するのかはわからないが、とにかく理解するということに努力をはらう行為そのものなのかもしれない。
あなたの位置はすぐにみつかった。
"netbsd/sys/dev/pci/auich.c"@<fn>{cvsweb_auich_c}がソースコード。
auich(4)@<fn>{man_auich_4}がマニュアルだった。
AC'97ハードウェアの仕様書@<fn>{spec_ac97}も見つかった。

//footnote[cvsweb_auich_c][@<href>{http://cvsweb.netbsd.org/bsdweb.cgi/src/sys/dev/pci/auich.c?only_with_tag=MAIN}]
//footnote[man_auich_4][@<href>{http://netbsd.gw.com/cgi-bin/man-cgi?auich++NetBSD-current}]
//footnote[spec_ac97][AC'97 Component Specification: @<href>{http://www-inst.eecs.berkeley.edu/~cs150/Documents/ac97_r23.pdf}]

xxx 関数どうしの関係を図にすること

xxx 主な構造体の説明

これだけであなたのすべてを理解したことになることにはならない。
バスドライバやautoconfのしくみ、
そしてなによりAC97 codec@<fn>{man_ac97_4}とaudio driver@<fn>{man_audio}を調べていない。
しかしまずあなたの表層を理解するだけで、スナッチをはじめることはできる。
残りの深部はスナッチをしながらでも理解することができることを、わたしは知っていた。

//footnote[man_ac97_4][generic AC97 codec driver: @<href>{http://netbsd.gw.com/cgi-bin/man-cgi?ac97+4+NetBSD-current}]
//footnote[man_audio][device-independent audio driver layer: @<href>{http://netbsd.gw.com/cgi-bin/man-cgi?audio++NetBSD-current}]
