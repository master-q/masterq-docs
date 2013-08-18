# ocamloptの全体像

Kiwamu Okabe

# 私は誰？
![background](img/enjoy.png)

* Twitter: @master_q
* Metasepiプロジェクト主催
* Ajhc Haskellコンパイラ開発者
* Debian Maintainer
* 前はデジタルサイネージの開発してました
* その昔はNetBSDでコピー機作ってた

# 今日のもくじ

# まずはOCaml 4.00.1をインストール

あ、Debian sid前提です...

~~~
$ git clone git@github.com:ocaml/ocaml.git
$ cd ocaml
$ git checkout 4.00.1
$ ./configure -with-debug-runtime
$ make world.opt
$ sudo make install
$ which ocamlopt
/usr/local/bin/ocamlopt
$ ocamlopt -version
4.00.1
~~~

# ocamloptってなんどすか？

OCamlソースを実行バイナリにするコンパイラ

~~~
$ cat hello.ml
let hello _ = print_endline "Hello world!";;
let _ = hello ();
$ ocamlopt -g -runtime-variant d -o hello.bin hello.ml
$ file hello.bin
hello.bin: ELF 64-bit LSB  executable, x86-64, version 1 (SYSV), dynamically linked (uses shared libs), for GNU/Linux 2.6.32, BuildID[sha1]=402030a9bb82606a9d38f73e6ec25455f96d3caf, not stripped
$ ./hello.bin
### OCaml runtime: debug mode ###
Initial minor heap size: 2048k bytes
Initial major heap size: 992k bytes
Initial space overhead: 80%
Initial max overhead: 500%
Initial heap increment: 992k bytes
Initial allocation policy: 0
Hello world!
~~~

# -with-debug-runtime？ -g？

* objdump -S hello.bin してみると...

![inline](img/objdump.png)

# どーいうことだってばよ!？

* objdumpやgdb
* 実行バイナリの中の
* コンパイル結果マシン語と
* ランタイムのマシン語の両方を
* ソースコードレベルデバッグ

できる。すばらしい!

# 