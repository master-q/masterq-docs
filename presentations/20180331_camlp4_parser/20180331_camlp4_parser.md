# A OCaml newbie meets Camlp4 parser

Kiwamu Okabe

# Why use Camlp4 parser?

* Why not use the Menhir as parser?
* Because VeriFast verifier uses Camlp4 parser.

# What is VeriFast verifier? #1

* https://github.com/verifast/verifast
* A verifier for single-threaded and multi-threaded C and Java language programs annotated with preconditions and postconditions written in separation logic.
* VeriFast avoids illegal memory accesses such like buffer overflow.
* VeriFast is written by OCaml.

# What is VeriFast verifier? #2

![inline](img/illegal_access.png)

# What is VeriFast verifier? #3

![inline](img/stack.png)

# Why modify VeriFast parser? #1

![inline](draw/platform.png)

# Why modify VeriFast parser? #2

* Real usage needs two C language header.
* Original C headers are for compiling C code.
* Pseud C headers are for verifying C code.
* They may have some semantic gaps, which cause miss verification.
* Because VeriFast's parser is a subset of C99 parser.

# What is a near-term goal?

* Let's verify "kern/subr_optstr.c" file in NetBSD kernel:

```
$ sh build.sh -U -u -j 4 -T obj/tooldir -m amd64 tools
$ sh build.sh -U -u -j 1 -T obj/tooldir -m amd64 kernel=GENERIC
$ vfide -D __STDC__ -D __GNUC__ -D _KERNEL -D __ELF__ -D NO_KERNEL_RCSIDS -I sys -I sys/arch/amd64/compile/obj/GENERIC sys/kern/subr_optstr.c
```

* The kernel code is found at following:
* https://github.com/IIJ-NetBSD/netbsd-src

# Original verifier

![inline](img/before.png)

# Modified verifier

xxx Screen shot

# My patches for VeriFast's parser

```
* Parse semicolon without any declarations #121
  https://github.com/verifast/verifast/pull/121
* Allow hardtab in string literals #120
  https://github.com/verifast/verifast/pull/120
* Ignore inline keyword at static function declaration #119
  https://github.com/verifast/verifast/pull/119
* Support operators in macros / Add -D option #116
  https://github.com/verifast/verifast/pull/116
```

# Knowledge to debug parser #1

xxx

# Ads: 静的コード解析の会 第7回
![background](img/metasepi_meeting.png)

* The meeting will be held on 2018-05-12.
* https://metasepi.connpass.com/event/77398/
* Discussing about following static code analysis technology:

```
ATS, B-Method, CBMC, Coq, Coverity Scan, CSP, Dafny, F*, Frama-C, FreeSafeTy, Infer, Isabelle, SATABS, SPARK, Spin, Uppaal, VDM, VeriFast, Why3, boogie, cogent, corral, seL4, vcc, etc.
```

* We are calling for presentation.

# Ads: Functional Ikamusume book
![background](img/C92cover.png)

* You can buy a latest issue from Toranoana.

```
http://www.toranoana.jp/mailorder/article/04/0030/58/73/040030587376.html
```

* And we are calling for article on Comiket 94.
* The abstract deadline is 2018-06-24.
* Please read following about CFA:
* https://wiki.haskell.org/Books#Joke

