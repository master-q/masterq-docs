# ATS2 updates 2017

Kiwamu Okabe

# What's ATS2 language?

* http://www.ats-lang.org/
* The syntax is similar to ML
* DML-style dependent types / Linear types
* Theorem proving / Safely using pointer
* Without GC / Without runtime
* Compiled into C language code

# What's Japan ATS User Group?

* http://jats-ug.metasepi.org/
* Translating ATS documents into Japanese

~~~
* ATSプログラミング入門
  http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/
* ATSプログラミングチュートリアル
  http://jats-ug.metasepi.org/doc/ATS2/ATS2TUTORIAL/
* Effective ATS
  https://github.com/jats-ug/translate/blob/master/Manual/EffectiveATS.md
~~~

# But the translations are stopped...

```
$ w3m http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/
$ pwd
/home/kiwamu/doc/ATS-Postiats.jats-ug/doc/BOOK/INT2PROGINATS
$ git log .
--snip--
commit 88ce0cdbd5ecf38f6d30a0c5141c3263389d91b8
Author: Kiwamu Okabe <kiwamu@debian.or.jp>
Date:   Sun Nov 29 11:50:07 2015 +0900

    Some fix
--snip--
```

After November 2015, what happens at ATS2?

# ATS-extsolve using SMT solver #1

* Command patsolve_smt2 turns ATS constraints in json format into those in smt-lib2 format.
* It means that z3, cvc4 and etc can be used to type-check.
* It can handle constraints on real numbers.

# ATS-extsolve using SMT solver #2

* Example:

```
$ git clone https://github.com/githwxi/ATS-Postiats.git
$ (cd ATS-Postiats/contrib/ATS-extsolve && make)
$ (cd ATS-Postiats/contrib/ATS-extsolve-smt2 && make)
$ (cd ATS-Postiats/contrib/ATS-extsolve-z3 && make)
$ cd ATS-Postiats/contrib/ATS-extsolve-z3/TEST
$ vi fib.dats
```

```ats
#include "share/atspre_staload.hats"

stacst fib: int -> int
extern praxi fib_bas0(): [fib(0)==0] void
extern praxi fib_bas1(): [fib(1)==1] void
extern praxi fib_ind2{n:int | n >= 2}(): [fib(n)==fib(n-1)+fib(n-2)] void
```

# ATS-extsolve using SMT solver #3

```ats
fun fib {n:nat} .<n>. (n: int(n)) : int(fib(n)) = let
    fun loop {i:nat|i <= n} .<n-i>.
      (ni: int(n-i), f0: int(fib(i)), f1: int(fib(i+1))) : int(fib(n)) =
        (if ni >= 2 then let
             prval () = fib_ind2{i+2}()
           in
             loop{i+1}(ni-1, f1, f0+f1)
           end
         else (if ni >= 1 then f1 else f0)) // Can't solve `f1`!
    prval () = fib_bas0() and () = fib_bas1()
  in
    loop{0}(n, 0, 1)
  end
```

# ATS-extsolve using SMT solver #4

```ats
implement main0(argc, argv) =
{
  val n = (if (argc >= 2)
           then g0string2int(argv[1]) else 10): int
  val n = g1ofg0(n)
  val n = (if n >= 0 then n else 0): intGte(0)
  val () = println! ("fib(", n, ") = ", fib(n))
}
```

```
$ patscc -o fib fib.dats
/home/kiwamu/src/ATS-Postiats/contrib/ATS-extsolve-z3/TEST/fib.dats: 554(line=17, offs=32) -- 556(line=17, offs=34): error(3): unsolved constraint: C3NSTRprop(C3TKmain(); S2Eeqeq(S2Eapp(S2Ecst(fib); S2Eapp(S2Ecst(add_int_int); S2Evar(i(8880)), S2Eintinf(1))); S2Eapp(S2Ecst(fib); S2Evar(n(8879)))))
```

# ATS-extsolve using SMT solver #5

* But z3 can type-check it!

```
$ patscc --constraint-ignore -o fib fib.dats
$ patsopt -tc --constraint-export -d fib.dats | ../patsolve_z3 -i
Hello from [patsolve_z3]!
typechecking is finished successfully!
$ ./fib 25
fib(25) = 75025
```

# NPM package manager for ATS2

* Many ATS2 packages are: https://www.npmjs.com/browse/keyword/ATS

# Better JavaScript support

* Adding some examples to illustrate a way of using ATS+Emscripten
* With libatsopt(JS), patsopt can now run entirely inside the browser.
* With libatscc2js(JS), Atscc2js can now run entirely inside the browser.
* Patsopt(JS) and Atscc2js(JS) have been combined to allow one to try ATS entirely in one's browser (running only client-side JS).

# The other language support

* Support in atscc2erl and atscc2scm for handling tail-call optimized code
* Adding support for stream_vt in the following compilers: atscc2js, atscc2py3, atscc2scm, atscc2clj, atscc2php, atscc2pl

# New parsing-combinator package

* Adding a parsing-combinator package for libats.
* Adding a parsing-combinator package for libatscc.

# Session types

* WebWorker-based support for session types in libatscc2js/Worker/channel

# Support templates

* Introducing $tyrep(...) for outputing terms representing types in the generated code (so as to support direct use of C++ templates in ATS2)
* Supporting $tempenver in template implementation: This is a crucial addition in support of programming in ATS that may potentially be (very) deeply template-based!!!

# Syntax highlighting

* DocBook: https://github.com/githwxi/ATS-Postiats/blob/master/doc/BOOK/INT2PROGINATS/MYTEXT/mytexting.dats
* Pandoc: https://github.com/jgm/skylighting/blob/master/xml/ats.xml

# Extend syntax

* ifcase-expressions: https://github.com/githwxi/ATS-Postiats/blob/master/doc/EXAMPLE/TESTATS/ifcase.dats

# ats-lang-club@googlegroups.com
