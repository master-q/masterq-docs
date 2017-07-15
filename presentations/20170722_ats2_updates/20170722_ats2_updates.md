# ATS2 updates 2017

Kiwamu Okabe

# What's ATS2 language?

* http://www.ats-lang.org/
* xxx

# What's Japan ATS User Group?

* http://jats-ug.metasepi.org/
* xxx

# ATS2 translations are stopped...

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

# ATS-extsolve using SMT solver

* Command patsolve_smt2 turns ATS constraints in json format into those in smt-lib2 format
* It means that z3, cvc4 and etc can be used to type-check
* Handle constraints on real numbers

# NPM package manager for ATS2

xxx

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
