# Writing NetBSD Sound Drivers in Haskell

Kiwamu Okabe @ Metasepi & Takayuki Muranushi @ RIKEN

# Who am I?
![background](img/enjoy.png)

* http://www.masterq.net/
* Self employed software engineer
* Trade name := METASEPI DESIGN
* Founder of Metasepi Project
* A Debian Maintainer
* 10 years' experience in developing OS using NetBSD

# Demo: NetBSD driver in Haskell
![background](img/netbsd.png)

* NetBSD audio driver play sound
* The driver's interrupt handler rewrited using Haskell
* GC occurs in interrupt handler
* Watch the movie at following

~~~
https://www.youtube.com/watch?v=XEYcR5RG5cA
~~~

# Agenda

* [1] Demo: NetBSD driver in Haskell
* xxx

# [2] Metasepi Project

![background](img/metasepi.png)

http://metasepi.org/

* Unix-like OS designed by strong type.
* Using ML or more strong type lang.

Haskell http://www.haskell.org/

OCaml http://caml.inria.fr/

MLton http://mlton.org/

. . . and suchlike.

# Scratch or Rewrite

xxx

# Snatch-driven development #1

http://en.wikipedia.org/wiki/Snatcher

![inline](draw/snatch-system.png)

# Snatch-driven development #2

![inline](draw/2012-12-27-arafura_design.png)

# System programming compiler

Programs to print "hoge" on terminal. The lesser depends on POSIX, the smaller values.

![inline](img/compare_compiler_ats.png)

Choose jhc for portability and flexibility.

# Need preemptive multitasking

* There are 2 way for multitasking
* [A] Nonpreemptive multitasking
* [B] Preemptive multitasking
* Unix-like OS needs [B]
* [B] needs the hardware interrupts
* Interrupt handler should be reentrant

# What's reentrancy ?

~~~
Reentrant code can be interrupted in the middle of its execution and then safely called again ("re-entered") before its previous invocations complete execution.
~~~

![inline](draw/reentrant.png)

# How to get reentrancy in C ?
# C language Context
# Problem: Interrupt and GC

![inline](draw/switch_ongc.png)

# There is Haskell Context ?
# When Haskell Context is born ?
# What's Haskell Context ?
# Implementation: CLHs
# Haskell Context (CLHs)
# Haskell Context life cycle (CLHs)

![inline](draw/arena_lifecycle.png)

# Benchmark

~~~
(O)  Original NetBSD 6.1.2 kernel
(S)  The kernel includes AC'97 and HD Audio driver snatched by Ajhc
(N)  (S) + using naive GC
(B4) (S) + having GC block 16 Byte
(B5) (S) + having GC block 32 Byte
(B6) (S) + having GC block 64 Byte
~~~

![inline](img/arafura-s1_benchmark.png)

# Conclusion

xxx

# License of used photos #1

~~~
~~~
