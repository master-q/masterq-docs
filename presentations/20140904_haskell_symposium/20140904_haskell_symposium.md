# Writing NetBSD Sound Drivers in Haskell

Kiwamu Okabe @ Metasepi Design & Takayuki Muranushi @ RIKEN AICS

# Demo: NetBSD driver in Haskell
![background](img/netbsd.png)

* NetBSD audio driver plays sound.
* The driver's interrupt handler rewrited using Haskell.
* GC occurs in interrupt handler.
* Watch the movie at following.

~~~
https://www.youtube.com/watch?v=XEYcR5RG5cA
~~~

# Demo architecture

![inline](draw/demo_arch_netbsd.png)

# Kernel wants type desperately

* Kernels are developed with C.
* We should design kernel with the greatest care.
* C language is unsafe!

# Kernel problem: Buffer overrun

* Pointer to array doesn't know the length.

![inline](draw/buffer_overrun.png)

# Kernel problem: Page fault

* Page fault in user space => SEGV
* Page fault in kernel space => Halt!

![inline](draw/page_fault.png)

# Kernel problem: Weak type

* Lots of (void *) type
* NetBSD kernel uses it 45130 times!

~~~
$ pwd
/home/kiwamu/src/netbsd/sys
$ grep "void \*" `find . -name "*.c"` | wc -l
45130
~~~

* Kernel developers frequently use (void *) for flexibility. It's realy BAD, but there is no other option.

# Metasepi Project

http://metasepi.org/

* Unix-like OS designed by strong type.
* We want to use Metasepi OS for daily desktop usage (e.g. web browser, programming, office suite, ... etc.)
* We have surveyed may functional languages (e.g. Haskell, OCaml, MLton, ... etc.)

# Scratch or Rewrite

![inline](draw/need_unixlike_kern.png)

# Snatch-driven development #1

http://en.wikipedia.org/wiki/Snatcher

![inline](draw/snatch-system.png)

# Snatch-driven development #2

![inline](draw/2012-12-27-arafura_design.png)

# Why we use jhc ?

Comparison of programs to print "hoge" on terminal.
The smaller the values, the lesser is the dependency on POSIX,
the more suitable for system programming.

![inline](img/compare_compiler_ats.png)

# Unix-like OS needs reentrancy

* Why we need reentrancy?
* Because interrupt handler should be reentrant.
* Why we need interrupt handler?
* Because preemptive multitasking uses it.
* Why we need the multitasking?
* Because Unix-like OS depends on it.

# What's reentrancy ?

~~~
Reentrant code can be interrupted in the middle of its execution and then safely called again ("re-entered") before its previous invocations complete execution.
~~~

![inline](draw/reentrant.png)

# How do we get reentrancy in C ?

* C language contexts are isolated.

![inline](draw/context_switch.png)

# What's C language Context ?

![inline](draw/context_c.png)

# Problem: Interrupt and GC

![inline](draw/switch_ongc.png)

# Root of the problem

* GHC's Haskell context is global and single.
* There is only one GC heap on GHC.
* If interrupt occurs while GC is running, the interrupt context can't use GC heap, because the running GC is paused!
* Therefore GHC's binary isn't reentrant.

# How we can fix this problem

We re-define the Haskell Context.

![inline](draw/define_haskell_context.png)

# What's Haskell Context on Ajhc?

![inline](draw/context_haskell.png)

# Context-Local Heaps (CLHs)

* Idea: Isolate contexts by local heap

![inline](draw/heapstyle.png)

# Haskell Context life cycle (CLHs)

![inline](draw/arena_lifecycle.png)

# Isolated contexts are reentrant?

![inline](draw/isolated_contexts_reentrant.png)

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

# Thank's for contributors !

![inline](draw/contributors.png)

# Conclusion

* Can we write Unix in Haskell?
* => Yes!
* How we realize Reentrant GC?
* => With Context-Local Heaps (CLHs)!
* Can we implement CLHs in other compilers?
* => Yes! Of cause, GHC can do it!

# License of used photos #1

~~~
~~~
