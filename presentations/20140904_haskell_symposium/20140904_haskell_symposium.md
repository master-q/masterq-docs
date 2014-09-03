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

* This slide: http://bit.ly/ajhc-icfp2014

# Demo architecture
![background](img/netbsd.png)

![inline](draw/demo_arch_netbsd.png)

# Kernel developers want type
![background](img/world_without_trees.png)

* Kernels are developed with C.
* We should design kernel with the greatest care.
* C language is unsafe!

# Kernel Bug #1: Buffer overrun
![background](img/panic.png)

* Pointer to array doesn't know the length.

![inline](draw/buffer_overrun.png)

# Kernel Bug #2: Page fault
![background](img/panic.png)

* Page fault in user space => SEGV
* Page fault in kernel space => Halt!

![inline](draw/page_fault.png)

# Kernel Bug #3: Weak type
![background](img/panic.png)

* Lots of (void *) and unsafe coercion.
* NetBSD kernel uses it 45130 times!

~~~
$ pwd
/home/kiwamu/src/netbsd/sys
$ grep "void \*" `find . -name "*.c"` | wc -l
45130
~~~

* Kernel developers frequently use (void *) for flexibility. It's realy BAD, but there is no other option.

# Metasepi Project
![background](img/thinkpad.png)

http://metasepi.org/

* Unix-like OS designed by strong type.
* We want to use Metasepi OS for daily desktop usage (e.g. web browser, programming, office suite, ... etc.)
* We have surveyed may functional languages (e.g. Haskell, OCaml, MLton, ... etc.)

# Scratch or Rewrite
![background](img/climbing.png)

![inline](draw/need_unixlike_kern.png)

# Snatch-driven development #1
![background](img/konami.png)

http://en.wikipedia.org/wiki/Snatcher

![inline](draw/snatch-system.png)

# Snatch-driven development #2
![background](img/konami.png)

![inline](draw/2012-12-27-arafura_design.png)

# Why we use jhc ?
![background](img/john_nevada.png)

Comparison of programs to print "hoge" on terminal.
The smaller the values, the lesser is the dependency on POSIX,
the more suitable for system programming.

![inline](img/compare_compiler_ats.png)

# Unix-like OS needs reentrancy
![background](img/re-entry.png)

* Why we need reentrancy?
* Because interrupt handler should be reentrant.
* Why we need interrupt handler?
* Because preemptive multitasking uses it.
* Why we need the multitasking?
* Because Unix-like OS depends on it.

# What's reentrancy ?
![background](img/block_diagram.png)

~~~
Reentrant code can be interrupted in the middle of its execution and then safely called again ("re-entered") before its previous invocations complete execution.
~~~

![inline](draw/reentrant.png)

# How do we get reentrancy in C ?
![background](img/pdp11.png)

* C language contexts are isolated.

![inline](draw/context_switch.png)

# What's C language Context ?
![background](img/pdp11.png)

![inline](draw/context_c.png)

# Problem: Interrupt and GC
![background](img/supernova.png)

![inline](draw/switch_ongc.png)

# Root of the problem

* GHC's Haskell context is global and single.
* There is only one GC heap on GHC.
* If interrupt occurs while GC is running, the interrupt context can't use GC heap, because the running GC is paused!
* Therefore GHC's binary isn't reentrant.

# How we can fix this problem
![background](img/headshift_business_card_discussion.png)

We re-define the Haskell Context.

![inline](draw/define_haskell_context.png)

# Context-Local Heaps (CLHs)
![background](img/great_wall_china.png)

* Idea: Isolate contexts by local heap

![inline](draw/heapstyle.png)

# What's Haskell Context on CLHs?
![background](img/great_wall_china.png)

![inline](draw/context_haskell.png)

# Haskell Context life cycle (CLHs)
![background](img/great_wall_china.png)

![inline](draw/arena_lifecycle.png)

# Isolated contexts are reentrant?
![background](img/great_wall_china.png)

![inline](draw/isolated_contexts_reentrant.png)

# Benchmark
![background](img/paper_airplane.png)

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
![background](img/metasepi.png)

~~~




~~~

![inline](draw/contributors.png)

# Conclusion

* Can we write Unix in Haskell?
* => Yes!
* How we realize Reentrant GC?
* => With Context-Local Heaps (CLHs)!
* Can we implement CLHs in other compilers?
* => Yes! Of course, GHC can do it!
