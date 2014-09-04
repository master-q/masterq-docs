# Writing NetBSD Sound Drivers in Haskell

Hi, jhc hackaers!
My name is Kiwamu Okabe.
I'm a self employed software engineer at METASEPI DESIGN.
Today, I talk about kernel programming in Haskell.
However, I don't touch detail of kernel programming,
and focus our Reentrant GC for real kernel programming.

# Demo: NetBSD driver in Haskell

First, this slide is found at the bit.ly URL.
The proof of the pudding is in the eating.
Let's see the demo!
Play music on NetBSD kernal.
I think the sound is normal.
However, set break point at Ajhc Haskell compiler's GC entry symbol,
and continue to run kernel.
The kernel stopped at the break point.
Continue. Break. Continue. Break.
GC occurs many times at kernel space!
Backtrace say that...
first hardware interrupt kicks Haskell "hdaudioIntr" function,
second the function allocates new heap from Ajhc compiler runtime,
and finally the runtime starts GC.
So, delete all breakpoints, and continue.
Then the sound becomes normal, too.
Additionally, CPU is mostly idle!
You could watch the mouvie at youtube.

# Demo architecture

The demo architicture is drawed as the figure.
First, hardware interrupt kicks Ajhc Haskell compiler runtime.
Second, the runtime assign virgin Haskell context for running Haskell function.
The context includes structure of Haskell GC heap.
Third, Haskell code allocates new thunk, while the code running.
Sometimes it causes GC.
Finally, Haskell code return to C language,
Ajhc runtime clean up the Haskell context as virgin and return it to pool.
Haskell code anytime gets virgin GC heap at entry point.

# Kernel developers want type

Let's start a long story.
Most kernels are developed with C language.
Some existing kernels such like Linux already have good quality.
Why we need strong type for kernel programming?
One answer is that designing kernel needs greatest care.
And the other is C is unsafe language.

# Kernel Bug #1: Buffer overrun

First one of many kernel bugs is buffer overrun.
Fundamentally, C language pointer to array doesn't know length of the array.
Also, C programmer sometimes use an extensible struct that isn't known by the pointer.

# Kernel Bug #2: Page fault

Second is page fault in kernel space.
You can debug page fault as segmentation fault in user space.
However you will see halt when page fault occurs in kernel space!
It's hard to debug.

# Kernel Bug #3: Weak type

Third is frequently using (void *) type in kernel.
For example, NetBSD kernel uses (void *) forty five thousand times!
Why kernel developers use it?
Simple reason is C language limitation, that have no flexibility.

# Metasepi Project

Now, I would like to talk about our project named Metasepi.
We challenge to create an open-source Unix-like operating system designed with strong type, on Metasepi Project.
The Metasepi aims to be desktop OS for daily usage.

# Scratch or Rewrite

Why does Metasepi focus Unix-like OS?
Because it's caused by design strategy.
It can be chosen from two way, that are building from scratch or rewriting existing OS.
If you choose the scratch way, you should develop everything such like kernel, system call, own compiler and own web browser.
On the other hand, if you choose the rewrite way, you can focus to develop your kernel.
Because the other modules can be re-used.
Therefore, we chosen the rewrite way to develop Metasepi.

# Snatch-driven development #1

By the way, do you know Konami's old video game named "Snatcher"?
The game is rendering of war between human society and android society.
The android kills a human secretly, takes skin of the human as interface and acts like the human using the skin.
Androids replace humans little by little.
Finally, the android society perfectly rewrite the human society keeping same structure.
It's called "Snatch" on the game.

# Snatch-driven development #2

How about OS society?


Therefore, we call it as "Snatch-driven development".

# Why we use jhc ?
# Unix-like OS needs reentrancy
# What's reentrancy ?
# How do we get reentrancy in C ?
# What's C language Context ?
# Problem: Interrupt and GC
# Root of the problem
# How we can fix this problem
# Context-Local Heaps (CLHs)
# What's Haskell Context on CLHs?
# Haskell Context life cycle (CLHs)
# Isolated contexts are reentrant?
# Benchmark
# Thank's for contributors !

Metasepi Project is supported by many people.
Thank's a lot!

# Conclusion

Thank's for your attention!

Ah, one more thing...
$ carettan +RTS -rtsopts
This presentation is also developed in Haskell language.
Thank's.
