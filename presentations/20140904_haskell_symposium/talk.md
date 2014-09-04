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
Existing OS is developed in C language, such like human society.
We would like to fully rewrite the OS with safer strong type, such like android society.
Our rewriting strategy is:
first focus a module in existing OS;
second rewrite it with strongly typed language;
finally connect every modules with FFI.
Therefore, we call it as "Snatch-driven development".

# Why we use jhc ?

I think this meeting is Haskell Symposium.
Of course, we choose Haskell language, however use jhc Haskell compiler.
Why?
Please see the table,
that is explain Hello world executable binary's size, number of undefined symbols and number of dependent libraries.
Lesser value means the compiler is potable for out of POSIX and good for system programming.
We choose jhc for the feature and flexible type system.

# Unix-like OS needs reentrancy

O.K. We decided the design method and language implementation.
What's next?
Actually, Unix-like OS is hard to rewrite or snatch.
Unix-like OS needs reentrancy, but many language implementations don't have it.
And reentrancy is needed to implement hardware interrupt handler.
The handler is used by preemptive multitasking.
So, the multitasking is depends on by Unix-like OS.

# What's reentrancy ?

So, what is reentrancy?
For example, context A and B share lock Y.
First, context A call subroutine X that acquire lock Y.
Second, some context switch occurs and switch to context B, while X is locking Y.
Third, context B also call subroutine X that try to acquire lock Y, however Y isn't unlock forever!
In this case, subroutine X is not reentrant.

# How do we get reentrancy in C ?

How C language shape reentrant code?
It's simple.
C language contexts are isolated.
If programmers develop code carefully, the codes are isolated.

# What's C language Context ?

And what is C language context?
They are registers in CPU and call stack in memory.
The C language context switch replaces them with the other.

# Problem: Interrupt and GC

So, this is the problem between reentrancy and GC.
We can't permit that hardware interrupt occurs while GC is running,
because the GC isn't reentrant.
With such like language implementation, you can't write interrupt handler using the language.

# Root of the problem

Let's think the problem in GHC's case.


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
