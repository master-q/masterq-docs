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
GHC's GC code is not reentrant, because GHC has only one Haskell context and onlye one GC heap region.

# How we can fix this problem

To fix the problem, we re-defined the Haskell context policy on jhc Haskell compiler.
According to the policy, Haskell context is born at entry point C to Haskell,
and the context dies at return point Haskell to C.
Also, new context is born when Haskell language calls C function that call the other Haskell function.
In GHC case, there Haskell contexts are equal.
With this policy, we can realize that Haskell contexts become to be isolated.

# Context-Local Heaps (CLHs)

So, let's implementation the Haskell context policy.
We assigned own Haskell heap to Haskell context.
GHC manages GC heap as global one.
Our customized jhc manage GC heap as context local one.
This local heap make Haskell context becomes isolated one.
We call the GC implementation technique as "Context-Local Heaps".
However Context-Local Heaps contexts can't use type for connection of contexts.
They only use binary interface, such like raw pointer.

# What's Haskell Context on CLHs?

What's Haskell context on Context-Local Heaps?
Again, C language context includes registers in CPU and call stack in memory.
The Haskell context includes the C language's ones
and GC infomations that are arena for GC structure, GC heap, GC stack as GC root.
From another perspective, Context-Local Heaps add add GC infomation to C language ABI while Haskell context is running.
It's simple idea.

# Haskell Context life cycle (CLHs)

Finally, I explain life cycle of the Haskell context.
First, jhc runtime function jgc\_alloc\_init() is called, when C language call Haskell function exported.
The function assigns arena and GC stack as GC root to new Haskell context,
that keeps them as 1st and 2nd argument in C language.
Second, the context initialize arena and GC stack.
Third, sometimes the context try to allocate new thunk on GC heap.
However virgin arena does not have any GC heap.
Therefore in the first place, the context gets new GC heap from jhc runtime's pool.
Also, if the context eats all of GC heap, then more GC heap will be assigned.
Finally, arena, GC stack and GC heap are return to runtime's pool, when the Haskell context return to C language.

# Isolated contexts are reentrant?

Let's see a result of our effort.
Now jhc Haskell compiler GC is isolated and reentrant.
Thread A and thread B are thread-safe.
Thread B can run own GC while thread B is running own GC.
Also if hardware interrupt occurs while thread A is running own GC, interrupt handler A can run own GC.
Haskell code obtains reentrancy with Context-Local Heaps!

# Benchmark

This is benchmark while Haskell code is running in NetBSD kernel.
(O) means original NetBSD kernel.
And (S) means the kernel partly rewriten using jhc Haskell compiler.
(N) means (S) with naive GC parameter that occurs GC mostly frequently.
Memory size and CPU load stay constant between them.
On (N) kernel, many GC occurs.
However no GC occurs on (N) kernel.
What's this?
In kernel, most context is event driven and short-lived.
For the context, GC heap chunk size is enough to run no GC.

# Thank's for contributors !

Metasepi Project is supported by many people.
Thank's a lot!

# Conclusion

Conclusion.
First, we can write Unix-like kernel in Haskell.
Second, with Context-Local Heaps technique, we can realize reentrant GC on jhc Haskell compiler.
Finally, we can use Context-Local Heaps technique on the other language implementation, such like OCaml, MLton, SML/NJ and GHC!
During next break, we will show this NetBSD demonstration and the others at the corner.
Thank's for your attention!

Ah, one more thing...
$ carettah +RTS -rtsopts
This presentation is also developed in Haskell language.
Thank's a lot!
