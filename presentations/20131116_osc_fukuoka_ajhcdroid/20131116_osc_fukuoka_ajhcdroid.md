# Metasepi team meeting #8:　Haskell apps on Android NDK
![background](img/android_idf13.png)

Kiwamu Okabe

# Who am I?
![background](img/enjoy.png)

* http://www.masterq.net/
* Twitter: @master_q
* Organizer of Metasepi Project
* A developer of Ajhc Haskell compiler
* A Debian Maintainer
* 10 years' experience in developing OS using NetBSD.

# Agenda
![background](img/shinkansen.png)

* [1] Demo
* [2] What is Ajhc?
* [3] What is Metasepi?
* [4] What is compiler to build OS
* [5] How to use Ajhc
* [6] Haskell on Android NDK
* [7] Status report of framework

# [1] Demo
![background](img/demo_mov_android.png)

* A touchable cube demo.
* Haskell App runs on Android NDK.
* GC is breaked by ndk-gdb debugger.
* You can watch the movie following.

http://bit.ly/jhcdroid

# Demo hardware
![background](img/harddisk.png)

Nexus 7 (2012) / NVIDIA Tegra 3

![inline](img/nexus7.png)

# Demo software
![background](img/ice.png)

github.com/ajhc/demo-android-ndk

![inline](draw/app_on_android.png)

# [2] What is Ajhc?
![background](img/ajhc.png)

http://ajhc.metasepi.org/

* Ajhc := A fork of jhc
* jhc := John's Haskell Compiler
* http://repetae.net/computer/jhc/
* Jhc outputs binary that has low-memory-footprint and runs fast.
* Good for embedded software.

# Why need Ajhc?
![background](img/fphaskell.png)

* GHC is de facto standard on Haskell.
* GHC := Glasgow Haskell Compiler
* http://www.haskell.org/ghc/
* Why need another Haskell compiler?
* To develop kernel named "Metasepi".

# [3] What is Metasepi?
![background](img/metasepi.png)

http://metasepi.org/

* Unix-like OS designed by strong type.
* Using ML or more strong type lang.

Haskell http://www.haskell.org/

OCaml http://caml.inria.fr/

MLton http://mlton.org/

. . . and suchlike.

# Why need Metasepi?
![background](img/mud.png)

* We have already Linux or Windows.
* But the developers are suffering.
* If use the kernel changed by you,
* you will get many runtime error.
* Difficult even to reproduce it.

# Doesn't OSS have good quality?
![background](img/egg.png)

* "The Cathedral and the Bazaar"
* "Given enough eyeballs, all bugs are shallow."

~~~
http://cruel.org/freeware/cathedral.html
~~~

* But if you develop your own product reusing OSS...

# Low quality out of OSS umbrella
![background](img/jump.png)

![inline](draw/oss_quality.png)

# Type safety
![background](img/safe.png)

* Less runtime errors.
* "数理科学的バグ撲滅方法論のすすめ"

~~~
http://itpro.nikkeibp.co.jp/article/COLUMN/20060915/248230/
~~~

![inline](draw/2013-01-18-few_error.png)

# Kernel desperately wants type
![background](img/stop.png)

* Kernels are developed with C lang.
* Error on user space => SEGV
* Error on kernel space => halt!
* Should design kernel with the greatest care.
* C language is safe?

# [4] What is compiler to build OS
![background](img/c.png)

* Need strong type.
* Need flexibility such as C language.
* Create it if there are not!
* From scratch? No thank you...
* Look for our compiler base.

# Want POSIX free compiler
![background](img/hiking.png)

Programs to print "hoge" on terminal.

![inline](img/compiler_list.png)

The lesser depends on POSIX, the smaller values.

# Jhc output has only 20 undef
![background](img/20.png)

~~~
$ nm hs.out | grep "U "
                 U _IO_putc@@GLIBC_2.2.5
                 U __libc_start_main@@GLIBC_2.2.5
                 U _setjmp@@GLIBC_2.2.5
                 U abort@@GLIBC_2.2.5
                 U ctime@@GLIBC_2.2.5
                 U exit@@GLIBC_2.2.5
                 U fflush@@GLIBC_2.2.5
                 U fprintf@@GLIBC_2.2.5
                 U fputc@@GLIBC_2.2.5
                 U fputs@@GLIBC_2.2.5
                 U free@@GLIBC_2.2.5
                 U fwrite@@GLIBC_2.2.5
                 U getenv@@GLIBC_2.2.5
                 U malloc@@GLIBC_2.2.5
                 U memset@@GLIBC_2.2.5
                 U posix_memalign@@GLIBC_2.2.5
                 U realloc@@GLIBC_2.2.5
                 U setlocale@@GLIBC_2.2.5
                 U sysconf@@GLIBC_2.2.5
                 U times@@GLIBC_2.2.5
~~~

# Jhc is translator to C language
![background](img/mix.png)

![inline](draw/2012-12-22-jhc_compile.png)

# Easy to cross build
![background](img/cross.png)

![inline](draw/cross_compile.png)

# Survive burning out
![background](img/goal.png)

Let's develop in dogfooding style. (The method is called "Snatch".)

![inline](draw/2012-12-27-arafura_design.png)

# [5] How to use Ajhc
![background](img/blank.png)

Case of Ubuntu 12.04 amd64.

~~~
$ sudo apt-get install haskell-platform libncurses5-dev gcc m4
$ cabal update
$ export PATH=$HOME/.cabal/bin/:$PATH
$ cabal install ajhc
$ which ajhc
/home/USER/.cabal/bin/ajhc
$ echo 'main = print "hoge"' > Hoge.hs
$ ajhc Hoge.hs
$ ./hs.out
"hoge"
~~~

You can use on Windows or Mac OS X.

# Detail of usage
![background](img/minix.png)

Please read "Ajhc User's Manual".

* ajhc.metasepi.org/manual.html

Also you can read in Japanese.

* ajhc.metasepi.org/manual_ja.html

# [6] Haskell on Android NDK
# [7] Status report of framework

# PR: Call For Articles
![background](img/c84.png)

* http://www.paraiso-lang.org/ikmsm/
* Fanzine of functional programming.
* About Haskell or OCaml or . . .
* Article about Ajhc in C84 book.
* Call me if you read it!

~~~
http://www.paraiso-lang.org/ikmsm/books/c85.html
~~~
