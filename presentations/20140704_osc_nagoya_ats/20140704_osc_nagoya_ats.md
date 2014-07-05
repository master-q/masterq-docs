# Metasepi team meeting #14:　ATS programming on MCU

Kiwamu Okabe @ Metasepi Project

# Who am I?
![background](img/enjoy.png)

* http://www.masterq.net/
* Self employed software engineer
* Trade name := METASEPI DESIGN
* Founder of Metasepi Project
* A Debian Maintainer
* 10 years' experience in developing OS using NetBSD

# Demo: NetBSD driver in Haskell
![background](img/demo_mov_netbsd.png)

* NetBSD audio driver play sound
* The driver's interrupt handler rewrited using Haskell
* Watch the movie at following

~~~
https://www.youtube.com/watch?v=XEYcR5RG5cA
~~~

* Paper for Haskell Symposium 2014

~~~
http://metasepi.org/doc/metasepi-icfp2014.pdf
~~~

# Ajhc Haskell compiler
![background](img/ajhc.png)

http://ajhc.metasepi.org/

* Ajhc := Arafura designed jhc
* jhc := John's Haskell Compiler
* http://repetae.net/computer/jhc/
* Jhc outputs binary that has low-memory-footprint and runs fast.
* Good for embedded software.

# Agenda

* [1] What is Ajhc?
* [2] What is Metasepi?
* [3] Demo using ATS language
* [4] What is ATS?
* [5] ATS programming on MCU

# What is Metasepi?
![background](img/metasepi.png)

http://metasepi.org/

* Unix-like OS designed by strong type.
* Using ML or more strong type lang.

# Why need Metasepi?

![background](img/mud.png)

* We have already Linux or Windows.
* But the developers are suffering.
* If use the kernel changed by you,
* you will get many runtime error.
* Difficult even to reproduce it.

# Type safety
![background](img/safe.png)

* Less runtime errors.
* "数理科学的バグ撲滅方法論のすすめ"

~~~
http://itpro.nikkeibp.co.jp/article/COLUMN/20060915/248230/
~~~

![inline](draw/2013-01-18-few_error.png)

# How create Metasepi?

![inline](draw/iterative.png)

# Snatch-driven development

Dogfooding style.

![inline](draw/2012-12-27-arafura_design.png)

# Demo: ATS on raw Arduino

~~~
https://github.com/fpiot/arduino-mega2560-ats
~~~

![inline](draw/demo_ats_arduino.png)

# Demo: ATS on mbed platform

~~~
https://github.com/fpiot/mbed-ats
~~~

![inline](draw/mbed_and_ats.png)

# What is ATS language?
![background](img/ats_hongwei.png)

http://www.ats-lang.org/

* Syntax like ML
* Dependent types
* Linear types
* Without any runtime
* Optional GC

# ATS compile flow

![inline](draw/flow.png)

# ATS programming on MCU

You can choose the following 2-way.

On BareMetal hardware

* Arduino (8-bit AVR)
* Cortex-M (32-bit ARM)

On RTOS

* mbed
* ChibiOS/RT

# ATS on BareMetal hardware

Read/write memory using pointer.

![inline](draw/ats_rw_pointer.png)

# ATS on RTOS

Interaction with C.

![inline](draw/ats_interaction_c.png)

# Produce/Consume Linear Type

![inline](draw/linear_consume.png)

# At-view

![inline](draw/at-view.png)

# Japan ATS User Group
![background](img/jats-ug_like.png)

http://jats-ug.metasepi.org/

* In a parody of http://jaws-ug.jp/
* Translate ATS docs into Japanese
* Push the Facebook like button, now!

# Follow me!

https://twitter.com/jats_ug

![background](img/twitter.png)

![inline](img/jats-ug_logo_v1.png)
