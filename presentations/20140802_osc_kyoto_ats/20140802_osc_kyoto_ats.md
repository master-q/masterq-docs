# Metasepi team meeting #15:　Safety on ATS language + MCU

Kiwamu Okabe @ Metasepi Project

# Who am I?
![background](img/enjoy.png)

* http://www.masterq.net/
* Self employed software engineer
* Trade name := METASEPI DESIGN
* Founder of Metasepi Project
* A Debian Maintainer
* 10 years' experience in developing OS using NetBSD

# Agenda

* [1] What is Metasepi?
* [2] Demo using ATS language
* [3] What is ATS?
* [4] Why ATS is safe?
* [5] ATS programming on MCU

# [1] What is Metasepi?
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

# [2] Demo: ATS on raw Arduino

~~~
https://github.com/fpiot/arduino-mega2560-ats
~~~

![inline](draw/demo_ats_arduino.png)

# Demo: ATS on mbed platform

~~~
https://github.com/fpiot/mbed-ats
~~~

![inline](draw/mbed_and_ats.png)

# [3] What is ATS language?
![background](img/ats_hongwei.png)

http://www.ats-lang.org/

* Syntax like ML
* Dependent types
* Linear types
* Without any runtime
* Optional GC

# ATS compile flow

![inline](draw/flow.png)

# [4] Why ATS is safe?

# [5] ATS programming on MCU

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

# Japan ATS User Group
![background](img/jats-ug_like.png)

http://jats-ug.metasepi.org/

* In a parody of http://jaws-ug.jp/
* Translate ATS docs into Japanese
* Push the Facebook like button, now!

# Many translated documents

~~~
* ATSプログラミング入門
  http://jats-ug.metasepi.org/doc/ATS2/INT2PROGINATS/index.html
* ATSプログラミングチュートリアル
  http://jats-ug.metasepi.org/doc/ATS2/ATS2TUTORIAL/index.html
* Effective ATS
  https://github.com/jats-ug/translate/blob/master/Manual/EffectiveATS.md
* MLプログラマ向けATS言語ガイド
  https://github.com/jats-ug/translate/blob/master/Web/cs.likai.org/ats/ml-programmers-guide-to-ats.md
* 安全なプログラミング言語を使って heartbleed を防ぐには
  https://github.com/jats-ug/translate/blob/master/Web/bluishcoder.co.nz/2014/04/11/preventing-heartbleed-bugs-with-safe-languages.md
* 状態を持つ観 (view) を通じてポインタを扱う安全なプログラミング
  https://github.com/jats-ug/translate/blob/master/Paper/SPPSV-padl05/SPPSV-padl05.md
~~~

# Follow me!

https://twitter.com/jats_ug

![background](img/twitter.png)

![inline](img/jats-ug_logo_v1.png)
