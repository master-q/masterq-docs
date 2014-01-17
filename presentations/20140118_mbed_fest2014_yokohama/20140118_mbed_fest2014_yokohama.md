# Functional MCU programming

Metasepi Project / Kiwamu Okabe

# Who am I ?
![background](img/enjoy.png)

* http://www.masterq.net/
* Twitter: @master_q
* Organizer of Metasepi Project
* A developer of Ajhc Haskell compiler
* A Debian Maintainer
* 10 years' experience in developing OS using NetBSD.

# Kick off workshop at Nagoya

Functional MCU programing workshop

* At Nagoya
* Meeting minutes

~~~
http://metasepi.org/posts/2014-01-05-mbed_fp_0.html
~~~

# Why functional lang for MCU?
![background](img/iot.png)

* We will live in IoT world
* Can't debug IoT device on field
* Should avoid runtime error
* We need strong type !

# Develop enviroment overview

![inline](draw/env_overview.png)

# Which MCU? Of course mbed !

Why?

* Major
* 32bit
* Large memory size (> 20kB)
* Ethernet

# Which Board is good for us...

* mbed LPC1768 (5,200 YEN)

~~~
https://mbed.org/platforms/mbed-LPC1768/
~~~

* Seeeduino Arch Pro ($43.41)

~~~
http://www.seeedstudio.com/depot/arch-pro-p-1677.html
~~~

* FRDM-KL46Z (1,634 YEN)

~~~
http://www.freescale.com/webapp/sps/site/prod_summary.jsp?code=FRDM-KL46Z
~~~

# Cross compiler: (A)jhc
![background](img/ajhc.png)

http://ajhc.metasepi.org/

* Language: Haskell
* Haskell is major !
* Need GC heap = Need more memory
* Experience running on MCU

~~~
ARM Cortex-M
https://github.com/ajhc/demo-cortex-m3
~~~

# Cross compiler: ATS
![background](img/ats_hongwei.png)

http://www.ats-lang.org/

http://jats-ug.metasepi.org/

* Language: ML
* Optional GC = Need low memory
* No experience running on MCU

# Cross compiler: Rust
![background](img/rust.png)

http://www.rust-lang.org/

* Language: Own syntax like C or JS
* Optional GC = Need low memory
* Experience running on MCU

~~~
ARM Cortex-M
https://github.com/neykov/armboot
~~~

* @pirapira knows detail of it

# gdbserver (Debugger)
![background](img/rms.png)

* Gdb is major debugger
* But gdb is only for the program running on your PC
* How debug program running on MCU ?
* Gdbserver is good for the use case
* There are many implementation

# gdbserver: pyOCD
![background](img/python.png)

https://github.com/mbedmicro/pyOCD

* Only for MCU using CMSIS-DAP
* But now support only mbed LPC1768
* In future, support more board ?

# gdbserver: OpenOCD
![background](img/openocd.png)

http://openocd.sourceforge.net/

* Comming CMSIS-DAP support!
* Need HIDAPI

http://www.signal11.us/oss/hidapi/

~~~
$ git clone git://github.com/signal11/hidapi.git
$ cd hidapi
$ ./bootstrap
$ ./configure
$ make
$ sudo make install
~~~

# OpenOCD enabling CMSIS-DAP

~~~
$ git clone git://git.code.sf.net/p/openocd/code openocd
$ cd openocd
$ ./bootstrap
$ ./configure --enable-cmsis-dap
--ship--
Olimex ARM-JTAG-EW Programmer           yes (auto)
CMSIS-DAP Compliant Debugger            yes
$ make
$ sudo make install
~~~

# Choose our environment set

![inline](draw/choose_env.png)
