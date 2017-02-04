# Hands-on VeriFast with STM32 microcontroller

Kiwamu Okabe

# Today's TOC

* Know ChibiOS/RT which is a RTOS
* Get development environment for ChibiOS/RT
* Build sample ChibiOS/RT application
* Know STM32 microcontroller
* Run the application on STM32 board
* Verify the application using VeriFast

# What's ChibiOS/RT?

* http://www.chibios.org/

xxx spec

# Get the devel-environment #Windows

```
Detail: https://github.com/fpiot/chibios-verifast/blob/master/doc/README.Windows.ja.md
```

* Install following:

```
* https://git-for-windows.github.io/
* https://launchpad.net/gcc-arm-embedded/+download
* https://ttssh2.osdn.jp/index.html.en
* https://cygwin.com/
```

* Install following cygwin packages

```
* cmake
* libusb1.0-devel
```

# Get the devel-environment #Windows

* Open cygwin terminal, and install "stlink":

```
$ git clone https://github.com/texane/stlink.git
$ (cd stlink && make)
$ (cd stlink/build/Release && make install)
```

* Download VeriFast from following URL, unzip it and set PATH to "verifast-XXXXXXX/bin" directory.

```
https://github.com/verifast/verifast#binaries
```

# Get the devel-environment #Windows

* Open cygwin terminal, and checkout ChibiOS/RT source code:

```
$ git clone https://github.com/fpiot/chibios-verifast.git
```

# Get the devel-environment #MacOS

```
Detail: https://github.com/fpiot/chibios-verifast/blob/master/doc/README.MacOS.ja.md
```

* Install brew packages:

```
$ brew tap PX4/homebrew-px4
$ brew update
$ brew install wget git gcc-arm-none-eabi cmake picocom
```

* Install "stlink":

```
$ git clone https://github.com/texane/stlink.git
$ (cd stlink && make)
$ (cd stlink/build/Release && sudo make install)
```

# Get the devel-environment #MacOS

* Download VeriFast, unzip it and set PATH:

```
$ wget http://82076e0e62875f063ae8-929808a701855dfb71539d0a4342d4be.r54.cf5.rackcdn.com/verifast-nightly-osx.tar.gz
$ tar xf verifast-nightly-osx.tar.gz
$ mv verifast-*/ verifast
$ export PATH=`pwd`/verifast/bin:$PATH
```

* Checkout ChibiOS/RT source code:

```
$ git clone https://github.com/fpiot/chibios-verifast.git
```

# Get the devel-environment #Linux

```
Detail: https://github.com/fpiot/chibios-verifast/blob/master/doc/README.Linux.ja.md
```

* Install deb packages:

```
$ sudo apt-get install wget git libgtk2.0-0 libusb-1.0-0-dev libgtk-3-dev build-essential cmake gcc-arm-none-eabi picocom
```

* Install "stlink":

```
$ git clone https://github.com/texane/stlink.git
$ (cd stlink && make)
$ (cd stlink/build/Release && sudo make install)
$ sudo ldconfig
```

# Get the devel-environment #Linux

* Download VeriFast, unzip it and set PATH:

```
$ wget http://82076e0e62875f063ae8-929808a701855dfb71539d0a4342d4be.r54.cf5.rackcdn.com/verifast-nightly.tar.gz
$ tar xf verifast-nightly.tar.gz
$ mv verifast-*/ verifast
$ export PATH=`pwd`/verifast/bin:$PATH
```

* Checkout ChibiOS/RT source code:

```
$ git clone https://github.com/fpiot/chibios-verifast.git
```

# Build a ChibiOS application

* Finally build the code:

```
$ cd chibios-verifast/verifast_demo/STM32/RT-STM32F091RC-NUCLEO
$ make
```

# Coffee break #1
# What's STM32?

xxx spec

# You can free to get own STM32 board!

```
https://developer.mbed.org/platforms/ST-Nucleo-F091RC/
```

* "NUCLEO-F091RC"
* ARM Cortex-M0 CPU / 256 KB Flash / 32 KB SRAM
* ADC / DAC / RTC / I2C / USART / SPI / CAN / HDMI CEC
* Able to flash fiwmware and debug it using GDB
* Thanks a lot, STMicroelectronics!

# How to run application? #Windows

* Connect the board to your PC using USB cable
* Open cygwin terminal, kick "st-util":

```
$ (cd /usr/local/lib && st-util)
```

* Open another cygwin terminal, and write application into the board:

```
$ cd chibios-verifast/verifast_demo/STM32/RT-STM32F091RC-NUCLEO
$ make gdbwrite
```

# How to run application? #Windows

* Continue application from GDB prompt:

```
(gdb) c
```

* Open serial console using TeraTerm with baud rate 38400
* Push "USER" button on the board

# How to run application? #MacOS

* Connect the board to your Mac using USB cable
* Kick "st-util":

```
$ st-util
```

* Open another terminal, and write application into the board:

```
$ cd chibios-verifast/verifast_demo/STM32/RT-STM32F091RC-NUCLEO
$ make gdbwrite
```

# How to run application? #MacOS

* Continue application from GDB prompt:

```
(gdb) c
```

* Open another terminal, and open serial console:

```
$ picocom -b 38400 /dev/tty.usbmodem1423
```

* Push "USER" button on the board

# How to run application? #Linux

* Connect the board to your Mac using USB cable
* Kick "st-util":

```
$ sudo st-util
```

* Open another terminal, and write application into the board:

```
$ cd chibios-verifast/verifast_demo/STM32/RT-STM32F091RC-NUCLEO
$ make gdbwrite
```

# How to run application? #Linux

* Continue application from GDB prompt:

```
(gdb) c
```

* Open another terminal, and open serial console:

```
$ picocom -b 38400 /dev/ttyACM0
```

* Push "USER" button on the board

# Do you see test log of ChibiOS?

* You will see following log on serial console:

```
*** ChibiOS/RT test suite
***
*** Kernel:       3.1.5
*** Compiled:     Jan 15 2017 - 20:38:01
*** Compiler:     GCC 4.8.4 20140725 (release) [ARM/embedded-4_8-branch revision 213147]
*** Architecture: ARMv6-M
*** Core Variant: Cortex-M0
*** Port Info:    Preemption through NMI
*** Platform:     STM32F091xC Entry Level Access Line devices
*** Test Board:   STMicroelectronics NUCLEO-F091RC
```

# Coffee break #2
# What's VeriFast?
# How to verify application?
# What should be verified on ChibiOS?
# Your code already has assertion

There is assertions in the fake header.

# Let's violate the assertion
# Then you will see error on verification
# Homework

# Special thanks

* STMicroelectronics provides STM32 boards.
* Eiwa System Management, Inc. provides this meeting room.
