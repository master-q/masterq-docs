# Functional IoT: Hardware and Platform
![background](img/The_Fuji_from_the_mountains_of_Totomi.png)

Kiwamu Okabe

# Hardware for hobby programmer
![background](img/Fujimi_Fuji_view_field_in_the_Owari_province.png)

* Industrial programmer chooses closed platform.
* But we are hobby programmer who loves open platform.
* Example of open platform is GCC, Arduino and etc.
* Let's take a look such open platform.

# Hardware and Platform
![background](img/Fuji_seen_through_the_Mannen_bridge_at_Fukagawa.png)

Which layer do you write using your language?

![inline](draw/layer.png)

# Arduino (compatible board)
![background](img/arduino_uno.png)

* CPU: AVR (8-bit)
* Compiler: GCC / Middleware: open source
* Network: Ethernet / C++ middleware
* Development board: 6.90 + 5.95 = US $12.85

```
https://www.arduino.cc/
http://www.aliexpress.com/item/new-Starter-Kit-UNO-R3-mini-Breadboard-LED-jumper-wire-button-for-Arduino-compatile-free-shipping/2050828592.html
http://www.aliexpress.com/item/UNO-Shield-Ethernet-Shield-W5100-R3-UNO-Mega-2560-1280-328-UNR-R3-only-W5100-Development/1571438805.html
```

# MSP430
![background](img/launchpad-mspexp430g2-01.png)

* CPU: MSP430 (16-bit)
* Compiler: GCC / Middleware: open source
* No network support
* Development board: US $9.99

```
http://ti.com/launchpad
https://github.com/pabigot/bsp430
```

# Pinguino
![background](img/pinguino-K-08010.png)

* CPU: PIC18 (8-bit) / PIC32 (32-bit)
* Compiler: SDCC
* No network support
* Development board: 950 + 4500 = ￥5450

```
http://pinguino.cc/
http://sdcc.sourceforge.net/
http://akizukidenshi.com/catalog/g/gK-08010/
http://akizukidenshi.com/catalog/g/gM-03608/
```

# mbed
![background](img/mbed-Arch-Max.png)

* CPU: Cortex-M (32-bit)
* Compiler: GCC / RTOS: open source
* Network: Ethernet / Big C++ middleware
* Debugger: pyOCD
* Development board: US $39.95

```
https://developer.mbed.org/ https://github.com/mbedmicro/pyOCD
http://www.seeedstudio.com/depot/Arch-Max-CortexM4-based-Mbed-enable-development-board-p-2201.html
```

# FreeRTOS
![background](img/stm32f7-discovery-freertos.png)

* CPU: MSP430, Cortex-M, ARM7, RX, PowerPC, etc.
* Compiler: GCC / RTOS: open source
* Network: Ethernet / Doesn't have gool HAL?
* Debugger: OpenOCD, stlink
* Development board: ￥7500

```
http://www.freertos.org/ http://openocd.org/ https://github.com/texane/stlink
https://github.com/sorrowed/STM32-F7-Ethernet-emWin
http://akizukidenshi.com/catalog/g/gM-09880/
```

# ChibiOS/RT
![background](img/stm32f7-discovery-chibios.png)

* CPU: AVR (8-bit), Cortex-M (32-bit)
* Compiler: GCC / RTOS: open source
* Network: Ethernet / Tiny RTOS supports HAL
* Debugger: OpenOCD, stlink
* Development board: ￥7500

```
http://www.chibios.org/
http://akizukidenshi.com/catalog/g/gM-09880/
```

# chopstx
![background](img/fsm-55.png)

* CPU: Cortex-M (32-bit / No MMU)
* Compiler: GCC / RTOS: open source
* No network support / Tiny thread library
* Debugger: OpenOCD
* Development board: 10 + 2.74 + 6.90 = US $19.64

```
http://git.gniibe.org/gitweb/?p=chopstx/chopstx.git
http://www.seeedstudio.com/depot/FSM55-LED-Matrix-Display-p-2121.html
http://www.aliexpress.com/item/Free-Shipping-1SET-ST-Link-st-link-V2-for-STM8S-STM8L-STM32-Cortex-M0-Cortex-M3/1619197946.html
```

# ESP8266
![background](img/ESP-WROOM-02-dev.png)

* CPU: Xtensa lx106 (32-bit / No MMU)
* Compiler: GCC / RTOS: closed source
* Network: Wi-Fi
* Debugger: GDB stub
* Development board: ￥2160

```
http://www.esp8266.com/
https://www.switch-science.com/catalog/2500/
https://github.com/espressif/esp-gdbstub
```

# ToCoNet (Jennic)
![background](img/twe-lite.png)

* CPU: OpenRISC 1200 (32-bit / No MMU)
* Compiler: GCC / RTOS: open source?
* Network: IEEE802.15.4 (ZigBee)
* Development board: 1620 + 2430 = ￥4050

```
http://mono-wireless.com/
http://akizukidenshi.com/catalog/g/gK-06759/
http://akizukidenshi.com/catalog/g/gM-08264/
```

# Raspberry Pi
![background](img/Raspberry-Pi.png)

* CPU: ARM11 or Cortex-A (32-bit / with MMU)
* Compiler: GCC, LLVM, etc
* Network: Ethernet / Wi-Fi (USB dongle)
* Debugger: OpenOCD
* Development board: ￥5000 (1 Model B+)

```
https://www.raspberrypi.org/
http://akizukidenshi.com/catalog/g/gM-08718/
http://asmichi.hatenablog.com/entry/2014/08/15/203057
```

# Trends
![background](img/The_Great_Wave_off_Kanagawa.png)

![inline](img/trends.png)

# License of photos #1
![background](img/creative_commons.png)

```
* 神奈川沖浪裏
  https://commons.wikimedia.org/wiki/File:The_Great_Wave_off_Kanagawa.jpg
  Copyright: 葛飾北斎 / License: public domain
* 深川万年橋下
  https://commons.wikimedia.org/wiki/File:Fuji_seen_through_the_Mannen_bridge_at_Fukagawa.jpg
  Copyright: 葛飾北斎 / License: public domain
* 遠江山中
  https://commons.wikimedia.org/wiki/File:The_Fuji_from_the_mountains_of_Totomi.jpg
  Copyright: 葛飾北斎 / License: public domain
* 尾州不二見原
  https://commons.wikimedia.org/wiki/File:Fujimi_Fuji_view_field_in_the_Owari_province.jpg
  Copyright: 葛飾北斎 / License: public domain
```

# License of photos #2
![background](img/creative_commons.png)

```
* Creative Commons BBB | Flickr - Photo Sharing!
  https://www.flickr.com/photos/steren/2732488224/
  Copyright: Steren Giannini / License: CC BY 2.0
```
