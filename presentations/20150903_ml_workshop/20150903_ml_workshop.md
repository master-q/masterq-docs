# Arduino programming of ML-style in ATS

Kiwamu Okabe @ METASEPI DESIGN / Hongwei Xi @ Boston University

# Demo: xxx

* http://youtu.be/5uPue0Jo1nc
* LCD sort
* LED fadein

# Demo: Software Architecture

https://github.com/fpiot/arduino-ats

![inline](draw/demo_arch.png)

# Arduino Uno hardware

It's poor.

* 8-bit Harvard architecture
* Flash Memory: 32 KB
* SRAM: 2 KB

Many people use C language on the hardware.

# Problem of C language

* Memory/Resource leak
* Out of bounds
* Weak type

We need functional language on embedded system!

# Approach 1: Virtual machine

![inline](draw/vm.png)

# Approach 2: DSL

![inline](draw/dsl.png)

# Approach 3: Direct language

![inline](draw/direct.png)

# Comparison of the approaches

We choose the 3rd approach.

![inline](draw/comparison.png)

# ATS language
![background](img/ats_logo_on_display.png)

* http://www.ats-lang.org/
* Dependent types
* Linear types
* Optional GC
* Optional malloc/free
* Optional run-time

# Functional style programming

* Envless function
* Unbind closure
* Stack closure
* Template function

# Safety shaped by ATS

* Termination metrics
* At-view
* View

# Conclusion
