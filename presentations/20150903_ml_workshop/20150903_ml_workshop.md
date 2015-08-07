# Arduino programming of ML-style in ATS

Kiwamu Okabe @ METASEPI DESIGN / Hongwei Xi @ Boston University

# Demo: xxx

* LCD greeting
* LCD sort
* LED fadein

# Arduino hardware

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
# Approach 2: DSL
# Approach 3: G.P.P language

Use general-purpose programming language.

# Pros/Cons
# ATS language

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
