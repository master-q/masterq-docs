# RTOS application verified by VeriFast, and future plan

![background](img/ESAT_KULeuven.png)

Kiwamu Okabe

# What's VeriFast?
# Why need VeriFast?
# VeriFasts find bugs on Linux kernel
# How to use VeriFast?
# A problem on ChibiOS/RT RTOS
# Let's verify RTOS application!
# Demo
# Verification platform for RTOS

xxx Figure

# Limitation of VeriFast today

* It need two C language header.
* One is for compiling C code.
* Another is for verifying C code.
* They may have some semantic gap, which cause miss verification.

# We need C99 compatible VeriFast!

* If VeriFast supports C99, pseud C header for verification.
* It means original Linux kernel code can include the verfication.
* Linux kernel quality is shaped by human review, today.
* If there is C99 compat VeriFast, we keep the quality with verification!

# Advice from \@ftake

```
https://twitter.com/ftake/status/872436138308378624
```

![inline](img/ftake.png)

# Next action: survey clang parser

* Is following good for first step?
* https://clangupc.github.io/clang-upc2c/
* Clang UPC2C Translator
* It translates UPC (Unified Parallel C) code into C language code
