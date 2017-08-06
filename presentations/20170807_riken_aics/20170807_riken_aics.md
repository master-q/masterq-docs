# Past and today of Metasepi project
![background](img/never_do_today.png)

Kiwamu Okabe

# Goal of today
![background](img/goal_of_today.png)

* Understand Metasepi as a research project.
* Debate whether RIKEN AICS should support it or not.

# Table of Contents
![background](img/Compass.png)

* Background of the project
* Runtime error is root of issue
* Result of 1st iteration
* Result of 2nd iteration
* Result of 3rd iteration
* Today's conclusion

# Everything is started at Ricoh
![background](img/ricoh_mfp.png)

* I worked at Ricoh Company, Ltd. to develop embedded devices.
* The device was a Multi Function Printer based on NetBSD OS.
* http://netbsd.org/
* My team didn't only create device drivers,
* but also add original features into the OS core.

# Get less quality on OSS product #1
![background](img/netbsd.png)

![inline](draw/2013-01-18-fork1.png)

# Get less quality on OSS product #2
![background](img/netbsd.png)

![inline](draw/2013-01-18-fork2.png)

# Get less quality on OSS product #3
![background](img/netbsd.png)

![inline](draw/2013-01-18-fork3.png)

# Get less quality on OSS product #4
![background](img/netbsd.png)

![inline](draw/2013-01-18-fork4.png)

# Get less quality on OSS product #5
![background](img/netbsd.png)

![inline](draw/2013-01-18-fork5.png)

# What's the root of such issues?
![background](img/root.png)

* Such issues are runtime errors, which are caused without sharing the specification.
* Specification =~ {Meaning, Invariant}
* If well specification is shared in people, we can easily understand new code and changing.
* Such specification is located at human's brain, and volatilized after writing code.

# Iceberg of runtime errors
![background](img/iceberg.png)

![inline](draw/iceberg_of_errors.png)

# How about watch more iceberg?
![background](img/iceberg.png)

![inline](draw/iceberg_more.png)

# Microsoft is getting less runtime errors
![background](img/bill_gates.png)

* Static Driver Verifier

```
https://docs.microsoft.com/en-us/windows-hardware/drivers/devtest/static-driver-verifier
"Static Driver Verifier is a static verification tool that systematically analyzes the source code of Windows kernel-mode drivers."
```

* VCC: A Verifier for Concurrent C

```
https://github.com/Microsoft/vcc
"The current primary goal of the VCC project is to verify Microsoft Hyper-V."
```

* Why is there no solution for OSS product?

# Metasepi Project
![background](img/metasepi.png)

* http://metasepi.org/
* Idea: Strong types can capture specification, which avoids some of runtime errors.
* Goal: Create Unix-like OS with strong type.
* Challenge: Find such type systems on science and engineering, and write practical code.

# 1st iteration: Ajhc Haskell compiler
![background](img/ajhc.png)

* John Meacham creates own Haskell compiler, named "jhc".
* http://repetae.net/computer/jhc/
* I forked jhc to be customized for embedded system.
* The compiler is named "Ajhc".
* http://ajhc.metasepi.org/

# How does Ajhc work?
![background](img/ajhc.png)

![inline](draw/2012-12-22-jhc_compile.png)

# 1st iteration: Plan for kernel
![background](img/ajhc.png)

![inline](draw/arafura_design.png)

# Ajhc for multitask and interrupt
![background](img/ajhc.png)

```
"Systems Demonstration: Writing NetBSD Sound Drivers in Haskell."
http://metasepi.org/doc/metasepi-icfp2014-demo.pdf
```

![inline](draw/arena_lifecycle.png)

# App: Haskell code runs on ARM MCU
![background](img/ajhc.png)

* http://youtu.be/C9JsJXWyajQ

![inline](img/rss_reader_haskell.png)

# App: Haskell code runs on Android NDK
![background](img/ajhc.png)

* http://youtu.be/n6cepTfnFoo

![inline](img/android_ndk.png)

# App: Audio driver written by Haskell
![background](img/ajhc.png)

* http://youtu.be/E30ZvEVExI0

![inline](img/netbsd_haskell.png)

# 1st iteration: Keep/Problem/Try
![background](img/ajhc.png)

Keep:

* Languages other than C can effectively write kernel code.

Problem:

* Haskell thunk easily eats huge memory.

Try:

* Find system language without GC.

# 2nd iteration: ATS language
![background](img/jats-ug.png)

* http://www.ats-lang.org/
* ATS is developed at Boston University.
* ATS can prove the code with dependent types.
* ATS code can run without GC.
* I found "Japan ATS User Group" to translate documents into Japanese.
* http://jats-ug.metasepi.org/

# How does ATS work?
![background](img/jats-ug.png)

![inline](draw/ats_flow.png)

# 2nd iteration: Plan for kernel
![background](img/jats-ug.png)

![inline](draw/bohai_design.png)

# App: ATS code runs on 8bit AVR
![background](img/jats-ug.png)

* http://youtu.be/5uPue0Jo1nc

![inline](img/ats_run_on_avr.png)

# App: Capture system state on RTOS
![background](img/jats-ug.png)

* https://github.com/fpiot/chibios-ats-2

![inline](img/system_states1.png)

# Tool: Generate ATS interface from C
![background](img/jats-ug.png)

* https://github.com/metasepi/c2ats

```c
/* C language */
FILE *fopen(const char *pathname, const char *mode);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
```

```
↓
```

```ats
(* ATS language *)
fun fun_c2ats_fopen: {l1,l2:addr} (!ptr_v_1(char, l1), !ptr_v_1(char, l2) | ptr l1, ptr l2) -> [l3:addr] (ptr_v_1(type_c2ats_FILE, l3) | ptr l3) = "mac#fopen"
fun fun_c2ats_fread: {l1:addr} (!ptr_v_1(type_c2ats_FILE, l1) | ptr, type_c2ats_size_t, type_c2ats_size_t, ptr l1) -> type_c2ats_size_t = "mac#fread"
```

# 2nd iteration: Keep/Problem/Try
![background](img/jats-ug.png)

Keep:

* Linear types safely uses pointer without GC.

Problem:

* Rewriting existing C code with ATS needs much human power.

Try:

* Apply specification to code without rewriting.

# 3rd iteration: VeriFast verifier
![background](img/ESAT_KULeuven.png)

* https://github.com/verifast/verifast
* VeriFast is developed at KU Leuven.
* VeriFast verifies pre/postcondition in comment.
* VeriFast can verify C and Java language.
* I translated VeriFast tutorial into Japanese.

```
https://github.com/jverifast-ug/translate/blob/master/Manual/Tutorial/Tutorial.md
```

# How does VeriFast work?
![background](img/ESAT_KULeuven.png)

xxx Figure

# 3rd iteration: Plan for kernel
![background](img/ESAT_KULeuven.png)

![inline](draw/chiers_design.png)

# App: Capture system state on RTOS
![background](img/ESAT_KULeuven.png)

* https://github.com/fpiot/chibios-verifast

![inline](img/slide_chibios_state.png)

# App: Capture system state on RTOS
![background](img/ESAT_KULeuven.png)

![inline](draw/platform.png)

# 3rd iteration: Keep/Problem/Try
![background](img/ESAT_KULeuven.png)

Keep:

* VeriFast exhaustively captures invariant in C code.

Problem:

* VeriFast only support subset of C language.

Try:

* Get C99 compat C language parser on VeriFast.

# Today's choice: Type or Verification?
![background](img/fork.png)

* ATS: Type system in new language can capture invariant in the code.
* VeriFast: Verification in comment of existing language can apply specification onto the code.
* Which is better for writing OS?
* "Faced with a choice, do both" -- Dieter Rot

# License of photos #1
![background](img/creative_commons.png)

```
* Creative Commons BBB | Flickr
  https://www.flickr.com/photos/steren/2732488224/
  Copyright: Steren Giannini / License: CC BY 2.0
* @ Ricoh, 2009/02/19 | In the lobby, the latest MFP, a multi … | Flickr
  https://www.flickr.com/photos/raitank/3292620200/
  Copyright: raitank / License: CC BY 2.0
* Rooted | Trees growing out of things FTW! … | Jody McIntyre | Flickr
  https://www.flickr.com/photos/scjody/4705083069/
  Copyright: Jody McIntyre / License: CC BY-SA 2.0
* Iceberg! | Christopher Michel | Flickr
  https://www.flickr.com/photos/cmichel67/8371340296/
  Copyright: Christopher Michel / License: CC BY 2.0
* Bill Gates @ the University of Waterloo | Bill Gates enthral… | Flickr
  https://www.flickr.com/photos/batmoo/61938659/
  Copyright: Mohammad Jangda / License: CC BY-SA 2.0
```

# License of photos #2
![background](img/creative_commons.png)

```
* Hooded Cuttlefish | Indonesia July 2007 | Silke Baron | Flickr
  https://www.flickr.com/photos/silkebaron/931247866/
  Copyright: Silke Baron / License: CC BY 2.0
* Hooded Cuttlefish | Indonesia July 2007 | Silke Baron | Flickr
  https://www.flickr.com/photos/silkebaron/931381358/
  Copyright: Silke Baron / License: CC BY 2.0
* Javi Recio y David Cabrera | Otakumunidad Damned | Flickr
  https://www.flickr.com/photos/otakumunidad/5787704531/
  Copyright: Otakumunidad Damned / License: CC BY 2.0
* ESAT KULeuven | Pues aquí es donde trabajo durante estos mes… | Flickr
  https://www.flickr.com/photos/juanvvc/4688054880/
  Copyright: Juan V. Vera del Campo / License: CC BY-SA 2.0
* The fork in the road | 例の分かれ道 | i_yudai | Flickr
  https://www.flickr.com/photos/y_i/2330044065/
  Copyright: i_yudai / License: CC BY-SA 2.0
```

# License of photos #3
![background](img/creative_commons.png)

```
* goal | olle svensson | Flickr
  https://www.flickr.com/photos/ollesvensson/4252196844/
  Copyright: olle svensson / License: CC BY 2.0
* Compass | A large old compass. Photo may be use as free stoc… | Flickr
  https://www.flickr.com/photos/waltstoneburner/6170496511/
  Copyright: Walt Stoneburner / License: CC BY 2.0
```
