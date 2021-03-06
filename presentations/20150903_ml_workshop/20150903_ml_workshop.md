# Arduino programming of ML-style in ATS
![background](img/led.png)

Kiwamu Okabe @ METASEPI DESIGN / Hongwei Xi @ Boston University

# Demo: Video
![background](img/demo_icfp2014.png)

* LCD greeting
* ⇒ http://youtu.be/5uPue0Jo1nc
* LED fadein
* ⇒ http://youtu.be/_Sx6GzuTm9k

# Demo: Software Architecture
![background](img/minecraft.png)

![inline](draw/demo_arch.png)

# Arduino Uno hardware
![background](img/arduino_uno.png)

It's poor.

* 8-bit Harvard architecture
* Flash Memory: 32 KB
* SRAM: 2 KB

Many people use C language on the hardware.

# Problem of C language
![background](img/Dennis_Ritchie.png)

* Memory/Resource leak
* Out of bounds
* Weak type

We need functional language for embedded system!

# Approach 1: Virtual machine
![background](img/climbing.png)

![inline](draw/vm.png)

# Approach 2: DSL
![background](img/climbing.png)

![inline](draw/dsl.png)

# Approach 3: Direct language
![background](img/climbing.png)

![inline](draw/direct.png)

# Comparison of the approaches
![background](img/climbing.png)

We choose the 3rd approach.

![inline](draw/comparison.png)

# ATS language
![background](img/ats_logo_on_display.png)

* http://www.ats-lang.org/
* DML-style dependent types
* Linear types
* Optional GC
* Optional malloc/free
* Optional run-time

# Author: Hongwei Xi
![background](img/with_hongwei.png)

![inline](img/with_hongwei.png)

# ATS programming level
![background](img/steps.png)

![inline](draw/ats_level.png)

Today, we focus on the level "c", because Arduino hardware is poor.

# Functional style programming
![background](img/recycle.png)

* Functional style programming can be used with ATS on embedded system?
* Yes.
* But only use some of functional style technique in ATS without GC and malloc/free.
* Let's see some examples.

# Style 1. Envless function
![background](img/memopad.png)

* Environment-less function.
* Not closure.
* C language function is also envless function.

```
%{^ // C language code
int cfunc(int a, int b) {
        return (a + b);
}
%}
// ATS language code
extern fun cfunc (a: int, b: int): int = "mac#"

implement main0 () = println! (cfunc (1, 2)) // => 3
```

# Style 2. Stack allocated closure
![background](img/memopad.png)

Stack allocated closure is allocated on stack.
It can use free variable.

```
fun run (f: &int -<clo1> int): int = f 1

implement main0 () = {
  val b = 2
  var plus = lam@ (a: int):int => a + b
  val () = println! (run plus) // => 3
}
```

# Style 3. Template function
![background](img/memopad.png)

The template is functorial style that has lexical scoping.

```
extern fun{} base (): int

fun{} plus (a: int): int = a + base ()

implement main0 () = {
  implement{} base () = 2
  val () = println! (plus 1) // => 3
}
```

# Safety shaped by ATS
![background](img/simulator.png)

* ATS is a better C language.
* "Better" is meaning "Safer".
* Let's see some examples.

# Safety 1. Termination metrics
![background](img/memopad.png)

".<255 - n>." is termination metric that grows smaller on each recursive call, for termination-checking.

```
fun loop_fadein {n:nat | n <= 255} .<255 - n>. (i: int n): void = {
  val () = analogWrite (LED, i)
  val () = delay_ms (BLINK_DELAY_MS)
  val () = if i < 255 then loop_fadein (i + 1)
}
...
val () = loop_fadein 0
```

# Safety 2. DML dependent types
![background](img/memopad.png)

"size_t (i)" is a type that depends on static value "i".
"i" has constraint "i < n".
If the constraint is not solved, it causes compile error.

```
fun lcd_print {n:int}{i:nat | i < n}{j:nat | i + j <= n}
      (lcd: !lcd_t, str: string (n), start: size_t (i),
       len: size_t (j)): void
```

![inline](draw/lcd_sats_type.png)

# Safety 3. View
![background](img/memopad.png)

Linear proofs should be produced and consumed.

If a produced linear proof is not consumed, it causes type error at compile time.

```
// lcd.sats - Library interface
absvtype lcd_t = ptr
fun lcd_open (rs: int, rw: int, enable: int, d0: int, d1: int,
              d2: int, d3: int): lcd_t // Produce linear value
fun lcd_close (lcd: lcd_t): void // Consume linear value
// main.dats - Application code
implement main () = {
  val lcd = lcd_open (8, 13, 9, 4, 5, 6, 7)
  // ...Do something...
  val () = lcd_close lcd // <= If not, compile error occurs.
}
```

# Safety 4. At-view
![background](img/memopad.png)

```
dataview array_v (a:t@ype+, addr, int) =
  | {l:addr} array_v_nil (a, l, 0)
  | {l:addr}{n:nat} array_v_cons (a, l, n+1) of
                    (a @ l, array_v (a, l+sizeof(a), n))
```

![inline](draw/at-view.png)

# Demo code: LED fadein
![background](img/memopad.png)

```
#define LED 9
#define DELAY_MS 10.0
typedef analog_w_t = natLt(256)

fun{} int_foreach_clo{n:nat}
(n: int(n), fwork: &natLt(n) -<clo1> void): void =
  loop(0, fwork) where {
  fun loop{i:nat | i <= n} .<n-i>.
    (i: int(i), fwork: &natLt(n) -<clo1> void): void =
    if i < n then (fwork(i); loop (i+1, fwork))
}

implement main () = {
  fun fadein() = let
    var fwork = lam@ (n: analog_w_t) =>
      (analogWrite (LED, n); delay_ms(DELAY_MS))
  in
    int_foreach_clo(256, fwork)
  end // end of [fadein]
  val () = pinMode (LED, OUTPUT)
  val () = (fix f(): void => (fadein(); f()))()
}
```

# Demo code: LCD greeting (cont.)
![background](img/memopad.png)

```
#define MY_DELAY_MS 400.0
#define LCD_WIDTH 16

val g_str_atsrun = "<ATS running!>"
val g_str_message = "...Greeting message..."

implement main () = {
  fun loop {n:int}{i:nat | i < n} .<n-i>.
         (lcd: !lcd_t, str: string (n), pos: size_t (i)): void = {
    val () = if pos + i2sz LCD_WIDTH <= length str then {
      val () = lcd_setCursor (lcd, 1, 0)
      val () = lcd_print (lcd, g_str_atsrun, i2sz 0,
                          length g_str_atsrun)
      val () = lcd_setCursor (lcd, 0, 1)
      val () = lcd_print (lcd, str, pos, i2sz LCD_WIDTH)
      val () = delay_ms (MY_DELAY_MS)
      val () = loop (lcd, str, pos + 1)
    }
  }
```

# Demo code: LCD greeting
![background](img/memopad.png)

```
  fun forever {n:int}{i:nat | i < n}
              (lcd: !lcd_t, str: string (n), pos: size_t (i)):
              void = {
    val () = loop (lcd, str, pos)
    val () = forever (lcd, str, pos)
  }
  val lcd = lcd_open (8, 13, 9, 4, 5, 6, 7)
  val () = forever (lcd, g_str_message, i2sz 0)
  val () = lcd_close lcd
}
```

# Binary size efficiency
![background](img/bird_flock.png)

For examples on "Getting Started with Arduino", ATS has good binary size efficiency as well as C.

![inline](img/space_efficiency.png)

# Conclusion
![background](img/arduino_unboxing.png)

* Can we directly use functional style programming on embedded system?
* ⇒ Yes, with ATS language.
* Can we make more safety than C language on embedded system?
* ⇒ Yes, with ATS language.

https://github.com/fpiot/arduino-ats

# License of photos #1
![background](img/creative_commons.png)

```
* Minecraft toys | Flickr - Photo Sharing!
  https://www.flickr.com/photos/sergesegal/15976451410/
  Copyright: 2015 Sergey Galyonkin / License: CC BY-SA 2.0
* Arduino Uno | Flickr - Photo Sharing!
  https://www.flickr.com/photos/snootlab/6052455554/
  Copyright: Snootlab / License: CC BY 2.0
* Thank You Dennis Ritchie | Flickr - Photo Sharing!
  https://www.flickr.com/photos/vincentpants/6239875256/
  Copyright: Vincent van Haaff / License: CC BY-SA 2.0
* Climbing Journal Mount Rinjani package | Flickr - Photo Sharing!
  https://www.flickr.com/photos/trekkingrinjani/4930552641/
  Copyright: Trekking Rinjani / License: CC BY 2.0
* Recycling Grunge Sign | Flickr - Photo Sharing!
  https://www.flickr.com/photos/80497449@N04/8677649972/
  Copyright: Nicolas Raymond / License: CC BY 2.0
* IMG_4097 | Flickr - Photo Sharing!
  https://www.flickr.com/photos/matthewpiatt/1562708158/
  Copyright: Matthew Piatt / License: CC BY-SA 2.0
* Seagulls in Flight | Flickr - Photo Sharing!
  https://www.flickr.com/photos/87007001@N04/13513835453/
  Copyright: Shaun Fisher / License: CC BY 2.0
```

# License of photos #2
![background](img/creative_commons.png)

```
* Arduino Uno unboxing | Flickr - Photo Sharing!
  https://www.flickr.com/photos/mightyohm/5052594028/
  Copyright: Jeff Keyzer / License: CC BY-SA 2.0
* Creative Commons BBB | Flickr - Photo Sharing!
  https://www.flickr.com/photos/steren/2732488224/
  Copyright: Steren Giannini / License: CC BY 2.0
* LED | Flickr - Photo Sharing!
  https://www.flickr.com/photos/nao904/6084536885/
  Copyright: Nao. Fujita / License: CC BY 2.0
* Pagoda's curly steps | Flickr - Photo Sharing!
  https://www.flickr.com/photos/kewl/6834141860/
  Copyright: Tristan Schmurr / License: CC BY 2.0
```
