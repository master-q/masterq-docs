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

# Demo code: LED fadein

```
#define LED 9
#define DELAY_MS 10.0
typedef analog_w_t = natLt(256)

fun{} int_foreach_clo{n:nat}
(n: int(n), fwork: &natLt(n) -<clo1> void): void =
  loop(0, fwork) where {
  fun loop{i:nat | i <= n} .<n-i>.
    (i: int(i), fwork: &natLt(n) -<clo1> void):void =
    if i < n then (fwork(i); loop (i+1, fwork))
}

implement main () = {
  fun fadein() = let
    var fwork = lam@ (n: analog_w_t) =>
      (analogWrite (LED, n); delay_ms(DELAY_MS))
  in
    int_foreach_clo(256, fwork)
  end // end of [fadein]
  (* val () = init () *)
  val () = pinMode (LED, OUTPUT)
  val () = (fix f(): void => (fadein(); f()))()
}
```

# Demo code: LCD greeting

```
#define MY_DELAY_MS 400.0
#define LCD_WIDTH 16

val g_str_atsrun = "<ATS running!>"
val g_str_message = "...Greeting message..."

implement main () = {
  fun loop {n:int}{i:nat | i < n}
           (lcd: !lcd_t, str: string (n), pos: size_t (i)): void = {
    val () = if pos + i2sz LCD_WIDTH <= length str then {
      val () = (lcd_setCursor (lcd, 1, 0); lcd_print (lcd, g_str_atsrun, i2sz 0, length g_str_atsrun))
      val () = (lcd_setCursor (lcd, 0, 1); lcd_print (lcd, str, pos, i2sz LCD_WIDTH))
      val () = delay_ms (MY_DELAY_MS)
      val () = loop (lcd, str, pos + 1)
    }
  }
  fun forever {n:int}{i:nat | i < n}
              (lcd: !lcd_t, str: string (n), pos: size_t (i)): void = {
    val () = loop (lcd, str, pos)
    val () = forever (lcd, str, pos)
  }
  val lcd = lcd_open (8, 13, 9, 4, 5, 6, 7)
  val () = forever (lcd, g_str_message, i2sz 0)
  val () = lcd_close lcd
}
```

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
