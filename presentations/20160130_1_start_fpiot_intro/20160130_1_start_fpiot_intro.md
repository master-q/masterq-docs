# Functional IoT: Introduction

Kiwamu Okabe

# Who am I?
![background](img/enjoy.png)

* http://masterq.metasepi-design.com/
* Name: Kiwamu Okabe
* Software engineer at Centillion
* Part-time researcher at RIKEN AICS
* Self-employed at METASEPI DESIGN
* A Debian Maintainer

# What's IoT?

The Internet of Things (or IoT for short) refers to uniquely identifiable objects and their virtual representations in an Internet-like structure. Imagine IoT devices are:

connected to the internet / developed in a short time / storing personal data / secure / more intelligence / inexpensive.

C language can easily design the IoT devices?

# What's Functional IoT?

* http://fpiot.metasepi.org/
* is a wrestling mat that strongly typed languages fight on, to become the champion of system programming language.
* tries to write demo code running on tiny MCU using the languages.
* summarizes knowledge of the languages as catalog.

# Why do Functional IoT?

* Today, we have many methodologies for system design.
* For example, they are static typing, theorem proving, formal method, design by contract, model checking, static verification, SMT solver, etc.
* However, nobody have the exhaustive catalog of them.
* Let's make the catalog!

# Why need such methodologies?

Many people choose C/C++ languages to design IoT device.
However the languages lack following:

* Avoiding vulnerability
* Specification to be verified
* Keeping good quality
* Getting less man-hour
* Fun

# Avoiding vulnerability
![background](img/crash.png)

Vulnerability is caused by following errors:

* Array index out of range (Buffer overflow)
* Type range violation
* Division by zero
* Numerical overflow

Some methodologies can avoid these errors.

# Specification to be verified
![background](img/Physical_Inventory_of_Fuel_Assemblies.png)

* Many people maintain specification as natural language.
* Some methodologies can formally maintain specification which is verified in computer.

![inline](draw/spec_verified.png)

# Keeping good quality
![background](img/old_car.png)

* Original author can keep quality of the code, however the other is hard to do it by lack of verify-able specification. Sometimes, the original author will become "the other" in future.

![inline](draw/keeping_quality.png)

# Getting less man-hour
![background](img/Iceberg.png)

* Easy to estimate compile time error, however hard to do run time error, because finding latter needs some testing.

![inline](draw/less_mon-hour.png)

# Fun
![background](img/kouki_jump.png)

C language can't use following feature:

* Algebraic data type
* Higher-order function
* Namespace
* Pattern matching
* Type inference
* Garbage collection

# How to do Functional IoT?
![background](img/pdca.png)

* Survey existing the methodologies as you like.
* Write some demo code running on tiny MCU.
* Monthly have meetup at Tokyo, Japan.
* Share your knowledge and demo at the meetup.
* Update our exhaustive catalog of the methodologies.

# Let's join the Functional IoT meetup!
![background](img/beer.png)

https://fpiot.doorkeeper.jp/

![inline](img/fpiot_meetup.png)

# License of photos
![background](img/creative_commons.png)

```
* Volkswagen Beetle crashed through a brick wall | Flickr - Photo Sharing!
  https://www.flickr.com/photos/simpleinsomnia/23882140795/
  Copyright: simpleinsomnia / License: CC BY 2.0
* Physical Inventory of Fuel Assemblies (03210024) | Flickr - Photo Sharing!
  https://www.flickr.com/photos/iaea_imagebank/8366285569/
  Copyright: IAEA Imagebank / License: CC BY-SA 2.0
* Old Car Festival, 2015 | Flickr - Photo Sharing!
  https://www.flickr.com/photos/50697352@N00/21210898408/
  Copyright: F. D. Richards / License: CC BY-SA 2.0
* Iceberg | Flickr - Photo Sharing!
  https://www.flickr.com/photos/usoceangov/8290528771/
  Copyright: NOAA's National Ocean Service / License: CC BY 2.0
* PDCA-Do | Flickr - Photo Sharing!
  https://www.flickr.com/photos/jurgenappelo/6797304300/
  Copyright: Jurgen Appelo / License: CC BY 2.0
* Creative Commons BBB | Flickr - Photo Sharing!
  https://www.flickr.com/photos/steren/2732488224/
  Copyright: Steren Giannini / License: CC BY 2.0
```
