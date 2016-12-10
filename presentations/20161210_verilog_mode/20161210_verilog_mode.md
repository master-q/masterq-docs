# Emacs verilog-mode is coming to Debian

Kiwamu Okabe

# Do you know IceStorm?
![background](img/swapforth.png)

* First, please watch following video:
* http://youtu.be/rdLgLCIDSk0
* It's a Forth machine on Lattice FPGA.
* And it's designed by an Open Source tool.
* The tool is called "IceStorm"!
* http://www.clifford.at/icestorm/

# Coming OSS wave to FPGA world

* Xilinx and Altera only support proprietary tool for own FPGA
* IceStorm is an Open Source tool for Lattice FPGA
* Please remember GCC that changed everything in software world.
* I hope IceStorm change FPGA world with the Open Source wave!

# Flow to design C application

* Everything is debianized.

xxx figure

# Flow to design FPGA application

* Emacs verilog-mode is not debianized.

xxx figure

# Let's debianize verilog-mode on Emacs!

```
Subject: ITP: verilog-mode -- emacs mode for Verilog code
Package: wnpp
Severity: wishlist
Owner: Kiwamu Okabe <kiwamu@debian.or.jp>

* Package name: verilog-mode
  Version: 20160910.debfc6d.vpo
  Upstream Author: Michael McNamara <mac@verilog.com>, Wilson Snyder
<wsnyder@wsnyder.org>
* URL: https://github.com/veripool/verilog-mode
  Vcs-Browser: https://anonscm.debian.org/cgit/collab-maint/verilog-mode.git
* License: GPL3
  Programming Lang: Emacs-lisp
  Description: emacs mode for Verilog code
 This package provides an emacs major mode for editing
 files in the Verilog code. It features syntax
 highlighting and auto-indentation.
```

# dh-elpa supports ELPA package

* ELPA (Emacs Lisp Package Archive)
* http://elpa.gnu.org/
* It's the default package repository for Emacs.
* But it only supported by Emacs
* and XEmacs has no feature such like ELPA.
* Debian pkg-emacsen team are converting all existing Emacs Lisp addon packages using dh-elpa.

# Where is the verilog-mode?

* http://www.veripool.org/projects/verilog-mode/wiki/Installing
* But it only publishes single el file...

![inline](img/only-elgz.png)

# Where is full source code?

* https://github.com/veripool/verilog-mode
* It has own test code for verilog-mode.el
* and info file.

# dh-elpa is easy to use

```
$ apt-get source elpa-vala-mode
$ cat elpa-rust-mode-0.3.0/debian/rules
```

```makefile
#!/usr/bin/make -f

%:
        dh "$@" --parallel --with elpa

override_dh_auto_clean:
        rm -f rust-mode.elc

override_dh_auto_test:
        ./run_rust_emacs_tests.sh
```

# Verilog-mode test fails on XEmacs

```
On Sat, Oct 15, 2016 at 7:44 PM, Wilson Snyder <wsnyder@wsnyder.org> wrote:
>>>On Sat, Oct 15, 2016 at 3:48 PM, Kiwamu Okabe <kiwamu@debian.or.jp> wrote:
>>>> I found that! If we run "make test" on fakeroot environment, we get
>>>> such error!
>>>
>>> I would like to fix this problem on Debian side by running "make test"
>>> on normal user.
>
> Makes sense.  I suspect the eval is turned off as
> root/fakeroot for security, however this verilog-mode test
> depends on that eval feature, so obviously the test will
> fail.
```

# Who is using XEmacs?

* ftp://ftp.xemacs.org/ is shutdown?

```
$ zcat /usr/share/doc/xemacs21/changelog.Debian.gz
--snip--
xemacs21 (21.4.24-1) unstable; urgency=low

  * New upstream release.
--snip--
 -- Mark Brown <broonie@debian.org>  Thu, 13 Aug 2015 13:14:35 +0100
```

# We should forget emacsen!

* Mule was dead.
* XEmacs will be dead.
* Emacs has a big community, today.
* How about only focus Emacs?

# Conclusion
# PR: Join SELTECH CORPORATION!

* http://seltech.co.jp/en/
* Developing a Hypervisor for embedded system
* We are hiring embedded software engineer!

![inline](img/foxvisor_contents01_illsut.png)
