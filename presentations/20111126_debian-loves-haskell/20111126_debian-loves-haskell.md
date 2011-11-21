# Debian loves Haskell
![background](debian.png)

Kiwamu Okabe

# Who am I?
![background](enjoy.png)

* twitter: @master_q
* web: http://www.masterq.net/
* I had met my wife with Debian.
* I am a Haskell NINJA.
* (NINJA:=No Income No Job or Asset)

# Do you like Haskell?
![background](haskell.png)

* Do you like Haskell? Of course!
* Do you like Hackages? Of course!
* Do you like Hoogle? Of course!
* Do you like Cabal? ............. no ...

You know, Cabal has many problems.....

# About Cabal
![background](cabal.png)

* Hackage := packaged Haskell code
* Cabal := installer of Hackage
* Cabal is like Perl's CPAN.
* Usage: "cabal install HACKAGE"
* Cabal will install depended Hackages.

# Using Cabal on Debian is easy.
![background](apt-get_moo.png)

~~~
$ sudo apt-get install cabal-install haskell-platform
$ cabal update
$ cabal install carettah
# ...cabal compiles the hackages...
$ ~/.cabal/bin/carettah
carettah version 0.0.4
~~~

Easy!

# In case: Ruby's gem
![background](rubygem.png)

If you use Ruby gem,

~~~
$ sudo gem update
$ sudo gem install earthquake
# ...After some days...
$ sudo gem update
~~~

Always, you use latest applications.

# In case: Haskell's Cabal #1
![background](yesod_logo.png)

~~~
$ cabal update # update Hackage database on local
$ cabal install yesod
# After some days, you will try to upgrade yesod.
$ cabal upgrade
--snip--
The 'cabal upgrade' command has been removed
because people found it confusing and it often
led to broken packages.
--snip--
~~~

...What you say?????

# In case: Haskell's Cabal #2
![background](accident.png)

...OK, I try to upgrade Hackages needed.

~~~
$ cabal install yesod
# Yesod runs with BUG, or Cabal doesn't solve dependency.
# ...OK. Let's try install yesod from scratch.
$ rm -rf ~/.ghc ~/.cabal
$ cabal update
$ cabal install yesod
~~~

Yesod upgraded has problems.

Yesod installed from scratch is no BUG.

Why?????

# Why Cabal support no upgrade?
![background](problem.png)

There are two types of problems.

* Problem of Cabal mechanism
* Problem of Hackage author's culture

# Problem of Hackage culture
![background](culture.png)

~~~
$ cabal info yesod
--snip--
    Versions available: 0.6.7, 0.7.2, 0.7.3, 0.8.0, 0.8.1, 0.8.2,
                        0.8.2.1, 0.9.1, 0.9.1.1 (and 35 others)
--snip--
    Dependencies:  yesod-core >=0.9.1.1 && <0.10,
                   yesod-auth ==0.7.*, yesod-json ==0.2.*,
                   yesod-persistent ==0.2.*, yesod-form ==0.3.*,
                   monad-control ==0.2.*, ...
~~~

It defines upper limit of Hackage.

I think who never know future...

# Hackage versioning policy

~~~
"http://www.haskell.org/haskellwiki/\
Package_versioning_policy"
~~~

Above URL explain versioning poliy.

* Example: 2.1.0.4 (A=2, B=1, C=0)
* A.B is major version number.
* C is minor version number.
* Change major version if change API.

# Problem of implementation #1

![background](gear.png)

![inline](cabal-1.png)

# Problem of implementation #2

![background](gear.png)

![inline](cabal-2.png)

# Problem of implementation #3

![background](gear.png)

![inline](cabal-3.png)

# Problem of implementation #4

![background](gear.png)

![inline](cabal-4.png)

# Problem of implementation #5

![background](gear.png)

![inline](cabal-5.png)

# Problem of implementation #6

![background](gear.png)

![inline](cabal-6.png)

# Cabal know only Haskell world.

![background](world.png)

Example: "hcwiid" Hackage

* The hcwiid depends on libcwiid-dev.
* If you run "cabal install hcwiid",
* not install libcwiid-dev automatically.
* Should we use auto-apt?

# Use latest Hackages sametime.

![background](new.png)

Example: yesod, hakyll, hamlet

* yesod-0.9.2 depends on hamlet-0.10.*
* hakyll-3.2.0.8 depends hamlet-0.{7,8}.*
* can't use yesod and hakyll sametime?
* hakyll is orphaned? No No No.

# hakyll used hamlet old API. #1

hakyll use Text.Hamlet.RT API.

hamlet 0.8.2.1 provided it.

![inline](hamlet_0.8.2.1.png)

# hakyll used hamlet old API. #2

But, Text.Hamlet.RT is removed,

on hamlet 0.9.0.

![inline](hamlet_0.9.0.png)

# A dream: @khibino's idea
![background](khibino.png)

![inline](cabal_khibino.png)

Upgrade is to search installable string.

# Let's Debianize!
![background](marie_antoinette.png)

* 最強のcabalができるまでどうすれば、、、
* cabalダメならdeb化しちゃえばイイじゃない
* Haskell以外のライブラリに紐づけ可だし

# Hackageのdeb化 #1

![background](bbq3.png)

Haskellパッケージ化環境整備

~~~
$ sudo apt-get install \
   haskell-debian-utils haskell-devscripts
~~~

debhelperななにかがインストールされる。

# Hackageのdeb化 #2

![background](bbq2.png)

cabal-debianでdebianディレクトリ作成

~~~
$ wget http://hackage.haskell.org/packages/archive/\
   hcwiid/0.0.1/hcwiid-0.0.1.tar.gz
$ tar xfz hcwiid-0.0.1.tar.gz
$ cd hcwiid-0.0.1/
$ cabal-debian --debianize --ghc \
   --maintainer="Kiwamu Okabe <kiwamu@debian.or.jp>"
$ ls debian
changelog compat control copyright rules
~~~

# Hackageのdeb化 #3

![background](bbq1.png)

~~~
$ debuild -rfakeroot -us -uc
$ ls ../*hcwiid*deb
../libghc-hcwiid-dev_0.0.1-1~hackage1_amd64.deb
../libghc-hcwiid-doc_0.0.1-1~hackage1_all.deb
../libghc-hcwiid-prof_0.0.1-1~hackage1_amd64.deb
~~~

* 通常使用するライブラリ
* Haddockで生成されたドキュメント
* プロファイラ対応ライブラリ

がでけた!

# どうしてこんなに簡単なの？
![background](helper.png)

debhelperの力です。

~~~
$ cat debian/rules
#!/usr/bin/make -f
include /usr/share/cdbs/1/rules/debhelper.mk
include /usr/share/cdbs/1/class/hlibrary.mk
$
~~~

ご覧の通りincludeしかないです。

# hlibrary.mk #build

![background](crane.png)

![inline](haskell-debian-utils_build.png)

cabalが普段やっていることと同じ

# hlibrary.mk #install

![background](install.png)

![inline](haskell-debian-utils_install.png)

# どーせなら本家にdebをアップロード

![background](temple.png)

* 複数台PCの環境同期めんどい
* そのうちubuntuも取り込むかもしれんし
* やっちまえ! やっちまえ!

# とりあえずDMになりましょう

![background](direct_mail.png)

* DDにならなくてもできることはアル
* 結構簡単になれる
* こちらからドゾ

http://wiki.debian.org/DebianMaintainer

~~~
後日談) JoachimからメールがあってDDやDMでなくても
pkg-haskellチームになれるそうです！
>> [Q2] Can the person as not DM (Debian Maintainer) join
>> pkg-haskell team? Or they should become DM, first?
> No need to be a DM, as there are DDs around that can do
> the sponsoring.
~~~

# Get alioth account.

![background](alioth.png)

* アカウントの作り方は日記に書いた
* 読んでちょ

http://d.masterq.net/?date=20100325

(あんま詳しくないかも。。。)

# Let's join pkg-haskell team.

![background](handshake.png)

ボスは

* Joachim Breitner
* E-mail: nomeata@debian.org

debian-haskell@lists.debian.org 常駐？

頼まなくても活動してると勝手に登録される

# さて作りますか

![background](mail.png)

http://wiki.debian.org/Haskell

をまずは熟読のこと。とりあえずITPメール。

~~~
Package: wnpp
Severity: wishlist
Owner: Kiwamu Okabe <kiwamu@debian.or.jp>

* Package name    : haskell-ansi-wl-pprint
 Version         : 0.6.3
 Upstream Author : Daan Leijen, Max Bolingbroke
<batterseapower@hotmail.com>
* URL             : http://github.com/batterseapower/ansi-wl-pprint
 Vcs-Browser     :
http://anonscm.debian.org/gitweb/?p=collab-maint/haskell-ansi-wl-pprint.git
* License         : BSD3
~~~

# cabal-debianコマンドでdeb化

![background](sukiyaki.png)

この時、debian/controlをpkg-haskell風に

~~~
$ vi debian/control
Maintainer: Debian Haskell Group \
<pkg-haskell-maintainers@lists.alioth.debian.org>
Uploaders: Kiwamu Okabe <kiwamu@debian.or.jp>
Vcs-Darcs: \
http://darcs.debian.org/pkg-haskell/haskell-ansi-wl-pprint
Vcs-Browser: http://darcs.debian.org/cgi-bin/\
darcsweb.cgi?r=pkg-haskell/haskell-ansi-wl-pprint
DM-Upload-Allowed: yes
~~~

DMでもdput可能にしておこう

# debian/changelogにも注意

![background](changelog.png)

リリースしていないバージョンには

とりあえずUNRELEASEDマークをつけとく

~~~
haskell-ansi-wl-pprint (0.6.3-2) UNRELEASED; urgency=low

  * repo is moved to darcs.
  * change Vcs-* lines on debian/control.

 -- Kiwamu Okabe <kiwamu@debian.or.jp>  Wed, 12 Oct 2011 22:45:11 +0900

haskell-ansi-wl-pprint (0.6.3-1) UNRELEASED; urgency=low

  * Debianization generated by cabal-debian

 -- Kiwamu Okabe <kiwamu@debian.or.jp>  Wed, 05 Oct 2011 11:14:50 +0900
~~~

# darcsリポジトリを作る

![background](darcs.png)

~~~
$ sudo apt-get install darcs
$ pwd
/home/kiwamu/deb/haskell-ansi-wl-pprint/debian
$ darcs init --darcs-2
$ darcs record -a -l -m "Initial Check-In"
Finished recording patch 'Initial Check-In'
$ darcs put kiwamu-guest@darcs.debian.org:/darcs\
  /pkg-haskell/haskell-ansi-wl-pprint
Finished applying...
Put successful.
~~~

debianディレクトリだけ管理って。。。

どんなGentooだよwwwww

# darcsフックを設定

![background](hook.png)

~~~
$ ssh kiwamu-guest@darcs.debian.org \
  /darcs/pkg-haskell/tools/add-hooks.sh \
  haskell-ansi-wl-pprint
~~~

これでコミットログが

~~~
pkg-haskell-commits@lists.alioth.debian.org
~~~

に流れるようになる。

# リリースを前提にしたお付き合い

![background](marriage_meeting.png)

リリース対象バージョンを決める。

debian/changelogの最新行をunstableに

~~~
$ dch # エディタが起動される
~~~

バージョンが一つ上げた。そしてdarcs push。

~~~
$ darcs record -a
$ darcs push
Sending mail to pkg-haskell-commits@lists.alioth.debian.org...
~~~

さっきのフックで通報されるはず。

# Package Entropy Trackerが検出!

![background](entropy.png)

~~~
http://pkg-haskell.alioth.debian.org/cgi-bin/pet.cgi
~~~

"Ready for upload"状態になる。

![inline](pit_cgi.png)

# sponsor uploadを誰かにお願い

![background](sponsor.png)

debian-haskell@lists.debian.org

にお願いメールすると、、、

、、、たぶんJoachimが反応する。

無事dputされてしまえば、次回からは

自分でdputできますね :)

# sponsorはこんなことしてるらしい

![background](question.png)

~~~
$ darcs get darcs.debian.org:/darcs/pkg-haskell/tools
$ tools/pkg-haskell-checkout haskell-ansi-wl-pprint
$ cd haskell-ansi-wl-pprint/
$ debuild -i -I
$ debrelease
$ debuild clean
$ cd debian/
$ darcs tag $(dpkg-parsechangelog -lchangelog |\
  grep-dctrl -n -s Version .)
$ darcs push -a
~~~

pkg-haskell-checkout失敗するような気が。。。

後darcsはhttp経由だととバグる sshでどぞ

# Then, let's say...
![background](omaeha_naniwo.png)

Can I remove your

* $HOME/.cabal
* $HOME/.ghc

directories?

# PR: My presentation tool
![background](turtle.png)

* http://carettah.masterq.net/
* made with Haskell
* A clone of http://rabbit-shockers.org/
* Now you see Carettah!
* You can "apt-get install carettah".
