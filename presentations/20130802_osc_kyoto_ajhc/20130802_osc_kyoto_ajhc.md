# 組込向けHaskellコンパイラAjhc　OSC 2013 Kansai@Kyoto版

Kiwamu Okabe

# 私は誰？
![background](img/enjoy.png)

* Twitter: @master_q
* Metasepiプロジェクト主催
* Ajhc Haskellコンパイラ開発者
* Debian Maintainer
* 前はデジタルサイネージの開発してました
* その昔はコピー機のOSをNetBSDで

# おしながき

* [1] Ajhcコンパイラとは
* [2] Metasepi kernelとは
* [3] OS開発向けコンパイラとは？
* [4] AjhcコンパイラでPOSIXの外へ
* [5] Ajhcコンパイラの今後

# [1] Ajhcコンパイラとは
![background](img/ajhc.png)

http://ajhc.metasepi.org/

* Ajhc := A fork of jhc
* jhc := John's Haskell Compiler
* http://repetae.net/computer/jhc/
* jhcはフットプリントが小さく、
* 高速な実行バイナリを吐くらしい
* 組み込みに向いてるかも

# どうしてAjhcコンパイラ作るの？

* HaskellコミュニティではGHCがデファクト
* GHC := Glasgow Haskell Compiler
* http://www.haskell.org/ghc/
* なぜ別のHaskellコンパイラが必要？
* Metasepiというkernelを作るため

# [2] Metasepi kernelとは

http://metasepi.org/

* UNIXモドキkernelを強い型によって設計
* ML同等以上に強い型を持つ言語を使う

Haskell http://www.haskell.org/

OCaml http://caml.inria.fr/

MLton http://mlton.org/

などなど

# どうしてMetasepiが必要？

* LinuxやWindowsが既にあるのでは？
* しかし組込開発は苦しんでいる
* kernelをカスタマイズして使用
* kernelのランタイムエラー
* 再現することでさえ困難

# 既存OSSの品質は高いのでは？

* OSSは品質が高いと言われている
* 伽藍とバザール
* 「目玉の数さえ十分あれば、どんなバグも深刻ではない」

~~~
http://cruel.org/freeware/cathedral.html
~~~

* ところが社内開発は...

# 主開発からそれると品質急降下

![inline](draw/oss_quality.png)

# 型安全とは

* ランタイムエラーを少なくできる
* 参考:数理科学的バグ撲滅方法論のすすめ

~~~
http://itpro.nikkeibp.co.jp/article/COLUMN/20060915/248230/
~~~

![inline](draw/2013-01-18-few_error.png)

# kernelにこそ型安全が必要

* ほとんどのkernelはC言語で設計される
* ユーザー空間でエラー → SEGV
* kernel空間でエラー → システム停止
* kernelの設計には細心の注意が必要
* C言語は安全なのか？

# [3] OS開発向けコンパイラとは？

* 強い型が使えてOSを作れるコンパイラは？
* ないみたいなので作りましょう!
* スクラッチから作るのはツライ
* 要求に近いコンパイラはないかな？

# POSIX外で使いやすいコンパイラ

![inline](img/compiler_list.png)

各数値が小さいほどPOSIX APIへの依存度が小さい

# jhcバイナリは未定義シンボル20個

~~~
$ nm hs.out | grep "U "
                 U _IO_putc@@GLIBC_2.2.5
                 U __libc_start_main@@GLIBC_2.2.5
                 U _setjmp@@GLIBC_2.2.5
                 U abort@@GLIBC_2.2.5
                 U ctime@@GLIBC_2.2.5
                 U exit@@GLIBC_2.2.5
                 U fflush@@GLIBC_2.2.5
                 U fprintf@@GLIBC_2.2.5
                 U fputc@@GLIBC_2.2.5
                 U fputs@@GLIBC_2.2.5
                 U free@@GLIBC_2.2.5
                 U fwrite@@GLIBC_2.2.5
                 U getenv@@GLIBC_2.2.5
                 U malloc@@GLIBC_2.2.5
                 U memset@@GLIBC_2.2.5
                 U posix_memalign@@GLIBC_2.2.5
                 U realloc@@GLIBC_2.2.5
                 U setlocale@@GLIBC_2.2.5
                 U sysconf@@GLIBC_2.2.5
                 U times@@GLIBC_2.2.5
~~~

# jhcはC言語への変換器

![inline](draw/ajhc_compile.png)

# クロスコンパイルも簡単

![inline](draw/cross_compile.png)

# 実用化に辿りつくために

* 実行可能状態を維持しながら開発

![inline](draw/2012-12-27-arafura_design.png)

# [4] AjhcコンパイラでPOSIXの外へ

* インストールしてみましょう

Ubuntu 12.04 amd64 の場合

~~~
$ sudo apt-get install haskell-platform libncurses5-dev libwww-perl gcc m4
$ cabal update
$ cabal install ajhc
$ export PATH=$HOME/.cabal/bin/:$PATH
$ which ajhc
/home/ユーザ名/.cabal/bin/ajhc
$ ajhc --version
ajhc 0.8.0.8 (f6c3f4b070acad8a5012682810f0f4d7b7b9ed44)
compiled by ghc-7.4 on a x86_64 running linux
~~~

あっさりですね!

# 簡単なプログラムを作りましょう

~~~
$ vi MyDiff.hs
~~~

~~~ {.haskell}
import System.Environment
import Data.Algorithm.Diff

main :: IO ()
main = do [s1, s2] <- getArgs
          c1 <- readFile s1
          c2 <- readFile s2
          print . filter f $ getDiff c1 c2
  where
    f (Both _ _) = False
    f _ = True
~~~

~~~
$ ajhc -p Diff -o mydiff MyDiff.hs
$ echo "hoge"  > s1.txt
$ echo "hofe0" > s2.txt
$ ./mydiff s1.txt s2.txt
[Second 'f',First 'g',Second '0']
~~~

# POSIX依存をなくそう

~~~
$ vi Small.hs
~~~

~~~ {.haskell}
main :: IO ()
main = return ()
~~~

~~~
$ ajhc -o small Small.hs
$ ./small
~~~

* これは「何もしないプログラム」です
* POSIX依存を少なくしてみましょう
* ここでの依存とは未定義シンボルの個数

~~~
$ nm small | grep -c "U "
19
~~~

# GCCコンパイルオプションを調べる

~~~
$ ajhc --tdir rtsdir Small.hs
$ ls
Small.hs  hs.out*  rtsdir/
$ head -1 rtsdir/main_code.c
char jhc_c_compile[] = "gcc rtsdir/rts/profile.c rtsdir/rts/rts_support.c rtsdir/rts/gc_none.c rtsdir/rts/jhc_rts.c rtsdir/lib/lib_cbits.c rtsdir/rts/gc_jgc.c rtsdir/rts/stableptr.c rtsdir/rts/conc.c -Irtsdir/cbits -Irtsdir rtsdir/main_code.c -o hs.out '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE'";
~~~

jhc_c_compile文字列からGCCのコンパイルオプションがわかる

# Makefileを使ってコンパイル

~~~
$ vi Makefile
~~~

~~~ {.makefile}
small: rtsdir/main_code.c
	gcc rtsdir/rts/profile.c rtsdir/rts/rts_support.c rtsdir/rts/gc_none.c rtsdir/rts/jhc_rts.c rtsdir/lib/lib_cbits.c rtsdir/rts/gc_jgc.c rtsdir/rts/stableptr.c rtsdir/rts/conc.c -Irtsdir/cbits -Irtsdir rtsdir/main_code.c -o hs.out '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE' -o small

rtsdir/main_code.c: Small.hs
	ajhc --tdir rtsdir -C Small.hs

clean:
	rm -rf rtsdir small *~
~~~

~~~
$ make
$ nm small | grep -c "U "
19
~~~

# ランタイムのソースを限定

~~~
$ vi Makefile
~~~

~~~ {.makefile}
small: rtsdir/main_code.c dummy.c
	gcc rtsdir/rts/rts_support.c rtsdir/rts/jhc_rts.c rtsdir/rts/gc_jgc.c rtsdir/rts/stableptr.c rtsdir/rts/conc.c dummy.c -Irtsdir/cbits -Irtsdir rtsdir/main_code.c -o hs.out '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE' -o small
# --snip--
~~~

~~~
$ vi dummy.c
~~~

~~~ {.c}
#include "jhc_rts_header.h"

void jhc_print_profile(void) {}
~~~

~~~
$ make
$ nm small | grep -c "U "
15
~~~

# ダミー関数をさらに投入

~~~
$ vi dummy.c
~~~

~~~ {.c}
#include "jhc_rts_header.h"

void abort() {for (;;);}
char *setlocale(int category, const char *locale) {return NULL;}
int fputc(int c, FILE *stream) {return 0;}
int fputs(const char *s, FILE *stream) {return 0;}
int fprintf(FILE *stream, const char *format, ...) {return 0;}
int fflush(FILE* stream) {return 0;}
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream) {return 0;}
void jhc_print_profile(void) {}
~~~

~~~
$ make
$ nm small | grep -c "U "
8
~~~

# 例外を無視する

~~~
$ vi Makefile
small: rtsdir/main_code.c dummy.c
	gcc rtsdir/rts/rts_support.c rtsdir/rts/jhc_rts.c rtsdir/rts/gc_jgc.c rtsdir/rts/stableptr.c rtsdir/rts/conc.c dummy.c main.c -Irtsdir/cbits -Irtsdir rtsdir/main_code.c -o hs.out '-std=gnu99' -D_GNU_SOURCE '-falign-functions=4' -ffast-math -Wextra -Wall -Wno-unused-parameter -fno-strict-aliasing -DNDEBUG -O3 '-D_JHC_GC=_JHC_GC_JGC' '-D_JHC_CONC=_JHC_CONC_NONE' '-D_JHC_STANDALONE=0' -o small
~~~

~~~
$ vi main.c
~~~

~~~ {.c}
#include "jhc_rts_header.h"

int
main(int argc, char *argv[])
{
        hs_init(&argc,&argv);
	_amain();
        hs_exit();
        return 0;
}
~~~

# 残ったPOSIX依存は何？

~~~
$ nm small | grep -c "U "
7
$ nm small | grep "U "   
                 U __libc_start_main@@GLIBC_2.2.5
                 U exit@@GLIBC_2.2.5
                 U free@@GLIBC_2.2.5
                 U malloc@@GLIBC_2.2.5
                 U memset@@GLIBC_2.2.5
                 U posix_memalign@@GLIBC_2.2.5
                 U realloc@@GLIBC_2.2.5
~~~

* main関数呼び出しとexitはまぁ...
* mallocなどのメモリ管理だけが実行に必要

# これでプログラミングできるの？

以下2つのしくみを使ってC言語側を触れます

* Foreign Function Interface (FFI)
* Foreign.Storableクラス

# FFIでお互いの関数を呼びあえる

![inline](draw/ffi.png)

# Storableでメモリ直読み書き

![inline](draw/storable.png)

# [5] Ajhcコンパイラの今後

# PR: λカ娘に記事を書きませんか？

http://www.paraiso-lang.org/ikmsm/

# 本スライドで使用した画像
