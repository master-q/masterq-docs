# さわって学ぶVCC

["Verifying C Programs: A VCC Tutorial"](https://swt.informatik.uni-freiburg.de/teaching/SS2015/swtvl/Resources/literature/vcc-tutorial-col2.pdf)で紹介されているコードを[オンライン](https://rise4fun.com/Vcc)で実行させながら[VCC](https://github.com/Microsoft/vcc)検証器の使い方を学びます。

## 1. Introduction
## 2. Verifying Simple Programs
### 2.1 Assertions

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/aMuM)

```c
#include <vcc.h>

int main()
{
  int x,y,z;
  if (x <= y)
    z = x;
  else z = y;
  _(assert z <= x)
  return 0;
}
```

`_(assert z <= x)`という部分はannotationと呼ばれていて、C言語コンパイラは`vcc.h`中の以下の定義を参照します:

```c
#define _(...) /* nothing */
```

しかしVCCは上記annotationを特殊構文と見做します。
`_(assert E)`と書くことで、当該行にて式`E`が真であることをVCCは保証します。

### 2.2 Logical Operators and Quantifiers

annotationでの構文はC言語から拡張されていて、以下を使うことができます:

* `==>`: Boolean operator。例えば`P ==> Q`と書くことで「`P`ならば`Q`」を表わします。
* `\forall T v; E`: もし`E`が型`T`の全て値`v`に対して`E`が非ゼロに評価できるなら、この式は`1`に評価されます。例えば`_(assert x > 1 && \forall int i; 1 < i && i < x ==> x % i != 0)`は`x`が素数であることを検査します。`_(assert \forall int i; \forall int j; 0 <= i && i <= j && j < N ==> b[i] <= b[j])`は`b`がソート済みであることを検査します。
* `\exists T v; E`: もし`E`が非ゼロに評価されるような型`T`の値`v`が在るなら、この式は`1`に評価されます。例えば`_(assert \exists int i; 0 <= i && i < N && b[i] == 0)`は`b`が`0`の要素を持つことを表明しています。
* `\integer`: 整数型を表わします。
* `\natural`: 自然数型を表わします。
* map type: 6章で紹介します。
* `pure`: 3.4章で紹介します。

### 2.3 Assumptions

VCCはゼロ除算を検査します。例えば以下のコードは検査エラーになります:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/dhOi)

```c
#include <vcc.h>

int main()
{
  int x, y;
  y = 100 / x;
  return 0;
}
```

しかし、以下のようにassumptionを追加することで検査を通すことができます:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/JHgi)

```c
#include <vcc.h>

int main()
{
  int x, y;
  _(assume x != 0)
  y = 100 / x;
  return 0;
}
```

また、assertionもVCCにassumptionを追加します。
もしassertionの検証に失敗しても、もちろんそのエラーはレポートしますが、表明された事実を仮定して検査を続行します。
例えば以下のコードは最初のassertionはエラーになりますが、次のassertionのエラーはレポートされません:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/NF9)


```c
#include <vcc.h>

int main()
{
  int x;
  _(assert x == 1)
  _(assert x > 0)
  return 0;
}
```

### 2.4 Overflows and unchecked arithmetic

xxx 後で調べる

#### 2.4.1 Bitvector Reasoning

xxx 後で調べる

## 3. Function Contracts

以下のように最小値を求める処理を関数にすると検証はエラーになります:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/04cG)

```c
#include <vcc.h>

int min(int a, int b)
{
  if (a <= b)
    return a;
  else return b;
}

int main()
{
  int x, y, z;
  z = min(x, y);
  _(assert z <= x)
  return 0;
}
```

これはVCCの検証がmodularであり、関数呼び出しにおいてその関数本体までVCCが調べないためです。
関数の仕様はときにcontractと呼ばれ、その関数自身と呼び元に影響します。
このcontractは次の4種類のannotationで与えられます:

* `requires`: 事前条件。
* `ensures`: 事後条件。
* `writes`: 次の章で説明します。
* `decreases`: 3.3章で説明します。

先の`min`関数は以下のようなcontractを追加することで検査に成功します:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/E2V3)

```c
#include <vcc.h>

int min(int a, int b)
  _(requires \true)
  _(ensures \result <= a && \result <= b)
{
  if (a <= b)
    return a;
  else return b;
}

int main()
{
  int x, y, z;
  z = min(x, y);
  _(assert z <= x)
  return 0;
}
```

### 3.1 Reading and Writing Memory

C言語におけるメモリアクセスは2種類に分類できます。

1つ目はSequentialアクセスで、他のスレッドと共有されていないメモリアクセスです。これはデフォルトの動作です。

2つ目はAtomicアクセスで、複数のスレッドから読み書きされる可能性があります。一般にこのようなメモリアクセスは`volatile`型を使います。
(訳注: `volatile`つけてもアトミックと見做せないんじゃなかったでしたっけ。。。[POS03-C](https://www.jpcert.or.jp/sc-rules/c-pos03-c.html))

Atomicアクセスについては8章で扱い、この章ではSequentialアクセスのみ紹介します。

xxx
