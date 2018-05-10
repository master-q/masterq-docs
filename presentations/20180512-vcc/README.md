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

しかしVCCは上記annotationを特殊構文を見做します。
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

xxx
