# さわって学ぶVCC

["Verifying C Programs: A VCC Tutorial"](https://swt.informatik.uni-freiburg.de/teaching/SS2015/swtvl/Resources/literature/vcc-tutorial-col2.pdf)で紹介されているコードを[オンライン](https://rise4fun.com/Vcc)で実行させながら[VCC](https://github.com/Microsoft/vcc)検証器の使い方を学びます。

## 1. Introduction
## 2. Verifying Simple Programs
### 2.1 Assertions

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

[オンラインで実行](https://rise4fun.com/Vcc/aMuM)

`_(assert z <= x)`という部分はannotationと呼ばれていて、C言語コンパイラは`vcc.h`中の以下の定義を参照します:

```c
#define _(...) /* nothing */
```

しかしVCCは上記annotationを特殊構文を見做します。
`_(assert E)`と書くことで、当該行にて式`E`が真であることをVCCは保証します。

xxx
