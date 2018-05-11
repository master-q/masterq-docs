# さわって学ぶVCC

(本ページの短縮URL: http://bit.ly/sawa-vcc )

["Verifying C Programs: A VCC Tutorial"](https://swt.informatik.uni-freiburg.de/teaching/SS2015/swtvl/Resources/literature/vcc-tutorial-col2.pdf)で紹介されているコードを[オンライン](https://rise4fun.com/Vcc)で実行させながら[VCC](https://github.com/Microsoft/vcc)検証器の使い方を学びます。

この翻訳は完全な翻訳でありません。また訳語も安定していません。詳しくは[原文](https://swt.informatik.uni-freiburg.de/teaching/SS2015/swtvl/Resources/literature/vcc-tutorial-col2.pdf)を参照してください。

## 1 Introduction
## 2 Verifying Simple Programs
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

## 3 Function Contracts

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

ポインタ`p`で指されるmemory objectにアクセスするためには、`p`が有効なメモリチャンクを指さなければなりません。
さらにメモリにSequentialなアクセスをするために、そのメモリは別のスレッドから並列に書き込まれてはなりません。
大抵の場合、これはmemory objectがスレッドによって所有された何かの一部だと解釈されます。
私達はこれをpredicate`\thread_local(p)`で表現し、VCCは`p`でSequentialアクセスする前にこれを主張しなければなりません。

`p`を通じたSequentialな書き込みには`\thread_local(p)`がが必要です。
このとき同時に他のスレッドはSequentialにも並列にも`p`を読めません。
これを`\mutable(p)`とも書けます。
`\thread_local`と同様に`\mutable`はあるスレッドのコンテキスト中でのみ意味を持ちます。

配列の範囲外を指すような`NULL`ポインタやアドレス空間の範囲外はスレッドローカルではありません。

またSequentialな書き込みにはもう1つの制限があり、それはmutableなobjectの集合を特定するために関数呼び出しのありうる副作用を制限するためです。
状態のそれ以外全ての部分が修正されていないことを事後条件を追加することでこれが可能になりますが、VCCはそんな状況を表わす便利な構文を提供しています。
そのアイデアはある関数を呼び出すとき、その関数にあるobjectへの書き込み権限を与え、他の関数には与えないというものです;
`\writable(p)`はその関数が`p`へ書き込む権利を持つことを表現しています。
そこで、`p`に書き込みたい場合、VCCでは`\mutable(p) && \writable(p)`と主張します。

`\mutable`はスレッドレベルで意味を持つのに対して、`\writable`は実行する関数個別の特性です。
したがって、
関数が`p`へ書き込みできる権限が必要であることを`_(requires \writable(p))`で表現できません。
なぜなら事前条件は関数呼び出し元のコンテキストで評価されるからです。
代わりに関数が`p`への書き込み権限が必要であることを、関数の入口でannotation`_(writes p)`で指定できます。
これをwrites clauseと呼びます。
関数を呼び出すと、VCCはwrites clause中の全てのobjectが書き込み可能であると関数入口で仮定します。

以下の例を見てみましょう:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/2oyd)

```c
#include <vcc.h>

void write(int *p)
  _(writes p)
{ *p = 42; }

void write_wrong(int *p)
  _(requires \mutable(p))
{ *p = 42; }

int read1(int *p)
  _(requires \thread_local(p))
{ return *p; }

int read2(int *p)
  _(requires \mutable(p))
{ return *p; }

int read_wrong(int *p)
{ return *p; }

void test_them()
{
  int a = 3, b = 7;
  read1(&a);
  _(assert a == 3 && b == 7) // OK
  read2(&a);
  _(assert a == 3 && b == 7) // OK
  write(&a);
  _(assert b == 7) // OK
  _(assert a == 3) // error
}
```

`write_wrong`関数は失敗します。なぜなら`p`はmutableであるのみだからからで、書き込み可能でありません。
`read_wrong`では、`p`に関して何も情報がないとVCCはエラーを吐きます。特にそれがスレッドローカルであるかどうかもVCCは知りません。
`read2`は正常です。なぜなら`\mutable`は`\thread_local`よりも強いからです。
最後に`test_them`における最初3つのassertionは成功します。なぜならwrites clause中に書いていない対象は当該関数から変更不能だからです。
最後のassertionは失敗します。なぜなら`write()`の引数`&a`はwrites clauseに書かれているためです。

直感的に節`_(writes p, q)`は、memory objectはスレッドローカルで、その関数は`p`で指されたobjectと`q`で指されたobjectのみ修正できます。

以下にメモリを明示的に読み書きする関数の単純な例を挙げます;
この関数はある位置のデータを別の位置にコピーします。

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/Befk)

```c
#include <vcc.h>

void copy(int *from, int *to)
  _(requires \thread_local(from))
  _(writes to)
  _(ensures *to == \old(*from))
{
  *to = *from;
}

int z;

void main()
  _(writes &z)
{
  int x,y;
  copy(&x,&y);
  copy(&y,&z);
  _(assert x==y && y==z)
}
```

事後条件における式`\old(E)`は関数が開始したときに式`E`が保持していた値を返します。
すなわち、この事後条件は`*to`の新しい値と`*from`が関数開始時に保持していた値が等しいことを表明しています。

#### 3.1.1 Local Variables

VCCはアドレスを取り出さないlocal variableとアドレスを扱うものを分けて扱います。
前者はpurely local variableと呼ばれます。
purely local variableはより推論しやすく; その値はその名前を通してのみ更新されるとわかります。
purely localw variableは常にスレッドローカルで買い込み可能です。
また、もしそのスコープでpurely local variableを修正しないループがあるなら、VCCは自動的にその変数の値がそのループ中で変更されないことを推論します。

purely local variableではないlocal variableは、ライフタイムの開始でヒープに確保されたらなら、ライフタイムの終了では解放されます。

### 3.2 Arrays

配列アクセスはポインタアクセスの一種です。
そのため、配列の要素を読むこと許可する前に、VCCはその配列がスレッドローカルであるかどうかチェックします。
通常は配列の全ての要素を`\thread_local_array(ar, sz)`式を使ってスレッドローカルにしたいでしょう。
これは`\forall unsigned i; i < sz ==> \thread_local(&ar[i])`の糖衣構文です。
annotation`\mutable_array()`も同様です。
配列が書き込み可能であることを示すには以下を使います:

```c
_(writes \array_range(ar, sz))
```

上記は以下の構文糖衣です:

```c
_(writes &ar[0], &ar[1], ..., &ar[sz-1])
```

例として、次の関数はCスタンダードライブラリ`memcpy()`関数の再帰実装です:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/TT2e)

```c
#include<vcc.h>

void my_memcpy(unsigned char *dst,
               unsigned char *src,
               unsigned len)
  _(writes \array_range(dst, len))
  _(requires \thread_local_array(src, len))
  _(requires \arrays_disjoint(src, len, dst, len))
  _(ensures \forall unsigned i; i < len
      ==> dst[i] == \old(src[i]))
{
  if (len > 0) {
    dst[len - 1] = src[len - 1];
    my_memcpy(dst, src, len - 1);
  }
}
```

`src`と`dst`はオーバラップしません。

### 3.3 Termination

`_(decreases 0)`と単純に指定すると、その関数は再帰しません。
再帰する関数であれば、その関数の`decreases`に関数の呼び出し毎に現象するmeasureを与えます。
例えば、上記の`my_memcpy`の停止を検証するためには、annotation`_(decreases len)`を追加するだけです。

### 3.4 Pure functions

pure functionは副作用を持たない関数です。
VCCではpure functionはメモリを確保できませんし、local variableにのみ書き込めます。
VCCのannotationから呼び出せるのはpure functionのみです; この関数は停止することが求められます。

上記の関数`min()`はpureとして宣言できます;
そのためには修飾子`_(pure)`を関数仕様の前に追加します。例えば:

```c
_(pure) min(int x, int y) ...
```

決して実行したくないpure functionは`_(def)`タグを使って定義できます。
これはpure ghost functionと呼ばれ、次のように使います:

```c
_(def \bool sorted(int *arr, unsigned len) {
  return \forall unsigned i, j;
    i <= j && j < len ==> arr[i] <= arr[j];
})
```

ソートの部分的な仕様は次のようになります:

```c
void sort(int *arr, unsigned len)
  _(writes \array_range(arr, len))
  _(ensures sorted(arr, len))
```

### 3.5 Contracts on Blocks

関数内ブロックに対する仕様を以下のように書けます:

```c
x = 5;
_(requires x == 5)
_(writes &x)
_(ensures x == 6)
{
  x++;
}
```

## 4 Loop invariants

ループの先頭で知るべきことを類推しようとするかわりに、VCCはループが知るべき情報をloop invariantで与えます。

例を見てみましょう:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/Cprv)

```c
#include <vcc.h>

void divide(unsigned x, 
            unsigned d, 
            unsigned *q, 
            unsigned *r)
_(requires d > 0 && q != r)
_(writes q,r)
_(ensures x == d*(*q) + *r && *r < d)
{
  unsigned lq = 0;
  unsigned lr = x;
  while (lr >= d)
  _(invariant x == d*lq + lr)
  {
    lq++;
    lr -= d;
  }
  *q = lq;
  *r = lr;
}
```

`divide()`関数は`x`を`d`で割った商と余りを古典的なアルゴリズムで求めます。
loop invariantは余りが大きくならないように強制しています。

別のループの例として次の関数は配列中に値が出現するかどうか線形探索します:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/fWT)

```c
#include <vcc.h>
#include <limits.h>

unsigned lsearch(int elt, int *ar, unsigned sz)
  _(requires \thread_local_array(ar, sz))
  _(ensures \result != UINT_MAX 
     ==> ar[\result] == elt)
  _(ensures \forall unsigned i; 
    i < sz && i < \result ==> ar[i] != elt)
{
  unsigned i;
  for (i = 0; i < sz; i++)
    _(invariant \forall unsigned j; 
        j < i ==> ar[j] != elt)
  {
    if (ar[i] == elt) return i;
  }
  return UINT_MAX;
}
```

事後条件は返り値が`elt`が出現した最初の配列インデックスであることを主張しています。
loop invariantは`elt`が`ar[0] . . . ar[i - 1]`中には出現しないことを主張しています。

### 4.1 Termination measures for loops

ループが終了することを証明するために、関数と同様に、`_(decreases)`節を使います。

例えば、`divide`関数では、ループに`_(decreases lr)`を追加することで、ループの終了を指定できます。
これで`divide`関数に`_(decreases 0)`を追加できます。

もしtermination measure付き関数がtermination measureなしの`for`ループを含むなら、VCCはそのループのヘッダの構文を推測しようとします。
すなわち、多くの`for`ループは明示的なtermination measureを要求しません。

以下はバブルソートを使って配列をソートする関数の例です。
VCCは外側のループのtermination measureを推論しますが、内側のループのtermination measureは必要です:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/WuXj)

```c
#include <vcc.h>

_(def \bool sorted(int *buf, unsigned len) {
  return \forall unsigned i, j; i < j && j < len 
            ==> buf[i] <= buf[j];
})

void sort(int *buf, unsigned len)
  _(writes \array_range(buf, len))
  _(ensures sorted(buf, len))
  _(decreases 0)
{
  if (len < 2) return;
  for (unsigned i = len; i > 0; i--)
    _(invariant i <= len)
    _(invariant \forall unsigned u,v; 
      i <= v && u <= v && v < len 
      ==> buf[u] <= buf[v])
    for (unsigned j = 0; j + 1 < i; j++)
      _(decreases i-j)
      _(invariant j < i)
      _(invariant \forall unsigned u,v; 
          i <= v && u <= v && v < len 
          ==> buf[u] <= buf[v])
      _(invariant \forall unsigned u; u < j 
          ==> buf[u] <= buf[j])
      if (buf[j] > buf[j+1]) {
        int tmp = buf[j];
        buf[j] = buf[j+1];
        buf[j+1] = tmp;
      }
}
```

この仕様はソートの結果が確かにソートされていることを表わします。
しかし、この仕様はその結果が置換(permutation)されたものであることを表わしません。
これについては6.2章で扱います。

### 4.2 Writes clauses for loops

ループは多くの面で再帰関数と似ています。
invariantは事前条件と事後条件の組み合わせとして働きます。
関数と同様に、ループもまたwrites clauseを持ちます。
関数に対してと同様、同じ構文を使えます。
ループ中でヒープに書き込まないのであれば、VCCは自動的に空のwrites clauseを推論します。
そうでねければ、関数で指定されたwrites clauseを取ります。
そのためデフォルトでは、ループは関数が書ける全ての対象に書き込めます。
次は暗黙のwrites clauseの例です:

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/7AsB)

```c
#include<vcc.h>

void my_memcpy(unsigned char *dst, 
               unsigned char *src, 
               unsigned len)
  _(writes \array_range(dst, len))
  _(requires \thread_local_array(src, len))
  _(requires \arrays_disjoint(src, len, dst, len))
  _(ensures \forall unsigned i; i < len ==> 
       dst[i] == \old(src[i]))
  _(decreases 0)
{
  unsigned k;
  for (k = 0; k < len; ++k)
    _(invariant \forall unsigned i; i < k ==> 
        dst[i] == \old(src[i]))
  {
    dst[k] = src[k];
  }
}
```

(VCCは`for`ループの適切なtermination measureも推論することに注意してください。)

もしループが関数が書き込める全てに書き込まないのであれば、しばしば明示的なwrite clausesを指定したくなるかもしれません。
次はコピーした後にソースバッファをクリアする`memcpy()`の変種です。

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/JleM)

```c
#include<vcc.h>

void memcpyandclr(unsigned char *dst, 
                  unsigned char *src, 
                  unsigned len)
  _(writes \array_range(src, len))
  _(writes \array_range(dst, len))
  _(requires \arrays_disjoint(src, len, dst, len))
  _(ensures \forall unsigned i; i < len 
      ==> dst[i] == \old(src[i]))
  _(ensures \forall unsigned i; i < len 
      ==> src[i] == 0)
  _(decreases 0)
{
  unsigned k;
  for (k = 0; k < len; ++k)
    _(writes \array_range(dst, len))
    _(invariant \forall unsigned i; i < k 
        ==> dst[i] == \old(src[i]))
  {
    dst[k] = src[k];
  }
  for (k = 0; k < len; ++k)
    _(writes \array_range(src, len))
    _(invariant \forall unsigned i; i < k 
        ==> src[i] == 0)
  {
    src[k] = 0;
  }
}
```

もし二番目のループがwrites clauseを持たないと、最初の事後条件を証明できません—VCCは二番目のループが`dst`を上書きすると見做してしまうのです。

## 5 Object invariants

VCCでは、C言語の型(structやunion)を組み合わせとobject invariantを関連付けることができます。
型のinvariantはその型の良いobjectの振舞いを表現します。
この章からは、この振舞いの静的な側面を扱います。
invariantはまた良いobjectの変化も記述でき、これは8章で学びます。

最初の例として、静的に確保された配列で実装された、`\0`で終端された安全な文字列の型定義を考えましょう。

[下記コードをオンラインで実行](https://rise4fun.com/Vcc/cEXs)

```c
#include <vcc.h>

/*{obj}*/
#define SSTR_MAXLEN 100
typedef struct SafeString {
  unsigned len;
  char content[SSTR_MAXLEN + 1];
  _(invariant \this->len <= SSTR_MAXLEN)
  _(invariant content[len] == '\0')
} SafeString;
```

`SafeString`のinvariantは、良い`SafeString`は長さが`SSTR_MAXLEN`以下で、`\0`終端されていることを表明しています。
type invariant中では`\this`は型のインスタンスへのポインタで、構造体メンバーは直接指定できます。

C言語のメモリは確保時に初期化されないため、object invariantが確保時に保持されることは保証できません。
良い状態のobjectはclosedであると呼ばれ、良い状態にないobjectはopenであると呼ばれます。
mutable objectは実行中のスレッドに所有されたopen objectで、wrapped objectは実行中のスレッドに所有されたclosed objectです。
型のコンストラクタとして動作する関数は通常、invariantを成立させ、objectをwrapします:

```c
/*{init}*/
void sstr_init(struct SafeString *s)
  _(writes \span(s))
  _(ensures \wrapped(s))
  _(decreases 0)
{
  s->len = 0;
  s->content[0] = '\0';
  _(wrap s)
}
```

struct型のポインタ`p`に対して、`\span(p)`は`p`のメンバーへのポインタの集合を返します。
そのため上記の例では`\span(s)`は次の集合を省略です:

```c
{ s, &s->len, &s->content[0], &s->content[1], ...,
  &s->content[SSTR_MAXLEN] }
```

したがって、 xxx

```c
/*{append}*/
void sstr_append_char(struct SafeString *s, char c)
  _(requires \wrapped(s))
  _(requires s->len < SSTR_MAXLEN)
  _(ensures \wrapped(s))
  _(writes s)
  _(decreases 0)
{
  _(unwrap s)
  s->content[s->len++] = c;
  s->content[s->len] = '\0';
  _(wrap s)
}
```

xxx

```c
/*{index}*/
int sstr_index_of(struct SafeString *s, char c)
  _(requires \wrapped(s))
  _(ensures \result >= 0 ==> s->content[\result] == c)
  _(decreases 0)
{
  unsigned i;
  for (i = 0; i < s->len; ++i)
          _(decreases s->len - i)
    if (s->content[i] == c) return (int)i;
  return -1;
}
```

xxx
