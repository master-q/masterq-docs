# プログラミングHaskell 13章 問題7
![background](debian.png)

Kiwamu Okabe

# 13章 問題7 ってなんどす？
以下の定義が与えられているとする。

~~~
(定義1) map f [] = []
(定義2) map f (x:xs) = f x : map f xs
(定義3) (f . g) x = f (g x)
~~~

このとき

~~~
map f (map g xs) = map (f . g) xs
~~~

であることを、

xsに対する数学的帰納法で証明せよ。

# 空リストに対して成り立つ？ #1

~~~
map f (map g []) = map (f . g) []
~~~

は成立するのか？

# 空リストに対して成り立つ？ #2

~~~
map f (map g []) = map (f . g) []
map f [] = []
~~~

以下の定義を適用。

~~~
(定義1) map f [] = []
~~~

# 空リストに対して成り立つ？ #3

~~~
map f (map g []) = map (f . g) []
map f [] = []
[] = []
~~~

以下の定義を適用。

~~~
(定義1) map f [] = []
~~~

空リストに対しては成立しましたね。やった

# 帰納法を使ってみよう #1

~~~
(仮定1) map f (map g xs) = map (f . g) xs
~~~

を仮定して

~~~
map f (map g (x:xs)) = map (f . g) (x:xs)
~~~

であることが示せれば証明完了デスネ。

# 帰納法を使ってみよう #2

~~~
map f (map g (x:xs)) = map (f . g) (x:xs)
map f (g x : map g xs) = (f . g) x : map (f . g) xs
~~~

以下の定義を適用。

~~~
(定義2) map f (x:xs) = f x : map f xs
~~~

# 帰納法を使ってみよう #3

~~~
map f (map g (x:xs)) = map (f . g) (x:xs)
map f (g x : map g xs) = (f . g) x : map (f . g) xs
f (g x) : map f (map g xs) = (f . g) x : map (f . g) xs
~~~

再度以下の定義を適用。

~~~
(定義2) map f (x:xs) = f x : map f xs
~~~

# 帰納法を使ってみよう #4

~~~
map f (map g (x:xs)) = map (f . g) (x:xs)
map f (g x : map g xs) = (f . g) x : map (f . g) xs
f (g x) : map f (map g xs) = (f . g) x : map (f . g) xs
f (g x) : map f (map g xs) = f (g x) : map (f . g) xs
~~~

以下の定義を適用。

~~~
(定義3) (f . g) x = f (g x)
~~~

# 帰納法を使ってみよう #5

~~~
map f (map g (x:xs)) = map (f . g) (x:xs)
map f (g x : map g xs) = (f . g) x : map (f . g) xs
f (g x) : map f (map g xs) = (f . g) x : map (f . g) xs
f (g x) : map f (map g xs) = f (g x) : map (f . g) xs
f (g x) : map (f . g) xs = f (g x) : map (f . g) xs
~~~

仮定1を適用。

~~~
(仮定1) map f (map g xs) = map (f . g) xs
~~~

これで帰納法を使って証明できました。

めでたしめでたし。

# 宣伝: λカ娘執筆者募集中!

~~~
http://www.paraiso-lang.org/ikmsm/
~~~

にいますぐアクセス!
