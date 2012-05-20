# EmacsとGlossでお絵描きしてみるよ
![background](debian.png)

Kiwamu Okabe

# わたしはだれ？

* Twitter: @master_q
* C言語とRubyは使えます
* Haskellをごくたまに使う
* てんぷれはすけるとかわかりません ＞＜

# Emacs使う理由

ghc-modが使えるから!

~~~
http://www.mew.org/~kazu/proj/ghc-mod/en/
~~~

# Glossってなんじゃらほい？

http://hackage.haskell.org/package/gloss

~~~
Gloss hides the pain of drawing simple vector graphics behind a nice data type and a few display functions. Gloss uses OpenGL under the hood, but you won't need to worry about any of that. Get something cool on the screen in under 10 minutes.
~~~

OpenGLの上に作られたお絵描きライブラリなんですね。

# 参考書とか説明書とか

PreludeとGlossのhaddockがあればなんとかなるんじゃなイカ？

~~~
http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html
http://hackage.haskell.org/packages/archive/gloss/latest/doc/html/Graphics-Gloss.html
http://www.haskell.org/hoogle/
~~~

# 今日作ったコード #1

~~~ {.haskell}
import Graphics.Gloss

fib :: [Float]
fib = 1:1:zipWith (+) fib (tail fib)

data Way = WLeft | WRight | WUp | WDown
next :: Way -> Way
next WLeft  = WDown
next WDown  = WRight
next WRight = WUp
next WUp    = WLeft

jumpTo :: Num t => Way -> t -> t -> (t, t)
jumpTo WLeft a b  = (-b - a, -b + a)
jumpTo WDown a b  = (b - a, -b - a)
jumpTo WRight a b = (b + a, b - a)
jumpTo WUp a b    = (-b + a, b + a)
~~~

(次ページに続く)

# 今日作ったコード #2

~~~ {.haskell}
position :: Num b => [b] -> [(b, b)]
position l = pos (0, 0) WDown (0 : l)
  where 
    pos p w (x:xs) = let p' = p `tplus` jumpTo w x (head xs)
                     in p' : pos p' (next w) xs
    pos _ _ [] = []
    tplus a b = let (f, s) = unzip [a, b] in (sum f, sum s)

putCircle :: (Float, Float) -> Float -> Picture
putCircle (x, y) r = Translate x y $ Circle r

circles :: [Float] -> Float -> Picture
circles l t = Scale s s . Pictures . take (truncate t) $ c
  where r = fmap (*5) l
        s = 10 / (1.4 ** t)
        c = zipWith putCircle (position r) r

main :: IO ()
main = animate win white $ circles fib
  where win = InWindow "MyGlossApp" (1024 `div` 2, 768 `div` 2) (0, 0)
~~~

# ちょっとウンチク

http://ja.wikipedia.org/wiki/黄金長方形

![inline](golden_rect.png)

# 宣伝: HaskellやるならDebian!

* 次のリリースではghc 7.4が使えるよ
* (実はUbuntuのHaskellパッケージはDebian開発者がメンテしてるんだよ!)
* プロジェクトリーダーはOCaml使いだよ
* 関数型ならDebianだね!
* Debian勉強会やってるらしいよ
* http://tokyodebian.alioth.debian.org/
