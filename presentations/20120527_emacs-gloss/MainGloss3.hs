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
