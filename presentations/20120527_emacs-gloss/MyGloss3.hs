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
jumpTo WDown a b  = ( b - a, -b - a)
jumpTo WRight a b = ( b + a,  b - a)
jumpTo WUp a b    = (-b + a,  b + a)

posFib' :: Num t => (t, t) -> Way -> [t] -> [(t, t)]
posFib' c w (x:xs) = c' : posFib' c' (next w) xs
  where
    tplus a b = let (f, s) = unzip [a, b] in (sum f, sum s)
    c' = c `tplus` jumpTo w x (head xs)
posFib' _ _ _ = error "posFib'"

posFib :: Num t => [t] -> [(t, t)]
posFib f = posFib' (0, 0) WDown (0:f)

posCircle :: Float -> (Float, Float) -> Picture
posCircle r (x, y) = Translate x y $ Circle r

mypic :: Float -> Picture
mypic t = Scale s s $ Pictures $ take n $ zipWith posCircle r $ posFib r
  where s = 10 / (1.4 ** t)
        r = fmap (*5) fib
        n = truncate t

main :: IO ()
main = animate d white mypic
  where d = InWindow "Nice Window" (1024 `div` 2, 768 `div` 2) (10, 10)
