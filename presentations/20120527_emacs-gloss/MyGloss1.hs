import Graphics.Gloss

fib :: [Float]
fib = 1:1:zipWith (+) fib (tail fib)

posfib' :: Num t => t -> [t] -> [(t, t)]
posfib' c (x:xs) = (c, x) : posfib' (c + x + head xs) xs
posfib' _ _ = error "posfib'"

posfib :: Num t => [t] -> [(t, t)]
posfib f = posfib' 0 f

posCircle :: Float -> (Float, Float) -> Picture
posCircle r (x, y) = Translate x y $ Circle r

mypic :: Picture
mypic = Pictures $ take 20 $ zipWith posCircle fib $ posfib fib

main :: IO ()
main = display d white mypic
  where d = InWindow "Nice Window" (1024 `div` 2, 768 `div` 2) (10, 10)
