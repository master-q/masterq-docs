import Graphics.Gloss

fib :: [Float]
fib = 1:1:zipWith (+) fib (tail fib)

pos :: Num a => [a] -> [a]
pos l = 0 : pos' l
  where pos' (x:xs) = (x + head xs) : pos' xs
        pos' [] = []

position :: Num b => [b] -> [(b, b)]
position l = zip x y
  where x = scanl1 (+) $ pos l
        y = l

putCircle :: (Float, Float) -> Float -> Picture
putCircle (x, y) r = Translate x y $ Circle r

circles :: [Float] -> Picture
circles l = Pictures $ zipWith putCircle (position r) r
  where r = take 20 $ fmap (*5) l
        
underline :: Picture
underline = Line [(0, 0), (10000, 0)]

main :: IO ()
main = display win white $ Pictures [underline, circles fib]
  where win = InWindow "MyGlossApp" (1024 `div` 2, 768 `div` 2) (0, 0)
