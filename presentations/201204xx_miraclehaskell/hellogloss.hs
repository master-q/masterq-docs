import Graphics.Gloss

main :: IO ()
main = display (InWindow "Hoge" (200, 200) (10, 10)) 
       white (Circle 80)
