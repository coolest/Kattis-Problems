import Control.Monad (unless)

p :: Double -> Double
p x = if x < 0 then x * (-1) else x

main = do
    line <- getLine

    unless (line == "0") $ do
        let [x1, y1, x2, y2, p'] = map (\x -> read x :: Double) (words line)
        
        print $ (p (x1-x2) ** p' + p (y1-y2) ** p')**(1/p')
        
        main
