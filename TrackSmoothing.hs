import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

--
getD :: [C.ByteString] -> Double -> Double
getD [x] d = d
getD (pt1:pt2:pts) d = getD (pt2:pts) (d+sqrt(fromIntegral(x1-x2)**2 + fromIntegral(y1-y2)**2))
    where
        [x1, y1] = map readInt (C.split ' ' pt1)
        [x2, y2] = map readInt (C.split ' ' pt2)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle xs

    where
        handle [] = return ()
        handle (x:xs) = do
            let [r, n] = map readInt (C.split ' ' x)

            let pts = take n xs
            let d = getD (last pts : pts) 0 
            let c = fromIntegral r*2*pi

            case (d `compare` c) of
                LT -> putStrLn "Not possible"
                EQ -> print 0
                GT -> print ((d-c)/d)

            handle (drop n xs)
            
