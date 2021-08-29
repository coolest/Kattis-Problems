import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

r :: Float -> Float
r = fromIntegral . round

getN :: Float -> Int -> (Int, Int)
getN l x = if s == r s then (round s, x) else getN (l+1) (x+1)
    where 
        s = sqrt l

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle xs

    where
        handle [] = return ()
        handle (x:xs) = do
            let (n, stars) = getN (fromIntegral $ C.length x) 0
            let x' = C.append x (C.pack $ replicate stars '*')
            let msg = C.filter (/='*') . C.concat . C.transpose . fst $ foldl (\(a, x) _ -> (C.take n x : a, C.drop n x)) ([], x') [1..n]
            C.putStrLn msg

            handle xs