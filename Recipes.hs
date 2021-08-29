import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Text.Printf (printf)
import Control.Monad (mapM_)

readInt = fst . fromJust . C.readInt

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle 1 xs

    where
        handle :: Int -> [C.ByteString] -> IO ()
        handle _ [] = return ()
        handle a (x:xs) = do
            let (n:p1:p2:_) = map readInt (C.split ' ' x)

            let ingreds = map (C.split ' ') (take n xs)
            let ingredNames = map head ingreds
            let ingredInfo = map ((map (\x -> read (C.unpack x) :: Double)) . tail) ingreds
            let mainIndex = fst $ foldl (\(a, a') x -> if a' == True then (a, a') else if last x == 100.0 then (a, True) else (a+1, a')) (0, False) ingredInfo
            let mainG = head (ingredInfo !! mainIndex) * (fromIntegral p2/ fromIntegral p1)

            let ans = zipWith (\a (g:p:_) -> (a, mainG*(p/100))) ingredNames ingredInfo
            
            printf "Recipe # %d\n" a
            mapM_ (\(a, b) -> printf "%s %.1f\n" (C.unpack a) b) ans
            putStrLn "----------------------------------------"

            handle (a+1) (drop n xs)