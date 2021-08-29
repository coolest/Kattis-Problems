import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Bifunctor (bimap)
import Data.Foldable (toList, traverse_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

p :: Int -> Double
p x 
    | x < 0 = fromIntegral $ x *(-1)
    | otherwise = fromIntegral x

main = do
    [c, d] <- getInts

    seq <- collect (S.fromList $ replicate d (0, 0)) c

    let wvSeq = fmap (uncurry wastedVotes) seq
    let tv = fromIntegral $ foldl (\ans (a, b) -> ans+a+b) 0 seq
    let (a, b) = foldl (\(a, b) (a', b', _) -> (a+a', b+b')) (0, 0) wvSeq

    traverse_ (putStrLn . handle) wvSeq
    print (p (a-b) / tv)

    where 
        handle (a, b, w) = w ++ " " ++ show a ++ " " ++ show b

        wastedVotes a b
            | a > b = (a-((a+b)`div`2+1), b, "A")
            | otherwise = (a, b-((a+b)`div`2+1), "B")

        collect :: S.Seq (Int, Int) -> Int -> IO (S.Seq (Int, Int))
        collect seq 0 = return seq
        collect seq c = do
            [i, a, b] <- getInts
            collect (S.adjust (bimap (+ a) (+ b)) (i-1) seq) (c-1)