import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM)
import Data.List (sortBy, groupBy)
import qualified Data.Set as S

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.split ' ' <$> C.getLine

main = do
    n <- getInt

    replicateM n $ do
        [_, m] <- getInts
        xs <- replicateM m ((\[a, b] -> (a, b)) <$> getInts)

        let importantXS = filter (uncurry (/=)) xs
        let (ans, set) = handle 0 S.empty  importantXS
        let (ansFINAL, _) = handle ans set xs
        
        print ansFINAL

        where
            handle ans set [] = (ans, set)
            handle ans set ((a, b):xs)
                | memA && memB = handle (ans+1) (S.union (S.fromList [a, b]) set) xs
                | not memA && memB = handle (ans+1) (S.insert b set) xs
                | memA && not memB = handle (ans+1) (S.insert a set) xs
                | otherwise = handle ans set xs
                where
                    memA = S.notMember a set
                    memB = S.notMember b set

