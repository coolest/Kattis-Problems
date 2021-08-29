import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Control.Monad (replicateM)
import Data.Maybe (isJust)

zero = S.fromList [[C.pack "***", C.pack "* *", C.pack "* *", C.pack "* *", C.pack "***"]]
one = S.fromList [[C.pack "  *", C.pack "  *", C.pack "  *", C.pack "  *", C.pack "  *"]]
two = S.fromList [[C.pack "***", C.pack "  *", C.pack "***", C.pack "*  ", C.pack "***"]]
three = S.fromList [[C.pack "***", C.pack "  *", C.pack "***", C.pack "  *", C.pack "***"]]
four = S.fromList [[C.pack "* *", C.pack "* *", C.pack "***", C.pack "  *", C.pack "  *"]]
five = S.fromList [[C.pack "***", C.pack "*  ", C.pack "***", C.pack "  *", C.pack "***"]]
six = S.fromList [[C.pack "***", C.pack "*  ", C.pack "***", C.pack "* *", C.pack "***"]]
seven = S.fromList [[C.pack "***", C.pack "  *", C.pack "  *", C.pack "  *", C.pack "  *"]]
eight = S.fromList [[C.pack "***", C.pack "* *", C.pack "***", C.pack "* *", C.pack "***"]]
nine = S.fromList [[C.pack "***", C.pack "* *", C.pack "***", C.pack "  *", C.pack "***"]]

getLetter :: S.Seq [C.ByteString] -> [C.ByteString] -> (S.Seq [C.ByteString], [C.ByteString])
getLetter ans xs = (S.singleton . concat . snd $ foldl (\(ind, seq) bstr -> (ind+1, S.adjust (\xs -> C.take 3 bstr : xs) ind seq)) (0, ans) xs, map (C.drop 4) xs)

empty = S.fromList (replicate 5 [])

getNum :: [S.Seq [C.ByteString]] -> Int -> Maybe Int
getNum [] ans = Just ans
getNum (x:xs) ans
    | x == zero = getNum xs (ans*10)
    | x == one = getNum xs (ans*10+1)
    | x == two = getNum xs (ans*10+2)
    | x == three = getNum xs (ans*10+3)
    | x == four = getNum xs (ans*10+4)
    | x == five = getNum xs (ans*10+5)
    | x == six = getNum xs (ans*10+6)
    | x == seven = getNum xs (ans*10+7)
    | x == eight = getNum xs (ans*10+8)
    | x == nine = getNum xs (ans*10+9)
    | otherwise = Nothing

strip (Just a) = a

main = do
    contents <- replicateM 5 (C.init <$> C.getLine)
 
    let n = getNum (reverse $ getLetters contents []) 0
    if isJust n
        then do
            let n' = strip n
            if n' `mod` 6 == 0
                then putStrLn "BEER!!"
                else putStrLn "BOOM!!"
        else putStrLn "BOOM!!"

    where
        getLetters :: [C.ByteString] -> [S.Seq [C.ByteString]] -> [S.Seq [C.ByteString]]
        getLetters contents letters = if C.null . head $ contents then letters else getLetters newContents (letter:letters)
            where (letter, newContents) = getLetter empty contents

    