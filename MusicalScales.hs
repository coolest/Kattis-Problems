import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S
import Data.Char (isSpace)
import Control.Monad (mapM_)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

notes = S.fromList ["A" , "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A" , "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A" , "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A" , "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]
getMajorOfNote i = [S.index notes i, 
    S.index notes (i+2), 
    S.index notes (i+4),
    S.index notes (i+5),
    S.index notes (i+7),
    S.index notes (i+9),
    S.index notes (i+11),
    S.index notes (i+12)
    ]


main = do
    getLine

    n <- words <$> getLine
    
    let ans = check n 0 []
    if null ans
        then putStrLn "none"
        else do
            mapM_ (\x -> putStr (x ++ " ")) (init ans)
            putStr (last ans)

    where
        check n i a
            | i == 12 = reverse a
            | isOk = check n (i+1) (S.index notes i :a)
            | otherwise = check n (i+1) a
            where 
                major = getMajorOfNote i
                isOk = foldl (\a x -> if not a then a else x `elem` major) True n