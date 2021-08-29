import qualified Data.ByteString.Char8 as C
import Data.Char (isSpace)
import Data.List (sortOn)
import Control.Monad (mapM)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

main = do
    str <- C.filter (not . isSpace) <$> C.getLine

    let wordLength = C.length str `div` 3
    let words = map (sortOn ((wordLength -) . C.length) . C.group . C.sort) . C.transpose $ f str wordLength []

    mapM (putChar . C.head . head) words

    where
        f w l a = if C.null w 
            then a
            else f d l (t:a)
            where 
                d = C.drop l w
                t = C.take l w