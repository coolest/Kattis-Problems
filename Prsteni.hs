import qualified Data.ByteString.Char8 as C
import Control.Monad (mapM_)
import Text.Printf (printf)

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine
getInts = map readInt . C.words <$> C.getLine

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23,
    29, 31, 37, 41, 43, 47, 53, 59, 61, 67,
    71, 73, 79, 83, 89, 97, 101, 103, 107, 109,
    113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
    173, 179, 181, 191, 193, 197, 199, 211, 223, 227,
    229, 233, 239, 241, 251, 257, 263, 269, 271, 277,
    281, 283, 293, 307, 311, 313, 317, 331, 337, 347,
    349, 353, 359, 367, 373, 379, 383, 389, 397, 401,
    409, 419, 421, 431, 433, 439, 443, 449, 457, 461,
    463, 467, 479, 487, 491, 499]

map' [] xP xS = (0, 0)
map' (x:xs) xP xS = if xP `mod` x == 0 && xS `mod` x == 0
    then (xP `div` x, xS `div` x)
    else map' xs xP xS

main = do
    getInt

    (x:xs) <- getInts

    mapM_ (handle x) xs

    where
        handle xP xS = if (xP', xS') == (0, 0)
            then printf "%d/%d\n" xP xS
            else handle xP' xS'
                where (xP', xS') = map' primes xP xS
