import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst. fromJust . C.readInt

solve :: Int -> [(Int, Bool)] -> Bool
solve x (a1:a2:_)
    | x <= 0 = not . snd $ a1
    | otherwise = solve (x - fst a1) ([a2, a1])

main = do
    contents <- C.getContents

    let a:b:_ = C.split '\n' contents
    let ts = map readInt (C.split ' ' a)
    let xs = map readInt (C.split ' ' b)

    handle xs ts

    where
        handle [] _ = return ()
        handle (x:xs) ts@[a1, a2, b1, b2] = do
            let safe1 = solve x [(a1, False), (a2, True)]
            let safe2 = solve x [(b1, False), (b2, True)]

            case (safe1, safe2) of
                (True, True) -> putStrLn "none"
                (False, True) -> putStrLn "one"
                (True, False) -> putStrLn "one"
                (False, False) -> putStrLn "both"

            handle xs ts