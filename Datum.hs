import qualified Data.ByteString.Char8 as C

strip (Just a) = a

readInt = fst. strip . C.readInt
getInts = map readInt . C.words <$> C.getLine

names = ["Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"]
days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

main = do
    [d, m] <- getInts
    let daysPassedFirst = sum (take (m-1) days) + (d-1)
    putStrLn (names !! (daysPassedFirst `rem` 7))