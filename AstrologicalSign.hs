import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

readInt = fst . fromJust . C.readInt

getSign [a, b]
    | (m == "Mar" && d >= 21) || (m == "Apr" && d <= 20) = "Aries"
    | (m == "Apr" && d >= 21) || (m == "May" && d <= 20) = "Taurus"
    | (m == "May" && d >= 21) || (m == "Jun" && d <= 21) = "Gemini"
    | (m == "Jun" && d >= 22) || (m == "Jul" && d <= 22) = "Cancer"
    | (m == "Jul" && d >= 23) || (m == "Aug" && d <= 22) = "Leo"
    | (m == "Aug" && d >= 23) || (m == "Sep" && d <= 21) = "Virgo"
    | (m == "Sep" && d >= 22) || (m == "Oct" && d <= 22) = "Libra"
    | (m == "Oct" && d >= 23) || (m == "Nov" && d <= 22) = "Scorpio"
    | (m == "Nov" && d >= 23) || (m == "Dec" && d <= 21) = "Sagittarius"
    | (m == "Dec" && d >= 22) || (m == "Jan" && d <= 20) = "Capricorn"
    | (m == "Jan" && d >= 21) || (m == "Feb" && d <= 19) = "Aquarius"
    | otherwise = "Pisces"
    where
        d = readInt a
        m = C.unpack b

main = do
    contents <- C.getContents

    let xs = tail
            $ C.lines contents

    mapM_ (putStrLn . getSign . C.words) xs

    