import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)
import Text.Printf

strip (Just a) = a

readInt = fst. strip . C.readInt
getInt = readInt <$> C.getLine

postStudiesCheck [y1, y2, y3, y4, _, m1, m2, _, d1, d2] = read [y1, y2, y3, y4] >= 2010
dateOfBirthCheck [y1, y2, y3, y4, _, m1, m2, _, d1, d2] = read [y1, y2, y3, y4] >= 1991
coursesCheck n = read n >= 41

main = do
    n <- getInt

    replicateM_ n handle

    where
        handle = do
            [name, dOS, dOB, nC] <- words <$> getLine
            case (postStudiesCheck dOS, dateOfBirthCheck dOB, coursesCheck nC) of
                (False, False, False) -> printf "%s %s\n" name "coach petitions"
                (True, _, _) -> printf "%s %s\n" name "eligible"
                (_, True, _) -> printf "%s %s\n" name "eligible"
                (False, False, True) -> printf "%s %s\n" name "ineligible"

