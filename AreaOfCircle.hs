import qualified Data.ByteString.Char8 as C
import Control.Monad (unless)
import Text.Printf

main = do
    [r, dT, dI] <- map read . words <$> getLine
    unless (r == 0 && dT == 0 && dI == 0) $ do
        let area = (r^2) * pi
        let guess = (r*2)^2 * (dI / dT)
        printf "%.6f %.6f\n" (area :: Float) (guess :: Float)
        main