import Control.Monad (unless)
import System.IO (isEOF)
import Text.Printf (printf)

main :: IO ()
main = do
    eof <- isEOF

    unless eof (do 
        [r, x, y] <- map read . words <$> getLine
        
        let d = sqrt (x^2 + y^2)
        let h = r-d
        let s1 = r**2 * acos((r - h)/r) - (r - h) * sqrt(2 * r * h - h**2)
        let s2 = r**2*pi - s1
        let ans = [s1, s2] :: [Double]

        if d>=r
            then putStrLn "miss"
            else printf "%f %f \n" (maximum ans) (minimum ans)

        main)
