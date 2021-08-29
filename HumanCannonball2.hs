import Control.Monad (replicateM)

g = 9.81

readInt x = read x :: Int

main = do
    n <- readInt <$> getLine
    replicateM n ( do
        [v0, theta, x1, h1, h2] <- map (\x -> read x :: Float) . words <$> getLine
        let t = x1 / (v0*cos (theta*pi/180))
        let y = (v0*t*sin (theta*pi/180)) - (0.5*g*t**2)
        if y-1 >= h1 && y+1 <= h2
            then putStrLn "Safe"
            else putStrLn "Not Safe"
        )