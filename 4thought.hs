import qualified Control.Monad as Monad (when)

_max = 4 * 4 * 4 * 4
_min = 4 - 4 - 4 - 4

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort more ++ [x] ++ sort less
    where
        less = filter (<=x) xs
        more = filter (>x) xs

options :: [Int -> Int -> Int]
options = [(+), (-), div, (*)]
optionsStr = [" + 4", " - 4", " / 4", " * 4"]

createOptions :: Int -> Int -> Int -> (Int, String)
createOptions first second third = (key, value)
    where
        o = sort [first, second, third]
        o1 = options !! head o
        o2 = options !! (o !! 2)
        o3 = options !! last o
            
        key = ((4 `o1` 4) `o2` 4) `o3` 4
        value = "4" ++ (optionsStr !! first) ++ (optionsStr !! second) ++ (optionsStr !! third)

answerList :: [(Int, String)]
answerList = createOptions <$> [0..3] <*> [0..3] <*> [0..3]

getValueFromAnswerList :: Int -> String
getValueFromAnswerList key = foldr (\(k, v) acc -> if k == key then v else acc) "no solution" answerList


main = do
    casesStr <- getLine
    let loopHandle n = do
        Monad.when (n > 0)(
            do 
                keyStr <- getLine
                let key = read keyStr :: Int
                if key > _max || key < _min
                    then putStrLn "no solution"
                    else (putStrLn . getValueFromAnswerList) key
                    
                loopHandle (n-1))
        in loopHandle (read casesStr :: Int)
        
