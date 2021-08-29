import Data.List (groupBy)
import Data.Char (isUpper, toLower, intToDigit)
import Control.Monad (mapM, when)
import Data.Bifunctor (second)
import qualified Data.Sequence as S

dividerLine = "|\n+---+---+---+---+---+---+---+---+\n"
mainLine = ["|:::", "|:::", "|:::", "|:::", "|:::", "|:::", "|:::", "|:::"]

getFile x = case x of 1 -> 'a'; 2 -> 'b'; 3 -> 'c'; 4 -> 'd'; 5 -> 'e'; 6 -> 'f'; 7 -> 'g'; 8 -> 'h'; _ -> '_'

board :: [(Char, S.Seq String)]
board = concatMap ((\[(fN, f), (fN', f')] -> [(fN, S.fromList f), (fN', S.fromList f')]) . snd)
    . tail . scanl (\(i, x') [divider, mainL] -> (i+1, [('_', divider), (getFile i, mainL)])) (1, [('z', []), ('z', [])]) 
    $ replicate 8 [[dividerLine], mainLine]

placePieces :: [String] -> [(Char, S.Seq String)] -> [(Char, S.Seq String)]
placePieces [] board = board
placePieces (p:ps) board = placePieces ps (map (\(fileName, file) -> if fileName == pfile then (fileName, changeFile file (read ind) p') else (fileName, file)) board)
    where 
        (p':pfile:ind) = p
        changeFile file ind p' = S.update (ind-1) ("|:" ++ [p'] ++ ":") file

main = do
    white <- drop 6 <$> getLine
    black <- drop 6 <$> getLine
    
    let pieces = (if length white < 2 then [] else map ((\str -> if isUpper (head str) then str else 'P':str) . tail) $ groupBy (\_ b -> b/=',') white) 
            ++ (if length black < 2 then [] else map ((\str -> if isUpper (head str) then toLower (head str) : tail str else 'p':str) . tail) $ groupBy (\_ b -> b/=',') black)
    let ans = placePieces pieces board
    let fAns = format (reverse ans) [] 0

    putStr "\n+---+---+---+---+---+---+---+---+\n"
    mapM putStr fAns

    where
        format :: [(Char, S.Seq String)] -> [String] -> Int -> [String]
        format [] f _ = f 
        format (('_', file):ps) f n = format ps f n -- yea i really made it more complicated than it needed to be
        format ((fileName, file):ps) f n = do
            let spot = S.index file 0
            let newSpot = map (\x -> if x == ':' then '.' else x) spot

            if not ((even (S.length file) && odd n) || (odd (S.length file) && even n))
                then format (if S.length file > 1 then ps++[(fileName, S.drop 1 file)] else ps) (if fileName == 'h' then newSpot:dividerLine:f else newSpot:f) (n+1)
                else format (if S.length file > 1 then ps++[(fileName, S.drop 1 file)] else ps) (if fileName == 'h' then spot:dividerLine:f else spot:f) (n+1)
