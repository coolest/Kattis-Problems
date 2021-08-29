
doCase = do
    line <- getLine
    let info = words line
    let n = read (head info) :: Int
    let (x, y) = (info !! 1, info !! 2)
    let (w, h) = (info !! 3, last info)
    print . last $ iterate (\m -> [[m, m], [m, -m]]) 1

main = do
    cases <- getLine >>= (\x -> return (read x :: Int))
    return $ replicate cases doCase


    -- TOO FUCKING DUMB FUCK ME I AM A FUCKING RETARD JACKASS FUCK ME
    