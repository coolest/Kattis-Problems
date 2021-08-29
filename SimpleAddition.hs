main = do
    contents <- getContents

    let xs = map read
            $ lines contents

    print . sum $ xs