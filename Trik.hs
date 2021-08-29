keys = [
    ('A', [('A', 'B'), ('C', 'C'), ('B', 'A')]),
    ('B', [('A', 'A'), ('B', 'C'), ('C', 'B')]),
    ('C', [('C', 'A'), ('B', 'B'), ('A', 'C')])]

f [] cp = cp
f (p:ps) cp = f ps (snd . head . filter (\(cp', np) -> cp' == p) $ snd $ head (filter (\(cp', nps) -> cp' == cp) keys))

ans 'A' = 1
ans 'B' = 2
ans 'C' = 3

main = do
    x <- getLine
    print . ans $ f x 'A'
    