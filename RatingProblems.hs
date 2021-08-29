import Control.Monad (replicateM)

getVote :: IO Float
getVote = getLine >>= (\x -> return (read x :: Float))

main = do
    info <- getLine >>= \x -> return $ map (\y -> read y :: Int) (words x)
    let judges = head info
    let castedVotes = last info
    let uncastedVotes = judges-castedVotes
    voteWeight <- replicateM castedVotes getVote >>= \x -> return $ sum x

    print $ (voteWeight - (fromIntegral uncastedVotes :: Float)*3.0) / (fromIntegral judges :: Float)
    print $ (voteWeight + (fromIntegral uncastedVotes :: Float)*3.0) / (fromIntegral judges :: Float)