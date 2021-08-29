import qualified Data.ByteString.Char8 as C

main = do
    C.putStr . C.init =<< C.getContents
    putChar 'O'
