import qualified Data.ByteString.Char8 as C
import qualified Data.Set as S
import Data.Maybe (fromJust)
import Text.Printf (printf)

readInt = fst . fromJust . C.readInt

entry = C.pack "entry"
exit = C.pack "exit"

main = do
    contents <- C.getContents

    let xs = init (tail (C.split '\n' contents))

    handle xs S.empty

    where
        handle []  _ = return ()
        handle (x:xs) ppl
            | action == entry = do putChar '\n'; C.putStr name; if S.member name ppl then putStr " entered (ANOMALY)" else putStr " entered"; handle xs (S.insert name ppl)
            | action == exit = do putChar '\n'; C.putStr name; if S.member name ppl then putStr " exited" else putStr " exited (ANOMALY)"; handle xs (S.delete name ppl)
            where (action:name:_) = C.split ' ' x