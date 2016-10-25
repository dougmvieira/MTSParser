import MTS.Decode
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

printOrThrow :: Either String String -> IO ()
printOrThrow (Left s) = error s
printOrThrow (Right s) = putStrLn s

headPair :: (M.Map k v, M.Map k' v') -> ((k, v), (k', v'))
headPair (xs, ys) = (head $ M.toAscList xs, head $ M.toAscList ys)

showPair :: (Show a, Show b) => (a, b) -> [String]
showPair (x, y) = [show x, show y]

unpackEither :: Either String a -> a
unpackEither (Right a) = a
unpackEither (Left s) = error s

testOrder = do
  contents <- B.readFile "orders_nov15.txt"
  printOrThrow . fmap (show . V.head) . parseOrder $ contents

testProposal = do
  contents <- B.readFile "IT0004923998.txt"
  printOrThrow . fmap (show . V.head) . parseProposal $ contents

testFill = do
  contents <- B.readFile "fills_nov15.txt"
  printOrThrow . fmap (show . V.head) . parseFill $ contents

main = do
  testOrder
  testProposal
  testFill
