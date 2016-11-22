import MTS.Query
import MTS.Decode
import MTS.Types
import MTS.LOB
import MTS.Encode
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

proposalExamples :: IO B.ByteString
proposalExamples = B.readFile "IT0004923998.txt"

proposalExamples' :: IO B.ByteString
proposalExamples' = B.readFile "IT0004019581.txt"

fillExamples :: IO B.ByteString
fillExamples = B.readFile "fills_nov15.txt"

orderExamples :: IO B.ByteString
orderExamples = B.readFile "orders_nov15.txt"
 
parseFillToList :: B.ByteString -> [Fill]
parseFillToList fstext = let (Right fs) = parseFill fstext in V.toList fs

parseOrderToList :: B.ByteString -> [Order]
parseOrderToList ostext = let (Right os) = parseOrder ostext in V.toList os

parseProposalToList :: B.ByteString -> [Proposal]
parseProposalToList pstext = let (Right ps) = parseProposal pstext in V.toList ps

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
  contents <- orderExamples
  printOrThrow . fmap (show . V.head) . parseOrder $ contents

testProposal = do
  contents <- proposalExamples
  printOrThrow . fmap (show . V.head) . parseProposal $ contents

testFill = do
  contents <- fillExamples
  printOrThrow . fmap (show . V.head) . parseFill $ contents

testRebuildLOB = do
  psText <- proposalExamples
  osText <- orderExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  B.writeFile "data.txt" . encodeDepth3LOB $ rebuildLOB ps os

main = do
  testOrder
  testProposal
  testFill
