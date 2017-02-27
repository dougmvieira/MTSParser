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
  fsText <- fillExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  (lob, log, log') <- return $ rebuildLOBWithLog ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile "data.txt" $ encodeDepth3LOB lob

testRebuildLOBWithTrades = do
  psText <- proposalExamples'
  osText <- orderExamples
  fsText <- fillExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  (lob, log, log') <- return $ rebuildLOBWithLog ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile "dataWithTrades.txt" $ encodeDepth3LOB lob

testRebuildOn :: V.Vector Order -> String -> IO ()
testRebuildOn os bondname = do
  putStrLn $ "Rebuilding bond " ++ bondname
  psText <- B.readFile $ bondname ++ ".txt"
  (Right ps) <- return . parseProposal $ psText
  B.writeFile (bondname ++ "rebuilt.txt") . encodeDepth3LOB $ rebuildLOB ps os

{-bondnames = ["IT0000366655",
             "IT0001086567",
             "IT0001174611",
             "IT0001278511",
             "IT0001444378",
             "IT0003242747",
             "IT0003256820",
             "IT0003493258",
             "IT0003535157",-}
bondnames = ["IT0003644769",
             "IT0003745541",
             "IT0003934657",
             "IT0004009673",
             "IT0004019581",
             "IT0004085210",
             "IT0004164775",
             "IT0004243512",
             "IT0004273493",
             "IT0004286966",
             "IT0004356843",
             "IT0004361041",
             "IT0004380546",
             "IT0004423957"]

testRebuildOnAllBTPs = do
  osText <- orderExamples
  (Right os) <- return . parseOrder $ osText
  mapM_ (testRebuildOn os) bondnames

main = do
  testOrder
  testProposal
  testFill
