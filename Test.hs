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

testRebuildLvl1LOB = do
  psText <- proposalExamples'
  osText <- orderExamples
  fsText <- fillExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  (lob, log, log') <- return $ rebuildRichLOB ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile "lvl1LOB.txt" $ encodeLvl1LOB lob

testRebuildTopOfBook = do
  psText <- proposalExamples'
  osText <- orderExamples
  fsText <- fillExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  (lob, log, log') <- return $ rebuildLOBWithLog ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile "topOfBook.txt" $ encodeTopOfBookLOB lob

testRebuildRegGridLOB = do
  psText <- proposalExamples'
  osText <- orderExamples
  fsText <- fillExamples
  (Right ps) <- return . parseProposal $ psText
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  (lob, log, log') <- return $ rebuildRichLOB ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile "regGridLOB.txt" $ encodeRegGridLOB (1 / (24 * 60 * 60)) $ fmap (\(x, y, _) -> (x, y)) <$> lob

testRebuildOn :: V.Vector Order -> V.Vector Fill -> String -> IO ()
testRebuildOn os fs bondname = do
  putStrLn $ "Rebuilding bond " ++ bondname
  psText <- B.readFile $ bondname ++ ".txt"
  (Right ps) <- return . parseProposal $ psText
  (lob, log, log') <- return $ rebuildRichLOB ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile ("./rebuilt/" ++ bondname ++ ".txt") $ encodeLvl1LOB lob

secsInADay :: Rational
secsInADay = 24*60*60

testRebuildRegGridOn :: V.Vector Order -> V.Vector Fill -> String -> IO ()
testRebuildRegGridOn os fs bondname = do
  putStrLn $ "Rebuilding bond " ++ bondname
  psText <- B.readFile $ bondname ++ ".txt"
  (Right ps) <- return . parseProposal $ psText
  (lob, log, log') <- return $ rebuildRichLOB ps os fs
  putStrLn log
  putStrLn log'
  B.writeFile ("./regGrid/1ms/"   ++ bondname ++ ".txt") $ encodeRegGridLOB (1/(1000*secsInADay)) $ fmap (\(x, y, _) -> (x, y)) <$> lob
  B.writeFile ("./regGrid/10ms/"  ++ bondname ++ ".txt") $ encodeRegGridLOB (1/( 100*secsInADay)) $ fmap (\(x, y, _) -> (x, y)) <$> lob
  B.writeFile ("./regGrid/100ms/" ++ bondname ++ ".txt") $ encodeRegGridLOB (1/(  10*secsInADay)) $ fmap (\(x, y, _) -> (x, y)) <$> lob
  B.writeFile ("./regGrid/1s/"    ++ bondname ++ ".txt") $ encodeRegGridLOB (1/      secsInADay ) $ fmap (\(x, y, _) -> (x, y)) <$> lob

bondnames = ["IT0000366655",
             "IT0001086567",
             "IT0001174611",
             "IT0001278511",
             "IT0001444378",
             "IT0003242747",
             "IT0003256820",
             "IT0003493258",
             "IT0003535157",
             "IT0003644769",
             "IT0003934657",
             "IT0004009673",
             "IT0004019581",
             "IT0004164775",
             "IT0004273493",
             "IT0004286966",
             "IT0004356843",
             "IT0004361041",
             "IT0004423957",
             "IT0004489610",
             "IT0004513641",
             "IT0004532559",
             "IT0004536949",
             "IT0004594930",
             "IT0004634132",
             "IT0004644735",
             "IT0004695075",
             "IT0004712748",
             "IT0004759673",
             "IT0004761950",
             "IT0004793474",
             "IT0004801541",
             "IT0004820426",
             "IT0004848831",
             "IT0004867070",
             "IT0004880990",
             "IT0004889033",
             "IT0004898034",
             "IT0004907843",
             "IT0004917792",
             "IT0004923998",
             "IT0004953417",
             "IT0004957574",
             "IT0004960826",
             "IT0004966401",
             "IT0004987191",
             "IT0004992308",
             "IT0005001547",
             "IT0005023459",
             "IT0005024234",
             "IT0005028003",
             "IT0005030504",
             "IT0005045270",
             "IT0005058463",
             "IT0005069395",
             "IT0005083057",
             "IT0005086886",
             "IT0005090318",
             "IT0005094088",
             "IT0005106049",
             "IT0005107708",
             "IT0005127086",
             "IT0005135840",
             "IT0005139099",
             "IT0005142143"]

testRebuildOnAllBTPs = do
  osText <- orderExamples
  fsText <- fillExamples
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  mapM_ (testRebuildOn os fs) bondnames

testRebuildRegGridOnAllBTPs = do
  osText <- orderExamples
  fsText <- fillExamples
  (Right os) <- return . parseOrder $ osText
  (Right fs) <- return . parseFill $ fsText
  mapM_ (testRebuildRegGridOn os fs) bondnames

main = testRebuildRegGridOnAllBTPs
