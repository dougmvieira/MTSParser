module MTS.LOB (combineMTSTime, shoot, rebuildLOB, Price, Quantity, Bid, Ask, LOBSide, AskSide, BidSide, Snapshot) where

import MTS.Types
import MTS.Decode
import Data.Fixed (Pico)
import Data.Text (Text, pack)
import Data.Time (TimeOfDay(..))
import qualified Data.Vector as V
import qualified Data.Map as M


type Price = Double
type Quantity = Double
type Bid = (Price, Quantity)
type Ask = (Price, Quantity)

type ProposalID = Int
type ProposalBook = M.Map ProposalID Proposal

type LOBSide = M.Map Price Quantity
type AskSide = LOBSide
type BidSide = LOBSide
type Snapshot = (BidSide, AskSide)

addPico :: TimeOfDay -> Pico -> TimeOfDay
addPico (TimeOfDay h m p) p' = TimeOfDay h m (p + p')

combineMTSTime :: MTSTime -> MTSPico -> TimeOfDay
combineMTSTime t p = addPico (getMTSTime t) (getMTSPico p)

mtsCode :: Text
mtsCode = pack "MTS"

pBid :: Proposal -> Bid
pBid p = (pBidPrice p, getMTSQty . pBidQty $ p)
pAsk :: Proposal -> Ask
pAsk p = (pAskPrice p, getMTSQty . pAskQty $ p)

isActive :: Proposal -> Bool
isActive p = pCheck_Logon p == 0 && pStatus p == Active

buildEventMap :: V.Vector Proposal -> M.Map TimeOfDay [Proposal]
buildEventMap = M.fromListWith (++) . map makeKeyVal . onlyMTS . V.toList where
  makeKeyVal :: Proposal -> (TimeOfDay, [Proposal])
  makeKeyVal p = (combineMTSTime (pUpdTime p) (pUpdTimeMsec p), [p])
  onlyMTS :: [Proposal] -> [Proposal]
  onlyMTS = filter ((==mtsCode) . pMarketCode)

-- |Shoot a snapshot of the visible LOB
shoot :: ProposalBook -> Snapshot
shoot ps = (compile . map pBid . filter isActive . M.elems $ ps, compile . map pAsk . filter isActive . M.elems $ ps) where
  compile :: [(Price, Quantity)] -> LOBSide
  compile = M.fromListWith (+)

-- |Shoot a snapshot of the LOB with hidden orders
shootXRay :: ProposalBook -> Snapshot 
shootXRay = undefined

hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate [] = False
hasDuplicate (x:xs) = x `elem` xs && hasDuplicate xs

hasDuplicatePrices :: ProposalBook -> Bool
hasDuplicatePrices pb = let ((bs, as), _) = M.mapAccum (\(bs', as') p -> ((pBidPrice p:bs', pAskPrice p:as'), ())) ([],[]) pb
                        in hasDuplicate bs || hasDuplicate as

makeProposalBook :: [Proposal] -> ProposalBook
makeProposalBook = sanityCheck . M.fromList . map (\p -> (pProposalID p, p)) where
   sanityCheck :: ProposalBook -> ProposalBook
   sanityCheck pb = if hasDuplicatePrices pb
		    then error $ "Invalid duplicate time proposals: " ++ show pb
		    else pb

rebuildLOB :: V.Vector Proposal -> M.Map TimeOfDay Snapshot
rebuildLOB = fmap shoot . snd . M.mapAccum (\acc x -> (updatePBook acc x, updatePBook acc x)) M.empty . buildEventMap where
   updatePBook acc = M.union acc . makeProposalBook
