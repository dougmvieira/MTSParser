module MTS.LOB (combineMTSTime, shoot, rebuildLOB) where

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

lastIfSameID :: Proposal -> Proposal -> Proposal
lastIfSameID p2 p1 = if pProposalID p1 == pProposalID p2
                     then p2
                     else error "Duplicate time stamps for different proposal IDs"

buildEventMap :: V.Vector Proposal -> M.Map TimeOfDay Proposal
buildEventMap = M.fromListWith lastIfSameID . map addKey . onlyMTS . V.toList where
  addKey :: Proposal -> (TimeOfDay, Proposal)
  addKey p = (combineMTSTime (pUpdTime p) (pUpdTimeMsec p), p)
  onlyMTS :: [Proposal] -> [Proposal]
  onlyMTS = filter ((==mtsCode) . pMarketCode)

insertEvent :: Proposal -> ProposalBook -> ProposalBook
insertEvent p = M.insert (pProposalID p) p

-- |Shoot a snapshot of the visible LOB
shoot :: ProposalBook -> Snapshot
shoot ps = (compile . map pBid . filter isActive . M.elems $ ps, compile . map pAsk . filter isActive . M.elems $ ps) where
  compile :: [(Price, Quantity)] -> LOBSide
  compile = M.fromListWith (+)

-- |Shoot a snapshot of the LOB with hidden orders
shootXRay :: ProposalBook -> Snapshot 
shootXRay = undefined

rebuildLOB :: V.Vector Proposal -> M.Map TimeOfDay Snapshot
rebuildLOB = fmap shoot . snd . M.mapAccum (\acc x -> (insertEvent x acc, insertEvent x acc)) M.empty . buildEventMap
