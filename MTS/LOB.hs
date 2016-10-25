module MTS.LOB (prepareLOB) where

import MTS.Types
import MTS.Decode
import Data.Fixed (Pico)
import Data.Text (Text, pack)
import Data.Time (TimeOfDay(..))
import qualified Data.Vector as V
import qualified Data.Map as M


type Price = Double
type Quantity = Double
type HiddenQty = Double
type Ask = (Price, Quantity)
type Bid = (Price, Quantity)
type ProposalID = Int

data MTSLimitOrder = MTSLimitOrder ProposalID MTSStatus (Maybe Ask) (Maybe Bid) deriving Show
data MTSMarketOrder = MTSMarketOrder (Maybe Ask) (Maybe Bid) deriving Show
type MTSLOB = M.Map TimeOfDay MTSLimitOrder
type Expiry = ProposalID
type Expiries = M.Map TimeOfDay Expiry

addPico :: TimeOfDay -> Pico -> TimeOfDay
addPico (TimeOfDay h m p) p' = TimeOfDay h m (p + p')

combineMTSTime :: MTSTime -> MTSPico -> TimeOfDay
combineMTSTime t p = addPico (getMTSTime t) (getMTSPico p)

pAskQty' :: Proposal -> Quantity
pAskQty' = getMTSQty . pAskQty

pBidQty' :: Proposal -> Quantity
pBidQty' = getMTSQty . pBidQty

pAskBid :: Proposal -> (Maybe Ask, Maybe Bid)
pAskBid p
  | pQuotingSide p == BothSides = (Just (pAskPrice p, pAskQty' p), Just (pBidPrice p, pBidQty' p))
  | pQuotingSide p == AskOnly = (Just (pAskPrice p, pAskQty' p), Nothing)
  | pQuotingSide p == BidOnly = (Nothing, Just (pBidPrice p, pBidQty' p))

proposalToLimitOrder :: Proposal -> ((TimeOfDay, MTSLimitOrder), (TimeOfDay, Expiry))
proposalToLimitOrder p = ( ( combineMTSTime (pUpdTime p) (pUpdTimeMsec p)
                           , MTSLimitOrder (pProposalID p) (pStatus p) ask bid )
			 , ( combineMTSTime (pEndTime p) (pEndTimeMsec p)
			   , pProposalID p ) ) where (ask, bid) = pAskBid p

mtsCode :: Text
mtsCode = pack "MTS"

prepareLOB :: V.Vector Proposal -> (MTSLOB, Expiries)
prepareLOB ps = ( M.fromList . fst $ ordersList
                , M.fromList . snd $ ordersList ) where
	          ordersList :: ([(TimeOfDay, MTSLimitOrder)], [(TimeOfDay, Expiry)])
	          ordersList = unzip . map proposalToLimitOrder . filter ((==mtsCode) . pMarketCode) . V.toList $ ps
