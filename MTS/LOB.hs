module MTS.LOB (combineMTSTime, shoot, rebuildEventBook, rebuildLOB, rebuildLOBXRay, Price, Quantity, Bid, Ask, LOBSide, AskSide, BidSide, Snapshot) where

import MTS.Types
import MTS.Decode
import Data.Fixed (Pico)
import Data.Text (Text, pack)
import Data.Time (Day,
                  TimeOfDay(..))
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

type Event = ([Proposal], Maybe Order)
type EventBook = (ProposalBook, Maybe Order)

type BondCode = Text


addPico :: TimeOfDay -> Pico -> TimeOfDay
addPico (TimeOfDay h m p) p' = TimeOfDay h m (p + p')

combineMTSTime :: MTSTime -> MTSPico -> TimeOfDay
combineMTSTime t p = addPico (getMTSTime t) (getMTSPico p)

getProposalTimeOfDay :: Proposal -> TimeOfDay
getProposalTimeOfDay p = combineMTSTime (pUpdTime p) (pUpdTimeMsec p)

oQty :: Order -> Quantity
oQty = (/1000000) . getMTSQty . oQuantity

mtsCode :: Text
mtsCode = pack "MTS"

pBid :: Proposal -> Bid
pBid p = (pBidPrice p, getMTSQty . pBidEbmQty $ p)
pAsk :: Proposal -> Ask
pAsk p = (pAskPrice p, getMTSQty . pAskEbmQty $ p)
pBidXRay :: Proposal -> Bid
pBidXRay p = (pBidPrice p, getMTSQty . pBidQty $ p)
pAskXRay :: Proposal -> Ask
pAskXRay p = (pAskPrice p, getMTSQty . pAskQty $ p)

isActive :: Proposal -> Bool
isActive p = pCheck_Logon p == 0 && pStatus p == Active

buildProposalMap :: V.Vector Proposal -> M.Map TimeOfDay [Proposal]
buildProposalMap = M.fromListWith (++) . map makeKeyVal . onlyMTS . V.toList where
  makeKeyVal :: Proposal -> (TimeOfDay, [Proposal])
  makeKeyVal p = (getProposalTimeOfDay p, [p])
  onlyMTS :: [Proposal] -> [Proposal]
  onlyMTS = filter ((==mtsCode) . pMarketCode)

buildOrderMap :: BondCode -> Day -> V.Vector Order -> M.Map TimeOfDay Order
buildOrderMap bc d = M.fromListWith err . map makeKeyVal . onlyMTS . filterDay . filterBond . V.toList where
  err = error "Found two orders with same time stamp."
  makeKeyVal :: Order -> (TimeOfDay, Order)
  makeKeyVal o = (combineMTSTime (oRefTime o) (oRefTimeMsec o), o)
  onlyMTS :: [Order] -> [Order]
  onlyMTS = filter $ (==mtsCode) . oMarketCode
  filterDay :: [Order] -> [Order]
  filterDay = filter $ (==d) . getMTSDay . oRefDate
  filterBond :: [Order] -> [Order]
  filterBond = filter $ (==bc) . oBondCode

buildEventMap :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay Event
buildEventMap ps os = M.unionWith (\(p, _) (_, o) -> (p, o)) pm om where
  pm :: M.Map TimeOfDay Event
  pm = fmap (\p -> (p, Nothing)) $ buildProposalMap ps
  om :: M.Map TimeOfDay Event
  om = fmap (\o -> ([], Just o)) $ buildOrderMap bc d os
  p :: Proposal
  p = head . V.toList $ ps
  d :: Day
  d = getMTSDay . pRefDate $ p
  bc :: BondCode
  bc = pBondCode p

filterActiveProposals :: ProposalBook -> [Proposal]
filterActiveProposals = filter isActive . M.elems

aggregQties :: [(Price, Quantity)] -> LOBSide
aggregQties = M.fromListWith (+)

-- |Shoot a snapshot of the visible LOB
shoot :: EventBook -> Snapshot
shoot eb = (aggregQties . map pBid . filterActiveProposals . fst $ eb,
            aggregQties . map pAsk . filterActiveProposals . fst $ eb)

-- |Shoot a snapshot of the LOB with hidden orders
shootXRay :: EventBook -> Snapshot
shootXRay eb = (aggregQties . map pBidXRay . filterActiveProposals . fst $ eb,
                aggregQties . map pAskXRay . filterActiveProposals . fst $ eb)

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

type PriceTimeOrdList = [((Price, TimeOfDay), Quantity)]

toPriceTimeOrdList :: Verb -> [Proposal] -> PriceTimeOrdList
toPriceTimeOrdList v = M.toAscList . M.fromList . map (\p -> ((pPrice p, getProposalTimeOfDay p), pQty p)) . filter isActive where
   pPrice :: Proposal -> Price
   pPrice = if v == Buy then pAskPrice else negate . pBidPrice
   pQty :: Proposal -> Quantity
   pQty = if v == Buy then getMTSQty . pAskQty else getMTSQty . pBidQty

aggressProposals :: Price -> (Quantity, PriceTimeOrdList, PriceTimeOrdList) -> (Quantity, PriceTimeOrdList, PriceTimeOrdList)
aggressProposals _  (q, acc, []) = (q, acc, [])
aggressProposals p' input@(q', acc, ((p, t), q):xs)
   | p <= p' && q <= q' = aggressProposals p' (q' - q, ((p, t), 0):acc, xs)
   | p <= p'            = (0, ((p, t), q - q'):acc, xs)
   | otherwise          = input

fillAndKill :: Price -> Quantity -> PriceTimeOrdList -> PriceTimeOrdList
fillAndKill pc q pps = let (_, pps', _) = aggressProposals pc (q, [], pps) in pps'

resolveTrade :: OrderType -> Order -> ProposalBook -> PriceTimeOrdList
resolveTrade FillAndKill o = fillAndKill p (oQty o) . toPriceTimeOrdList (oVerb o) . M.elems where
   p :: Price
   p = (if oVerb o == Buy then id else negate) $ oPrice o
resolveTrade AllOrNone _ = undefined

head' t [] = error $ "fetchByPrice failed at " ++ show t
head' _ l = head l

fetchByPriceTime :: Verb -> Price -> TimeOfDay -> [Proposal] -> Proposal
fetchByPriceTime v pc t = head' t . filter (\p -> (getProposalTimeOfDay p) == t && (pPrice p) == pc) where
   pPrice :: Proposal -> Price
   pPrice = if v == Buy then pAskPrice else negate . pBidPrice

proposalAggressionCheck :: Verb -> Quantity -> Proposal -> Bool
proposalAggressionCheck _    0 = not . isActive
proposalAggressionCheck Buy  q = (== q) . getMTSQty . pAskQty
proposalAggressionCheck Sell q = (== q) . getMTSQty . pBidQty

proposalAggressionPairing :: Verb -> PriceTimeOrdList -> [Proposal] -> [(Quantity, Proposal)]
proposalAggressionPairing _ [] _ = []
proposalAggressionPairing v (((p, t), q):ptol) ps = (q, fetchByPriceTime v p t ps):proposalAggressionPairing v ptol ps

checkAggression :: Verb -> PriceTimeOrdList -> [Proposal] -> Bool
checkAggression v ptol ps = (length ptol == length ps
   && (all (uncurry $ proposalAggressionCheck v) $ proposalAggressionPairing v ptol ps))

matchingEngine :: [Proposal] -> Order -> ProposalBook -> ProposalBook
matchingEngine ps o pb = if   checkAggression (oVerb o) aggression ps
                         then M.union (makeProposalBook ps) pb
             else error $ "New state of LOB is not consistent with market order" ++ show o ++ show aggression where
             aggression :: PriceTimeOrdList
             aggression = (resolveTrade (oOrderType o) o pb)

rebuildEventBook :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay EventBook
rebuildEventBook ps os = M.fromDescList . init . M.foldlWithKey accFun acc0 $ buildEventMap ps os where
   updateEBook :: Event -> EventBook -> EventBook
   updateEBook (ps, Nothing) (pb, o) = (M.union (makeProposalBook ps) pb, o)
   updateEBook (ps, Just o) (pb, _) = (matchingEngine ps o pb, Just o)
   accFun :: [(TimeOfDay, EventBook)] -> TimeOfDay -> Event -> [(TimeOfDay, EventBook)]
   accFun acc@((_, eb):xs) k e = (k, updateEBook e eb):acc
   acc0 :: [(TimeOfDay, EventBook)]
   acc0 = [(TimeOfDay 0 0 0, (M.empty, Nothing))]

rebuildLOB :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay Snapshot
rebuildLOB ps os = fmap shoot $ rebuildEventBook ps os

rebuildLOBXRay :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay Snapshot
rebuildLOBXRay ps os = fmap shootXRay $ rebuildEventBook ps os
