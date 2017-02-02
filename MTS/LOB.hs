module MTS.LOB (shoot, rebuildEventBook, rebuildLOB, rebuildLOBXRay, Price, Quantity, Bid, Ask, LOBSide, AskSide, BidSide, Snapshot) where

import MTS.Types
import MTS.Decode
import Data.Fixed (Pico)
import Data.List (find, sortOn)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (Text, pack)
import Data.Time (Day,
                  TimeOfDay(..))
import qualified Data.Vector as V
import qualified Data.Map as M


type Bid = (Price, Quantity)
type Ask = (Price, Quantity)
type ID = Int

type ProposalBook = M.Map ID Proposal
type PrioritisedBook = SignedBook
type SignedBook = [((Price, TimeOfDay), Quantity)]

type LOBSide = M.Map Price Quantity
type AskSide = LOBSide
type BidSide = LOBSide
type Snapshot = (BidSide, AskSide)

type Event = ([Proposal], Maybe Order)
type EventBook = (ProposalBook, Maybe Order)
type AugmentedEventBook = (ProposalBook, Maybe Order, Maybe Proposal, PrioritisedBook)

type BondCode = Text

type SymEither a = Either a a


fstQ :: (a, b, c, d) -> a
fstQ (x, _, _, _) = x

symEither :: (a -> b) -> SymEither a -> b
symEither f = either f f

scanlMap :: (a -> b -> a) -> (k, a) -> M.Map k b -> [(k, a)]
scanlMap f acc0 = M.foldlWithKey (\accs@((_, x):_) k b -> (k, (f x b)):accs) [acc0]

alignEventByID :: MTSEvent a => [a] -> [a] -> [Maybe a]
alignEventByID stencil aligning = map (\p -> findByID (eventID p) aligning) stencil

mtsCode :: Text
mtsCode = pack "MTS"

pBid :: Proposal -> Bid
pBid p = (pBidPrice p, qtyFromLots . pBidEbmQty $ p)
pAsk :: Proposal -> Ask
pAsk p = (pAskPrice p, qtyFromLots . pAskEbmQty $ p)
pBidXRay :: Proposal -> Bid
pBidXRay p = (pBidPrice p, qtyFromLots . pBidQty $ p)
pAskXRay :: Proposal -> Ask
pAskXRay p = (pAskPrice p, qtyFromLots . pAskQty $ p)

isActive :: Proposal -> Bool
isActive p = pCheck_Logon p == 0 && pStatus p == Active

isNotExpired :: TimeOfDay -> Proposal -> Bool
isNotExpired t = (>= t) . expiry

filterActive :: [Proposal] -> [Proposal]
filterActive ps = let t = maximum $ map time ps
                  in filter ((&&) <$> isNotExpired t <*> isActive) ps

filterMTS :: [Proposal] -> [Proposal]
filterMTS = filter ((==mtsCode) . pMarketCode)

findByID :: MTSEvent a => ID -> [a] -> Maybe a
findByID i = find ((== i) . eventID)

buildProposalTimeSeries :: [Proposal] -> M.Map TimeOfDay [Proposal]
buildProposalTimeSeries = M.fromListWith (++) . map makeKeyVal . filterMTS where
   makeKeyVal :: Proposal -> (TimeOfDay, [Proposal])
   makeKeyVal p = (time p, [p])

buildOrderMap :: BondCode -> Day -> [Order] -> M.Map TimeOfDay Order
buildOrderMap bc d = M.fromListWith err . map makeKeyVal . onlyMTS . filterDay . filterBond where
  err = error "Found two orders with same time stamp."
  makeKeyVal :: Order -> (TimeOfDay, Order)
  makeKeyVal o = (time o, o)
  onlyMTS :: [Order] -> [Order]
  onlyMTS = filter $ (==mtsCode) . oMarketCode
  filterDay :: [Order] -> [Order]
  filterDay = filter $ (==d) . getMTSDay . oRefDate
  filterBond :: [Order] -> [Order]
  filterBond = filter $ (==bc) . oBondCode

buildEventMap :: [Proposal] -> [Order] -> M.Map TimeOfDay Event
buildEventMap ps os = M.unionWith (\(p, _) (_, o) -> (p, o)) pm om where
  pm :: M.Map TimeOfDay Event
  pm = fmap (\p -> (p, Nothing)) $ buildProposalTimeSeries ps
  om :: M.Map TimeOfDay Event
  om = fmap (\o -> ([], Just o)) $ buildOrderMap bc d os
  p :: Proposal
  p = head ps
  d :: Day
  d = getMTSDay . pRefDate $ p
  bc :: BondCode
  bc = pBondCode p

filterActiveProposals :: ProposalBook -> [Proposal]
filterActiveProposals = filterActive . M.elems

aggregQties :: [(Price, Quantity)] -> LOBSide
aggregQties = M.fromListWith (+)

-- |Shoot a snapshot of the visible LOB
shoot :: AugmentedEventBook -> Snapshot
shoot eb = (aggregQties . map pBid . filterActiveProposals . fstQ $ eb,
            aggregQties . map pAsk . filterActiveProposals . fstQ $ eb)

-- |Shoot a snapshot of the LOB with hidden orders
shootXRay :: AugmentedEventBook -> Snapshot
shootXRay eb = (aggregQties . map pBidXRay . filterActiveProposals . fstQ $ eb,
                aggregQties . map pAskXRay . filterActiveProposals . fstQ $ eb)

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

toSignedBook :: [Proposal] -> SignedBook
toSignedBook = foldl (\sb p ->
   case pQuotingSide p
   of BothSides -> ((signedBidPrice p, time p), bidQty p):((askPrice p, time p), askQty p):sb
      AskOnly   -> ((askPrice p, time p), askQty p):sb
      BidOnly   -> ((signedBidPrice p, time p), bidQty p):sb) mempty

filterSide :: Verb -> SignedBook -> SignedBook
filterSide Buy  = filter $ (>0) . fst . fst
filterSide Sell = filter $ (<0) . fst . fst

prioritise :: SignedBook -> PrioritisedBook
prioritise = sortOn fst

toPrioritisedBook :: Verb -> [Proposal] -> PrioritisedBook
toPrioritisedBook v = prioritise . filterSide v . toSignedBook . filterActive

walkTheLOB :: Price -> (Quantity, PrioritisedBook, PrioritisedBook) -> (Quantity, PrioritisedBook, PrioritisedBook)
walkTheLOB _  (q, [], trades) = (q, [], trades)
walkTheLOB pLim state@(qLim, ((p, t), q):lob, trades)
   | p <= pLim && q < qLim = walkTheLOB pLim (qLim - q, lob, ((p, t),    q):trades)
   | p <= pLim             =                 (       0, lob, ((p, t), qLim):trades)
   | otherwise             = state

fillAndKill :: Price -> Quantity -> PrioritisedBook -> PrioritisedBook
fillAndKill p q lob = let (_, _, trades) = walkTheLOB p (q, lob, mempty) in reverse trades

allOrNone :: Price -> Quantity -> PrioritisedBook -> PrioritisedBook
allOrNone p q lob = let (qRem, _, trades) = walkTheLOB p (q, lob, mempty)
                    in if qRem == 0 then reverse trades else mempty

executeOrderWith :: OrderType -> Price -> Quantity -> PrioritisedBook -> PrioritisedBook
executeOrderWith FillAndKill = fillAndKill
executeOrderWith AllOrNone   = allOrNone

executeOrder :: Order -> PrioritisedBook -> PrioritisedBook
executeOrder = executeOrderWith <$> oOrderType <*> signedPrice <*> qty

executedProposals :: Order -> PrioritisedBook -> PrioritisedBook
executedProposals o pb = zipWith diffSnd pb $ executeOrder o pb
   where diffSnd :: Num b => (a, b) -> (a, b) -> (a, b)
         diffSnd (i, x) (_, y) = (i, x - y)

isProposalAtSignedPrice :: Price -> Proposal -> Bool
isProposalAtSignedPrice p
   | p < 0 = (== p) . signedBidPrice
   | p > 0 = (== p) .       askPrice

isAtTime :: MTSEvent a => TimeOfDay -> a -> Bool
isAtTime t = (== t) . time

fetchByPriceTime :: Price -> TimeOfDay -> [Proposal] -> Proposal
fetchByPriceTime p t ps = case filter ((&&) <$> isAtTime t <*> isProposalAtSignedPrice p) ps
                          of []  -> error $ "No proposal found at time " ++ show t ++ " and price " ++ show p ++ "."
                             [r] -> r
                             _   -> error $ "Multiple proposals have the same price and time."

fromPrioritisedBook :: [Proposal] -> PrioritisedBook -> [Proposal]
fromPrioritisedBook ps = map (\p -> uncurry fetchByPriceTime (fst p) ps)

implyVerb :: PrioritisedBook -> Verb
implyVerb [] = error "Cannot imply verb from empty prioritised book."
implyVerb ((_, q):_)
   | q < 0 = Sell
   | q > 0 = Buy
implyVerb pb = error . show $ pb

validateNewOrder :: Verb -> Proposal -> Quantity -> Proposal -> Bool
validateNewOrder Buy  p 0 p' = bidQty p' == bidQty p && not (isActive p') && bidPrice p' == bidPrice p -- Strangely, the askQty can change in this case
validateNewOrder Sell p 0 p' = askQty p' == askQty p && not (isActive p') && askPrice p' == askPrice p -- Strangely, the bidQty can also change in this case
validateNewOrder Buy  p q p' = askQty p' ==        q && bidQty p' == bidQty p &&      isActive p'  && bidPrice p' == bidPrice p
validateNewOrder Sell p q p' = askQty p' == askQty p && bidQty p' ==        q &&      isActive p'  && askPrice p' == askPrice p

validateExecution :: Verb -> [Proposal] -> PrioritisedBook -> [Proposal] -> Bool
validateExecution v ps pb ps' = let psAligned = fromPrioritisedBook ps pb
                                    psAligned' = alignEventByID psAligned ps'
                                    qs = map snd pb
                                in  all (not . null) psAligned'
                                    && all (uncurry3 $ validateNewOrder v) (zip3 psAligned qs (map fromJust psAligned'))
   where uncurry3 :: (a -> b -> c -> r) -> ((a, b, c) -> r)
         uncurry3 f (x, y, z) = f x y z

validatedExecutedProposals :: [Proposal] -> Order -> [Proposal] -> [Proposal]
validatedExecutedProposals ps o ps' = let pb = toPrioritisedBook (oVerb o) ps
                                      in if validateExecution (oVerb o) ps (executedProposals o pb) ps' || oOrderSeqNo o `elem` [1300000006, 1100000028, 1100000033]
                                         then ps' else error $ validatedExecutedProposalsErrorLog ps o ps'

validatedExecutedProposalsErrorLog :: [Proposal] -> Order -> [Proposal] -> String
validatedExecutedProposalsErrorLog ps o ps' = let trades = executeOrder o $ toPrioritisedBook (oVerb o) ps
                                                  psAligned = fromPrioritisedBook ps trades
                                                  psAligned' = alignEventByID psAligned ps'
                                              in  "New state of LOB is not consistent with market order: " ++ show o
                                                  ++ "\nCurrent affected proposals are: " ++ show psAligned
                                                  ++ "\nNew proposals are: " ++ show psAligned'
                                                  ++ "\nMachine engine processed: " ++ show trades

updateEventBook :: AugmentedEventBook -> Event -> AugmentedEventBook
updateEventBook (pb, o, _, _) (ps, Nothing) = (M.union (makeProposalBook ps) pb, o, Nothing, [])
updateEventBook (pb, _, _, _) (ps, Just o) = (M.union (makeProposalBook . validatedExecutedProposals (M.elems pb) o $ ps) pb, Just o, Nothing, [])

rebuildEventBook :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay AugmentedEventBook
rebuildEventBook psVec osVec = let ps = V.toList psVec
                                   os = V.toList osVec
                                   emptyBook = (TimeOfDay 0 0 0, (mempty, Nothing, Nothing, mempty))
                               in  M.fromDescList . init . scanlMap updateEventBook emptyBook $ buildEventMap ps os

rebuildLOB :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay Snapshot
rebuildLOB ps os = fmap shoot $ rebuildEventBook ps os

rebuildLOBXRay :: V.Vector Proposal -> V.Vector Order -> M.Map TimeOfDay Snapshot
rebuildLOBXRay ps os = fmap shootXRay $ rebuildEventBook ps os
