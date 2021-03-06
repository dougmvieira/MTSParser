{-# LANGUAGE TupleSections #-}

module MTS.LOB (shoot, rebuildEventBook, rebuildLOB, rebuildLOBWithLog, rebuildRichLOB, rebuildLOBXRay, rebuildEventTypes, Price, Quantity, Bid, Ask, LOBSide, AskSide, BidSide, Snapshot, PrioritisedBook, EventType) where

import MTS.Types
import MTS.Decode
import Control.Applicative
import Data.Fixed (Pico)
import Data.List (find, nub, nubBy, sort, sortOn)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.Time (Day,
                  TimeOfDay(..))
import GHC.Exts (groupWith, sortWith)
import qualified Data.Vector as V
import qualified Data.Map as M


data ParseError = OrderMatching | ProposalMatching | FillVerification deriving (Read, Show)

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

data EventType = LimitOrder | MarketOrder | Cancellation | Modification deriving (Eq, Read, Show)

type Event = ([Proposal], Maybe Order)
type EventBook = (ProposalBook, Maybe Order)
type AugmentedEventBook = (ProposalBook, Maybe Order, Maybe AggrProposal, PrioritisedBook, [ParseError])

type BondCode = Text

type SymEither a = Either a a


nubWith :: Eq b => (a -> b) -> [a] -> [a]
nubWith f = nubBy $ \x y -> f x == f y

choice2 :: (Alternative f, Alternative g) => (f a, g b) -> (f a, g b) -> (f a, g b)
choice2 (x, y) (x', y') = (x <|> x', y <|> y')

fst5 :: (a, b, c, d, e) -> a
fst5 (x, _, _, _, _) = x

symEither :: (a -> b) -> SymEither a -> b
symEither f = either f f

constEither :: a -> a -> Either b c -> a
constEither x y = either (const x) (const y)

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

-- TODO: See better approach (deactivated filterMTS because MTS orders interact with EBM)
filterMTS :: MTSEvent a => [a] -> [a]
--filterMTS = filter $ (==mtsCode) . marketCode
filterMTS = id

filterDate :: MTSEvent a => Day -> [a] -> [a]
filterDate d = filter $ (==d) . date

findByID :: MTSEvent a => ID -> [a] -> Maybe a
findByID i = find $ (== i) . eventID

filterID :: MTSEvent a => ID -> [a] -> [a]
filterID i = filter $ (== i) . eventID

filterBond :: MTSEvent a => Text -> [a] -> [a]
filterBond b = filter $ (== b) . bondCode

stampTime :: MTSEvent a => a -> (TimeOfDay, a)
stampTime = (,) <$> time <*> id

toProposalTimeSeries :: [Proposal] -> M.Map TimeOfDay [Proposal]
toProposalTimeSeries = M.fromListWith (++) . fmap (fmap pure . stampTime)

toOrderTimeSeries :: [Order] -> M.Map TimeOfDay Order
toOrderTimeSeries = M.fromListWith err . fmap stampTime where
   err = error "Found two orders with same time stamp."

pairWithOrders :: Alternative f => [Order] -> M.Map TimeOfDay (f a) -> M.Map TimeOfDay (f a, Maybe Order)
pairWithOrders os xs = M.unionWith choice2 ((, empty) <$> xs) . fmap ((empty,) . Just) $ toOrderTimeSeries os

toDescEventTimeSeries :: [Proposal] -> [Order] -> [(TimeOfDay, Event)]
toDescEventTimeSeries [] _ = mempty
toDescEventTimeSeries (p:ps) os = let os' = filterBond (bondCode p) . filterDate (date p) $ filterMTS os
                                  in  M.toDescList . pairWithOrders os' . toProposalTimeSeries $ filterMTS ps

filterActiveProposals :: ProposalBook -> [Proposal]
filterActiveProposals = filterActive . M.elems

aggregQties :: [(Price, Quantity)] -> LOBSide
aggregQties = M.fromListWith (+)

-- |Shoot a snapshot of the visible LOB
shoot :: AugmentedEventBook -> Snapshot
shoot eb = (aggregQties . map pBid . filterActiveProposals . fst5 $ eb,
            aggregQties . map pAsk . filterActiveProposals . fst5 $ eb)

-- |Shoot a snapshot of the LOB with hidden orders
shootXRay :: AugmentedEventBook -> Snapshot
shootXRay eb = (aggregQties . map pBidXRay . filterActiveProposals . fst5 $ eb,
                aggregQties . map pAskXRay . filterActiveProposals . fst5 $ eb)

hasDuplicate :: Eq a => [a] -> Bool
hasDuplicate = (/=) <*> nub

hasDuplicatePrices :: ProposalBook -> Bool
hasDuplicatePrices pb = let ((bs, as), _) = M.mapAccum (\(bs', as') p -> ((pBidPrice p:bs', pAskPrice p:as'), ())) ([],[]) pb
                        in hasDuplicate bs || hasDuplicate as

toProposalBook :: [Proposal] -> ProposalBook
toProposalBook = M.fromList . fmap ((,) <$> eventID <*> id)

--TODO: Consider more carefully the sanityCheck issue
--toProposalBook = sanityCheck . M.fromList . map ((,) <$> eventID <*> id) where
--   sanityCheck :: ProposalBook -> ProposalBook
--   sanityCheck pb = if hasDuplicatePrices pb
--            then error $ "Invalid duplicate time proposals: " ++ show pb
--            else pb

toSignedBook :: [Proposal] -> SignedBook
toSignedBook = foldl (\sb p ->
   case pQuotingSide p
   of BothSides -> ((signedBidPrice p, time p), bidQty p):((askPrice p, time p), askQty p):sb
      AskOnly   -> ((askPrice p, time p), askQty p):sb
      BidOnly   -> ((signedBidPrice p, time p), bidQty p):sb) empty

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
fillAndKill p q lob = let (_, _, trades) = walkTheLOB p (q, lob, empty) in reverse trades

allOrNone :: Price -> Quantity -> PrioritisedBook -> PrioritisedBook
allOrNone p q lob = let (qRem, _, trades) = walkTheLOB p (q, lob, empty)
                    in if qRem == 0 then reverse trades else empty

proposalAggr :: Price -> Quantity -> PrioritisedBook -> PrioritisedBook
proposalAggr = fillAndKill

executeOrderWith :: OrderType -> Price -> Quantity -> PrioritisedBook -> PrioritisedBook
executeOrderWith FillAndKill = fillAndKill
executeOrderWith AllOrNone   = allOrNone

executeOrder :: Order -> PrioritisedBook -> PrioritisedBook
executeOrder = executeOrderWith <$> oOrderType <*> signedPrice <*> qty

executeProposal :: AggrProposal -> PrioritisedBook -> PrioritisedBook
executeProposal p = (proposalAggr <$> signedPrice <*> qty $ p) . filter ((/= (signedPrice p, time p)) . fst)

execute :: Either AggrProposal Order -> PrioritisedBook -> PrioritisedBook
execute (Left p) = executeProposal p
execute (Right o) = executeOrder o

executedProposals :: PrioritisedBook -> PrioritisedBook -> PrioritisedBook
executedProposals trades pb = zipWith diffSnd pb trades
   where diffSnd :: Num b => (a, b) -> (a, b) -> (a, b)
         diffSnd (i, x) (_, y) = (i, x - y)

isProposalAtSignedPrice :: Price -> Proposal -> Bool
isProposalAtSignedPrice p | p < 0 = (== p) . signedBidPrice
                          | p > 0 = (== p) .       askPrice

isAtTime :: MTSEvent a => TimeOfDay -> a -> Bool
isAtTime t = (== t) . time

fetchByPriceTime :: Price -> TimeOfDay -> [Proposal] -> Proposal
fetchByPriceTime p t ps = case filter ((&&) <$> isAtTime t <*> isProposalAtSignedPrice p) ps
                          of []  -> error $ "No proposal found at time " ++ show t ++ " and price " ++ show p ++ "."
                             [r] -> r
                             rs   -> error $ "Multiple proposals at price " ++ show p ++ " and time " ++ show t ++ ". The proposals are:\n" ++ unlines (show <$> rs)

fromPrioritisedBook :: [Proposal] -> PrioritisedBook -> [Proposal]
fromPrioritisedBook ps = map (\p -> uncurry fetchByPriceTime (fst p) $ filterActive ps)

implyVerb :: PrioritisedBook -> Verb
implyVerb [] = error "Cannot imply verb from empty prioritised book."
implyVerb ((_, q):_) | q < 0 = Sell
                     | q > 0 = Buy
implyVerb pb = error $ show pb

validateNewOrder :: Verb -> Proposal -> Quantity -> Proposal -> Bool
validateNewOrder Buy  p 0 p' = bidQty p' == bidQty p && not (isActive p') && bidPrice p' == bidPrice p -- Strangely, the askQty can change in this case
validateNewOrder Sell p 0 p' = askQty p' == askQty p && not (isActive p') && askPrice p' == askPrice p -- Strangely, the bidQty can also change in this case
validateNewOrder Buy  p q p' = validateNewOrder Buy  p 0 p' || (askQty p' ==        q && bidQty p' == bidQty p &&      isActive p'  && bidPrice p' == bidPrice p)
validateNewOrder Sell p q p' = validateNewOrder Sell p 0 p' || (askQty p' == askQty p && bidQty p' ==        q &&      isActive p'  && askPrice p' == askPrice p)

validateExecution :: Verb -> [Proposal] -> PrioritisedBook -> [Proposal] -> Bool
validateExecution v ps pb ps' = let psAligned = fromPrioritisedBook ps pb
                                    psAligned' = alignEventByID psAligned ps'
                                    qs = map snd pb
                                in  all (not . null) psAligned'
                                    && all (uncurry3 $ validateNewOrder v) (zip3 psAligned qs (map fromJust psAligned'))
   where uncurry3 :: (a -> b -> c -> r) -> ((a, b, c) -> r)
         uncurry3 f (x, y, z) = f x y z

validatedTrades :: [Proposal] -> Either AggrProposal Order -> [Proposal] -> SymEither PrioritisedBook
validatedTrades ps o ps' = let pb = toPrioritisedBook (either verb verb o) ps
                               trades = execute o pb
                               psToValidate = case o of Left p  -> filter ((/= eventID p) . eventID) ps' -- TODO: check if the aggressing limit order stays in the LOB with the remaining size
                                                        Right _ -> ps'
                           in if   validateExecution (either verb verb o) ps (executedProposals trades pb) psToValidate
                              then Right trades
                              else Left trades

validatedTradesFallback :: [Proposal] -> Either AggrProposal Order -> [Proposal] -> PrioritisedBook
validatedTradesFallback ps o ps' = let err = error "Fallback mechanism failed."
                                       psAligned = catMaybes $ alignEventByID (nubWith eventID ps') ps
                                   in  either err id $ validatedTrades psAligned o ps'

orderMatchingLongErrorLog :: [Proposal] -> [Proposal] -> Order -> PrioritisedBook -> String
orderMatchingLongErrorLog ps ps' o trades = let psAligned = fromPrioritisedBook ps trades
                                                psAligned' = alignEventByID psAligned ps'
                                            in  "New state of LOB is not consistent with market order: " ++ show o
                                                ++ "\nCurrent affected proposals are: " ++ show psAligned
                                                ++ "\nNew proposals are: " ++ show psAligned'
                                                ++ "\nMachine engine processed: " ++ show trades
                                                ++ "\n\n"

orderMatchingShortErrorLog :: AugmentedEventBook -> String
orderMatchingShortErrorLog (_, Just o, _, _, _) = unwords [unpack $ bondCode o,
                                                           show $ date o,
                                                           show $ time o]
                                                  ++ ". Order matching error."
orderMatchingShortErrorLog _ = "WHAT!? Order matching error without an order!?."

proposalMatchingShortErrorLog :: AugmentedEventBook -> String
proposalMatchingShortErrorLog (_, _, Just p, _, _) = unwords [unpack $ bondCode p,
                                                              show $ date p,
                                                              show $ time p]
                                                     ++ ". Proposal matching error."
proposalMatchingShortErrorLog _ = "WHAT!? Proposal matching error without a proposal!?."


errorLogWith :: (AugmentedEventBook -> String)
             -> (AugmentedEventBook -> String)
             -> AugmentedEventBook
             -> String
errorLogWith orderLog proposalLog eb@(_, _, _, _, err) = mconcat $ logEach <$> err 
   where logEach :: ParseError -> String
         logEach OrderMatching = orderLog eb
         logEach ProposalMatching = proposalLog eb

shortErrorLog :: AugmentedEventBook -> String
shortErrorLog = errorLogWith orderMatchingShortErrorLog proposalMatchingShortErrorLog

tradesFromOrder :: ProposalBook -> [Proposal] -> Either AggrProposal Order -> (PrioritisedBook, [ParseError])
tradesFromOrder pb ps o = let pb' = M.elems pb
                              eitherTrades = validatedTrades pb' o ps
                          in (either (const $ validatedTradesFallback pb' o ps) id eitherTrades,
                              constEither [constEither ProposalMatching OrderMatching o] [] eitherTrades)

incorporateProposals :: ProposalBook -> [Proposal] -> ProposalBook
incorporateProposals pb ps = M.union (toProposalBook $ solveAmbiguity pb ps) pb

pIDPricesQties :: Proposal -> (ID, Price, Quantity, Price, Quantity)
pIDPricesQties = (,,,,) <$> eventID <*> bidPrice <*> bidQty <*> askPrice <*> askQty

solveAmbiguity :: ProposalBook -> [Proposal] -> [Proposal]
solveAmbiguity pb = mconcat . fmap solveEach . groupWith eventID
   where solveEach :: [Proposal] -> [Proposal]
         solveEach [p] = [p]
         solveEach ps  = let psActive = filter ((== 0) . pCheck_Logon) ps
                             psInactive = filter ((== 1) . pCheck_Logon) ps
                          in case length psActive `compare` length psInactive
                             of LT -> psInactive
                                GT -> psActive
                                EQ -> case M.lookup (eventID $ head ps) pb
                                      of   Nothing -> psInactive
                                           Just p  -> if   pIDPricesQties p == pIDPricesQties (head ps) || marketCode p == mtsCode
                                                      then psActive else psInactive -- I have no idea if this EBM/MTS difference holds for other cases


augmentEventBook :: ProposalBook -> (TimeOfDay, Event) -> (TimeOfDay, AugmentedEventBook)
augmentEventBook pb (t, (ps, o)) = let pb' = incorporateProposals pb ps
                                   in case o
                                      of   Nothing ->     (t, (pb', o, empty,  empty, empty))
                                           Just o' -> let (trades, log) = tradesFromOrder pb ps (Right o')
                                                      in  (t, (pb', o, empty, trades,   log))

augmentEventBookWithAggrProposal :: ProposalBook -> AggrProposal -> (TimeOfDay, Event) -> (TimeOfDay, AugmentedEventBook)
augmentEventBookWithAggrProposal pb p (t, (ps, _)) = let pb' = incorporateProposals pb ps
                                                         (trades, log) = tradesFromOrder pb ps (Left p)
                                                     in  (time p, (pb', empty, Just p, trades, log))

augmentEventTimeSeries :: [(TimeOfDay, Event)] -> [(TimeOfDay, AugmentedEventBook)]
augmentEventTimeSeries = reverse . snd . foldr f (empty, empty)
  where f :: (TimeOfDay, Event)
          -> (Maybe AggrProposal, [(TimeOfDay, AugmentedEventBook)])
          -> (Maybe AggrProposal, [(TimeOfDay, AugmentedEventBook)])
        f e ~(p, aebs) = let pb = fst5 . snd $ head aebs
                             ps = fst $ snd e
                         in  if   null aebs
                             then (empty, [augmentEventBook (toProposalBook ps) e])
                             else case p
                                  of Just p' -> (empty, augmentEventBookWithAggrProposal pb p' e:aebs)
                                     Nothing -> case getAggresiveProposal (M.elems pb) ps
                                                of   Just p' -> (Just p', aebs)
                                                     Nothing -> (empty  , augmentEventBook pb e:aebs)

getAggresiveProposal :: [Proposal] -> [Proposal] -> Maybe AggrProposal
getAggresiveProposal _  []  = Nothing
getAggresiveProposal ps [p] = let ps' = filter ((/= eventID p) . eventID) $ filter isActive ps
                                  bidAggr = (&&) <$> not . null <*> (bidPrice p >=) . minimum $ askPrice <$> filter ((/= BidOnly) . pQuotingSide) ps'
                                  askAggr = (&&) <$> not . null <*> (askPrice p <=) . maximum $ bidPrice <$> filter ((/= AskOnly) . pQuotingSide) ps'
                                  justPIf b v = if b then Just $ AggrProposal (p, v) else Nothing
                              in  case pQuotingSide p
                                  of   BidOnly   -> justPIf bidAggr Buy
                                       AskOnly   ->                         justPIf askAggr Sell
                                       BothSides -> justPIf bidAggr Buy <|> justPIf askAggr Sell
getAggresiveProposal _  _   = Nothing

rebuildEventBook :: [Proposal] -> [Order] -> [(TimeOfDay, AugmentedEventBook)]
rebuildEventBook ps = augmentEventTimeSeries . toDescEventTimeSeries ps

filterFillsByBondAndDate :: Text -> Day -> [Fill] -> [Fill]
filterFillsByBondAndDate b d = filterDate d . filterBond b . filterMTS

fillToQtiesAndPrices :: [Fill] -> [(ID, (Quantity, Price))]
fillToQtiesAndPrices = fmap ((,) <$> eventID <*> ((,) <$> qty <*> price)) . sortWith time

tradesToQtiesAndPrices :: Either AggrProposal Order -> PrioritisedBook -> [(ID, (Quantity, Price))]
tradesToQtiesAndPrices o = fmap (\((p, _), q) -> (either eventID eventID o, (q, abs p)))

verifyTradesWithFills :: [Fill] -> [(Either AggrProposal Order, PrioritisedBook)] -> Maybe ParseError
verifyTradesWithFills fs [] = Nothing
verifyTradesWithFills fs trades@((o, _):_) = let b = either bondCode bondCode o
                                                 d = either date date o
                                                 fs' = filterFillsByBondAndDate b d fs
                                             in if sort (fillToQtiesAndPrices fs') == sort (mconcat $ uncurry tradesToQtiesAndPrices <$> trades)
                                                then Nothing else Just FillVerification

fillsVerificationLog :: String
fillsVerificationLog = "Order matches NOT consistent with Fills file"

rebuildLOB :: V.Vector Proposal -> V.Vector Order -> [(TimeOfDay, Snapshot)]
rebuildLOB ps os = fmap (fmap shoot) . rebuildEventBook (V.toList ps) $ V.toList os

countActiveProposals :: ProposalBook -> Int
countActiveProposals = length . filterActive . M.elems

implyEventType :: AugmentedEventBook -> AugmentedEventBook -> EventType
implyEventType (pb, _, _, _, _) (pb', o, p, _, _)
   = let impliedByProposals = case compare (countActiveProposals pb') $ countActiveProposals pb
                              of   GT -> LimitOrder
                                   EQ -> Modification
                                   LT -> Cancellation
     in  maybe impliedByProposals (const MarketOrder)  $ toEitherAggression p o

rebuildEventTypes :: [AugmentedEventBook] -> [EventType]
rebuildEventTypes [] = empty
rebuildEventTypes es@(_:es') = LimitOrder:zipWith implyEventType es es'

toEitherAggression :: Maybe AggrProposal -> Maybe Order -> Maybe (Either AggrProposal Order)
toEitherAggression  Nothing  Nothing = Nothing
toEitherAggression (Just p)  Nothing = Just (Left p)
toEitherAggression  Nothing (Just o) = Just (Right o)
toEitherAggression _ _ = error "Found order and aggressive proposal at the same time."

rebuildLOBWithLog :: V.Vector Proposal -> V.Vector Order -> V.Vector Fill -> ([(TimeOfDay, Snapshot)], String, String)
rebuildLOBWithLog ps os fs = let eb = rebuildEventBook (V.toList ps) (V.toList os)
                                 lob = fmap shoot <$> eb
                                 log = unlines . filter (not . null) $ shortErrorLog . snd <$> eb
                                 trades = catMaybes $ (\(x, y) -> maybe Nothing (Just . (, y)) x) . (\(_, o, p, t, _) -> (toEitherAggression p o, t)) . snd <$> eb
                                 log' = maybe empty (const fillsVerificationLog) $ verifyTradesWithFills (V.toList fs) trades
                             in  (lob, log, log')

rebuildRichLOB :: V.Vector Proposal -> V.Vector Order -> V.Vector Fill -> ([(TimeOfDay, (Snapshot, PrioritisedBook, EventType))], String, String)
rebuildRichLOB ps os fs = let eb = rebuildEventBook (V.toList ps) (V.toList os)
                              log = unlines . filter (not . null) $ shortErrorLog . snd <$> eb
                              trades = catMaybes $ (\(x, y) -> maybe Nothing (Just . (, y)) x) . (\(_, o, p, t, _) -> (toEitherAggression p o, t)) . snd <$> eb
                              log' = maybe empty (const fillsVerificationLog) $ verifyTradesWithFills (V.toList fs) trades
                              lob = zip <$> fmap fst <*> (zip3 <$> fmap shoot <*> fmap (\(_, _, _, x, _) -> x) <*> rebuildEventTypes) . fmap snd $ eb
                          in  (lob, log, log')

rebuildLOBXRay :: V.Vector Proposal -> V.Vector Order -> [(TimeOfDay, Snapshot)]
rebuildLOBXRay ps os = fmap (fmap shootXRay) $ rebuildEventBook (V.toList ps) (V.toList os)
