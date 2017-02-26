{-# LANGUAGE TupleSections #-}

module MTS.LOB (shoot, rebuildEventBook, rebuildLOB, rebuildLOBWithLog, rebuildLOBXRay, Price, Quantity, Bid, Ask, LOBSide, AskSide, BidSide, Snapshot) where

import MTS.Types
import MTS.Decode
import Control.Applicative
import Data.Fixed (Pico)
import Data.List (find, sortOn)
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Data.Time (Day,
                  TimeOfDay(..))
import GHC.Exts (sortWith)
import qualified Data.Vector as V
import qualified Data.Map as M


data ParseError = OrderMatching | ProposalMatching deriving (Read, Show)

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
type AugmentedEventBook = (ProposalBook, Maybe Order, Maybe (Proposal, Verb), PrioritisedBook, [ParseError])

type BondCode = Text

type SymEither a = Either a a


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

filterMTS :: MTSEvent a => [a] -> [a]
filterMTS = filter $ (==mtsCode) . marketCode

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
hasDuplicate [] = False
hasDuplicate (x:xs) = x `elem` xs && hasDuplicate xs

hasDuplicatePrices :: ProposalBook -> Bool
hasDuplicatePrices pb = let ((bs, as), _) = M.mapAccum (\(bs', as') p -> ((pBidPrice p:bs', pAskPrice p:as'), ())) ([],[]) pb
                        in hasDuplicate bs || hasDuplicate as

toProposalBook :: [Proposal] -> ProposalBook
toProposalBook = sanityCheck . M.fromList . map ((,) <$> eventID <*> id) where
   sanityCheck :: ProposalBook -> ProposalBook
   sanityCheck pb = if hasDuplicatePrices pb
            then error $ "Invalid duplicate time proposals: " ++ show pb
            else pb

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
proposalAggr p q lob = let (qRem, _, trades) = walkTheLOB p (q, lob, empty)
                    in if qRem == 0 then reverse trades else undefined -- TODO

executeOrderWith :: OrderType -> Price -> Quantity -> PrioritisedBook -> PrioritisedBook
executeOrderWith FillAndKill = fillAndKill
executeOrderWith AllOrNone   = allOrNone

executeOrder :: Order -> PrioritisedBook -> PrioritisedBook
executeOrder = executeOrderWith <$> oOrderType <*> signedPrice <*> qty

executeProposal :: AggrProposal -> PrioritisedBook -> PrioritisedBook
executeProposal = proposalAggr <$> signedPrice <*> qty

execute :: Either AggrProposal Order -> PrioritisedBook -> PrioritisedBook
execute (Left p) = executeProposal p
execute (Right o) = executeOrder o
--------------------------------------------------------------------------------------------

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
                             _   -> error $ "Multiple proposals have the same price and time."

fromPrioritisedBook :: [Proposal] -> PrioritisedBook -> [Proposal]
fromPrioritisedBook ps = map (\p -> uncurry fetchByPriceTime (fst p) ps)

implyVerb :: PrioritisedBook -> Verb
implyVerb [] = error "Cannot imply verb from empty prioritised book."
implyVerb ((_, q):_) | q < 0 = Sell
                     | q > 0 = Buy
implyVerb pb = error $ show pb

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

validatedTrades :: [Proposal] -> Either AggrProposal Order -> [Proposal] -> SymEither PrioritisedBook
validatedTrades ps o ps' = let pb = toPrioritisedBook (either verb verb o) ps
                               trades = execute o pb
                           in if   validateExecution (either verb verb o) ps (executedProposals trades pb) ps'
                              then Right trades
                              else Left trades

validatedTradesFallback :: [Proposal] -> Either AggrProposal Order -> [Proposal] -> PrioritisedBook
validatedTradesFallback ps o ps' = let err = error "Fallback mechanism failed."
                                       psAligned = catMaybes $ alignEventByID ps' ps
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
orderMatchingShortErrorLog (_,  Just o, _, _, _) = unwords [unpack $ bondCode o,
                                                            show $ date o,
                                                            show $ time o]
                                                   ++ ". Order matching error.\n"
orderMatchingShortErrorLog _ = "WHAT!? Order matching error without an order!?.\n"

errorLogWith :: (AugmentedEventBook -> String)
             -> (AugmentedEventBook -> String)
             -> AugmentedEventBook
             -> String
errorLogWith orderLog proposalLog eb@(_, _, _, _, err) = mconcat $ logEach <$> err 
   where logEach :: ParseError -> String
         logEach OrderMatching = orderLog eb
         logEach ProposalMatching = proposalLog eb

tradesFromOrder :: ProposalBook -> [Proposal] -> Either AggrProposal Order -> (PrioritisedBook, [ParseError])
tradesFromOrder pb ps o = let pb' = M.elems pb
                              eitherTrades = validatedTrades pb' o ps
                          in (either (const $ validatedTradesFallback pb' o ps) id eitherTrades,
                              constEither [OrderMatching] [] eitherTrades)

incorporateProposals :: ProposalBook -> [Proposal] -> ProposalBook
incorporateProposals pb ps = M.union (toProposalBook ps) pb

augmentEventBook :: ProposalBook -> (TimeOfDay, Event) -> (TimeOfDay, AugmentedEventBook)
augmentEventBook pb (t, (ps, o)) = let pb' = incorporateProposals pb ps
                                   in case o
                                      of   Nothing ->     (t, (pb', o, empty,  empty, empty))
                                           Just o' -> let (trades, log) = tradesFromOrder pb ps (Right o')
                                                      in  (t, (pb', o, empty, trades,   log))

augmentEventTimeSeries :: [(TimeOfDay, Event)] -> [(TimeOfDay, AugmentedEventBook)]
augmentEventTimeSeries = snd . foldr f (empty, empty)
  where f :: (TimeOfDay, Event)
          -> (Maybe (Proposal, Verb), [(TimeOfDay, AugmentedEventBook)])
          -> (Maybe (Proposal, Verb), [(TimeOfDay, AugmentedEventBook)])
        f e ~(p, aebs) = let pb = fst5 . snd $ head aebs
                             ps = fst $ snd e
                         in  if   null aebs
                             then (empty, [augmentEventBook (toProposalBook ps) e])
                             else case p
                                  of Just p' -> (empty, (time $ fst p', (incorporateProposals pb ps, empty, Just p', empty, empty)):aebs)
                                     Nothing -> case getAggresiveProposal (M.elems pb) ps
                                                of   Just p' -> (Just p', aebs)
                                                     Nothing -> (empty  , augmentEventBook pb e:aebs)

getAggresiveProposal :: [Proposal] -> [Proposal] -> Maybe (Proposal, Verb)
getAggresiveProposal _  []  = Nothing
getAggresiveProposal ps [p] = let bidAggr = bidPrice p >= minimum (askPrice <$> ps)
                                  askAggr = askPrice p <= maximum (bidPrice <$> ps)
                                  justPIf b = if b then Just p else Nothing
                              in  case pQuotingSide p
                                  of   AskOnly   -> (, Buy) <$> justPIf askAggr
                                       BidOnly   ->                                 (, Sell) <$> justPIf bidAggr
                                       BothSides -> (, Buy) <$> justPIf askAggr <|> (, Sell) <$> justPIf bidAggr
getAggresiveProposal _  _   = Nothing

rebuildEventBook :: [Proposal] -> [Order] -> [(TimeOfDay, AugmentedEventBook)]
rebuildEventBook ps = augmentEventTimeSeries . toDescEventTimeSeries ps

fillToQtiesAndPrices :: MTSEvent a => a -> [Fill] -> [(Quantity, Price)]
fillToQtiesAndPrices e = fmap ((,) <$> qty <*> price) . sortWith time . filterDate (date e) . filterBond (bondCode e) . filterID (eventID e)

tradesToQtiesAndPrices :: PrioritisedBook -> [(Quantity, Price)]
tradesToQtiesAndPrices = fmap (\((p, _), q) -> (q, abs p))

verifyTradesWithFills :: [Fill] -> Maybe Order -> PrioritisedBook -> Bool
verifyTradesWithFills fs Nothing pb = False
verifyTradesWithFills fs (Just o) pb = fillToQtiesAndPrices o fs == tradesToQtiesAndPrices pb

fillsVerificationLog :: Bool -> Maybe String
fillsVerificationLog True  = Nothing
fillsVerificationLog False = Just "[Warning] Order matches NOT consistent with Fills file"

rebuildLOB :: V.Vector Proposal -> V.Vector Order -> [(TimeOfDay, Snapshot)]
rebuildLOB ps os = fmap (fmap shoot) . rebuildEventBook (V.toList ps) $ V.toList os

rebuildLOBWithLog :: V.Vector Proposal -> V.Vector Order -> V.Vector Fill -> IO [(TimeOfDay, Snapshot)]
rebuildLOBWithLog ps os fs = do
   (lob, log, trades) <- return $ (,,) <$> fmap (fmap shoot) <*> mconcat . fmap (errorLogWith orderMatchingShortErrorLog (const empty)) . map snd <*> filter (not . null . snd) . fmap ((\(_, o, _, t, _) -> (o, t)) . snd) $ rebuildEventBook (V.toList ps) (V.toList os)
   mapM_ putStrLn . fillsVerificationLog $ all (uncurry (verifyTradesWithFills $ V.toList fs)) trades
   putStrLn log
   return lob

rebuildLOBXRay :: V.Vector Proposal -> V.Vector Order -> [(TimeOfDay, Snapshot)]
rebuildLOBXRay ps os = fmap (fmap shootXRay) $ rebuildEventBook (V.toList ps) (V.toList os)
