{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MTS.Encode (encodeDepth3LOB, encodeLvl1LOB, encodeTopOfBookLOB, encodeRegGridLOB) where

import Control.Applicative
import Data.Time (TimeOfDay,
                  dayFractionToTimeOfDay,
                  timeOfDayToDayFraction)
import Data.Csv (ToField,
                 toField,
                 ToRecord,
                 encodeByName,
                 encode,
                 Header,
                 Parser,
                 (.=),
                 ToNamedRecord,
                 toNamedRecord,
                 namedRecord)
import Data.List (groupBy)
import GHC.Exts (the)
import MTS.LOB (Price,
                Quantity,
                Bid,
                Ask,
                LOBSide,
                BidSide,
                AskSide,
                Snapshot,
                EventType,
                PrioritisedBook)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C


-- Depth 3 LOB
type Depth3LOB = (TimeOfDay,
                  Price,
                  Quantity,
                  Price,
                  Quantity,
                  Price,
                  Quantity,
                  Price,
                  Quantity,
                  Price,
                  Quantity,
                  Price,
                  Quantity)

instance ToNamedRecord Depth3LOB where
   toNamedRecord (t, bp1, bq1, bp2, bq2, bp3, bq3,
                  ap1, aq1, ap2, aq2, ap3, aq3) = namedRecord [
      "Time" .= t,
      "BidPrice1" .= bp1,
      "BidQty1" .= bq1,
      "BidPrice2" .= bp2,
      "BidQty2" .= bq2,
      "BidPrice3" .= bp3,
      "BidQty3" .= bq3,
      "AskPrice1" .= ap1,
      "AskQty1" .= aq1,
      "AskPrice2" .= ap2,
      "AskQty2" .= aq2,
      "AskPrice3" .= ap3,
      "AskQty3" .= aq3]

depth3LOBHeader :: Header
depth3LOBHeader = V.fromList . map C.pack $ ["Time",
                                             "BidPrice1",
                                             "BidQty1",
                                             "BidPrice2",
                                             "BidQty2",
                                             "BidPrice3",
                                             "BidQty3",
                                             "AskPrice1",
                                             "AskQty1",
                                             "AskPrice2",
                                             "AskQty2",
                                             "AskPrice3"]

instance ToField TimeOfDay where
   toField = C.pack . show

extractBestNBid :: Int -> BidSide -> [(Price, Quantity)]
-- | Extract best N bid. If there are less levels than N, the result is padded with zeros.
extractBestNBid n = take n . (++ repeat (0, 0)) . M.toDescList

extractBestNAsk :: Int -> AskSide -> [(Price, Quantity)]
-- | Extract best N ask. If there are less levels than N, the result is padded with zeros.
extractBestNAsk n = take n . (++ repeat (0, 0)) . dropWhile ((== 0) . fst) . M.toAscList


snapshotToDepth3LOB :: (TimeOfDay, Snapshot) -> (TimeOfDay, Price, Quantity, Price, Quantity, Price, Quantity
                                                , Price, Quantity, Price, Quantity, Price, Quantity)
snapshotToDepth3LOB (t, (bs, as)) = let [(bp1, bq1), (bp2, bq2), (bp3, bq3)] = extractBestNBid 3 bs
                                        [(ap1, aq1), (ap2, aq2), (ap3, aq3)] = extractBestNAsk 3 as
                                    in  (t, bp1, bq1, bp2, bq2, bp3, bq3, ap1, aq1, ap2, aq2, ap3, aq3)

encodeDepth3LOB :: [(TimeOfDay, Snapshot)] -> B.ByteString
encodeDepth3LOB = encodeByName depth3LOBHeader . map snapshotToDepth3LOB


-- Level 1 LOB

type Lvl1LOB = (TimeOfDay,
                Price,
                Quantity,
                Price,
                Quantity,
                Price,
                Quantity,
                Price,
                EventType)

instance ToField EventType where
   toField = C.pack . show

instance ToNamedRecord Lvl1LOB where
   toNamedRecord (t, bp, bs, ap, as, lp, ls, lv, et)
      = namedRecord ["Time"               .= t,
                     "BidPrice"           .= bp,
                     "BidSize"            .= bs,
                     "AskPrice"           .= ap,
                     "AskSize"            .= as,
                     "LastPrice"          .= lp,
                     "LastSize"           .= ls,
                     "LastTradedVolume"   .= lv,
                     "EventType"          .= et]

lvl1LOBHeader :: Header
lvl1LOBHeader = V.fromList . map C.pack $ ["Time",
                                           "BidPrice",
                                           "BidSize",
                                           "AskPrice",
                                           "AskSize",
                                           "LastPrice",
                                           "LastSize",
                                           "LastTradedVolume",
                                           "EventType"]

dotProduct :: Num a => [a] -> [a] -> a
dotProduct = (sum .) . zipWith (*)

summariseTrades :: PrioritisedBook -> (Price, Quantity, Price)
-- | Get price, size and volume from trades. If trades is empty, return zeros.
summariseTrades [] = (0, 0, 0)
summariseTrades pb@(((p, _), _):_) = (abs p,,) <$> (* signum p) . sum <*> dotProduct (fst . fst <$> pb) $ snd <$> pb

extractBestBid :: BidSide -> (Price, Quantity)
extractBestBid = head . extractBestNBid 1

extractBestAsk :: AskSide -> (Price, Quantity)
extractBestAsk = head . extractBestNAsk 1

lvl1LOBslice :: (TimeOfDay, (Snapshot, PrioritisedBook, EventType)) -> Lvl1LOB
lvl1LOBslice (t, ((aSide, bSide), pb, et)) = let (lp, ls, lv) = summariseTrades pb
                                                 (bp, bs)     = extractBestBid bSide
                                                 (ap, as)     = extractBestAsk aSide
                                             in  (t, bp, bs, ap, as, lp, ls, lv, et)

toLvl1LOB :: [(TimeOfDay, (Snapshot, PrioritisedBook, EventType))] -> [Lvl1LOB]
toLvl1LOB [] = empty
toLvl1LOB (x:xs) = reverse . foldl accum [lvl1LOBslice x] $ lvl1LOBslice <$> xs
   where accum :: [Lvl1LOB] -> Lvl1LOB -> [Lvl1LOB]
         accum acc@((_, _, _, _, _, lp', lq', lv', _):_) (t, bp, bs, ap, as, lp, lq, lv, et)
            = (t, bp, bs, ap, as, lp `alt` lp', lq `alt` lq', lv `alt` lv', et):acc
         alt :: Double -> Double -> Double
         alt 0 x = x
         alt x _ = x

encodeLvl1LOB :: [(TimeOfDay, (Snapshot, PrioritisedBook, EventType))] -> B.ByteString
encodeLvl1LOB = encodeByName lvl1LOBHeader . toLvl1LOB


-- Top of Book LOB

type TopOfBookLOB = (TimeOfDay,
                     Price,
                     Quantity,
                     Price,
                     Quantity,
                     Quantity)

instance ToNamedRecord TopOfBookLOB where
   toNamedRecord (t, bp, bs, ap, as, et)
      = namedRecord ["Time"               .= t,
                     "BidPrice"           .= bp,
                     "BidSize"            .= bs,
                     "AskPrice"           .= ap,
                     "AskSize"            .= as,
                     "OrderFlowImbalance" .= et]

topOfBookLOBHeader :: Header
topOfBookLOBHeader = V.fromList . map C.pack $ ["Time",
                                                "BidPrice",
                                                "BidSize",
                                                "AskPrice",
                                                "AskSize",
                                                "OrderFlowImbalance"]

type TopOfBook = ((Price, Quantity), -- Bid side
                  (Price, Quantity)) -- Ask side

orderFlowImbalBid :: (Price, Quantity) -> (Price, Quantity) -> Quantity
orderFlowImbalBid (_, q) (0 ,  _) = -q
orderFlowImbalBid (0, _) (_ , q') = q'
orderFlowImbalBid (p, q) (p', q') | p == p' = q' - q
                                  | p <  p' = q'
                                  | p >  p' =    - q

orderFlowImbalAsk :: (Price, Quantity) -> (Price, Quantity) -> Quantity
orderFlowImbalAsk (p, q) (p', q') = orderFlowImbalBid (-p, q) (-p', q')

orderFlowImbalSlice :: TopOfBook -> TopOfBook -> Quantity
orderFlowImbalSlice (b, a) (b', a') = orderFlowImbalBid b b' - orderFlowImbalAsk a a'

orderFlowImbal :: [TopOfBook] -> [Quantity]
orderFlowImbal [] = []
orderFlowImbal xs@(((_, bs), (_, as)):xs') = bs - as:zipWith orderFlowImbalSlice xs xs'

coupleWithOrderFlowImbal :: [TopOfBook] -> [(TopOfBook, Quantity)]
coupleWithOrderFlowImbal = zip <$> id <*> orderFlowImbal

toTopOfBook :: Snapshot -> TopOfBook
toTopOfBook = (,) <$> extractBestBid . fst <*> extractBestAsk . snd

groupWith :: Eq b => (a -> b) -> [a] -> [[a]]
groupWith f = groupBy (\x y -> f x == f y)

compressWith :: (Show a, Ord b) => (a -> b) -> [a] -> [a]
-- | Removes consecutive duplicates, where duplicates are in the sense of the input function. Keeps the first of each group of duplicate elements.
compressWith = (fmap head .) . groupWith

toTopOfBookLOB :: [(TimeOfDay, Snapshot)] -> [TopOfBookLOB]
toTopOfBookLOB = uncurry (zipWith flatten) . fmap (zip <$> id <*> orderFlowImbal) . unzip . compressWith snd . fmap (fmap toTopOfBook)
   where flatten :: TimeOfDay -> (TopOfBook, Quantity) -> TopOfBookLOB
         flatten t (((bp, bs), (ap, as)), ofs) = (t, bp, bs, ap, as, ofs)

encodeTopOfBookLOB :: [(TimeOfDay, Snapshot)] -> B.ByteString
encodeTopOfBookLOB = encodeByName topOfBookLOBHeader . toTopOfBookLOB


-- Regularly-gridded LOB

type RegGridLOB = (TimeOfDay,
                   Price,
                   Quantity,
                   Price,
                   Quantity,
                   Price,
                   Quantity,
                   Quantity,
                   Quantity)

regGridLOBHeader :: Header
regGridLOBHeader = V.fromList . map C.pack $ ["Time",
                                              "BidPrice",
                                              "BidSize",
                                              "AskPrice",
                                              "AskSize",
                                              "LastPrice",
                                              "LastSize",
                                              "LastVolume",
                                              "OrderFlowImbalance"]

instance ToNamedRecord RegGridLOB where
   toNamedRecord (t, bp, bs, ap, as, lp, ls, lv, ofi)
      = namedRecord ["Time"               .= t,
                     "BidPrice"           .= bp,
                     "BidSize"            .= bs,
                     "AskPrice"           .= ap,
                     "AskSize"            .= as,
                     "LastPrice"          .= lp,
                     "LastSize"           .= ls,
                     "LastVolume"         .= lv,
                     "OrderFlowImbalance" .= ofi]

toResolution :: RealFrac a => a -> a -> a
toResolution res = (* res) . fromIntegral . ceiling . (/ res)

toRegularGrid :: Rational -> [(TimeOfDay, a)] -> [(TimeOfDay, [(TimeOfDay, a)])]
toRegularGrid dt = fmap colapseRepeatedTime . groupWith fst . fmap stampGridTime
   where stampGridTime :: (TimeOfDay, a) -> (TimeOfDay, (TimeOfDay, a))
         stampGridTime = (,) <$> dayFractionToTimeOfDay . toResolution dt . timeOfDayToDayFraction . fst <*> id
         colapseRepeatedTime :: [(TimeOfDay, v)] -> (TimeOfDay, [v])
         colapseRepeatedTime = ((,) <$> the . fst <*> snd) . unzip

firstThatIsNotXOrX :: Eq a => a -> [a] -> a
firstThatIsNotXOrX x xs = case filter (/= x) xs of []     -> x
                                                   (x':_) -> x'

lastThatIsNotXOrX :: Eq a => a -> [a] -> a
lastThatIsNotXOrX x = firstThatIsNotXOrX x . reverse

rebuiltTopOfBookWithTrades :: [(TimeOfDay, (Snapshot, PrioritisedBook))]
                           -> [(TimeOfDay, ((Price, Quantity, Price), (TopOfBook, Quantity)))]
rebuiltTopOfBookWithTrades = uncurry zip . fmap (uncurry zip . fmap coupleWithOrderFlowImbal . unzip) . unzip . compressWith snd . fmap (fmap $ (,) <$> summariseTrades . snd <*> toTopOfBook . fst)

toRegGridSlice :: [((Price, Quantity, Price), (TopOfBook, Quantity))]
               -> ((Price, Quantity, Price), (TopOfBook, Quantity))
toRegGridSlice = ((,) <$> lastThatIsNotXOrX (0, 0, 0) . fst <*> ((,) <$> last . fst <*> sum . snd) . unzip . snd) . unzip

flattenRegGridSlice :: (TimeOfDay, ((Price, Quantity, Price), (TopOfBook, Quantity)))
                    -> RegGridLOB
flattenRegGridSlice (t, ((lp, lq, lv), (((bp, bs), (ap, as)), ofi))) =
   (t, bp, bs, ap, as, lp, lq, lv, ofi)

toRegGridLOB :: Rational -> [(TimeOfDay, (Snapshot, PrioritisedBook))] -> [RegGridLOB]
toRegGridLOB dt = fmap flattenRegGridSlice . uncurry zip . fmap (fmap $ toRegGridSlice . snd . unzip) . unzip . toRegularGrid dt . rebuiltTopOfBookWithTrades

encodeRegGridLOB :: Rational -> [(TimeOfDay, (Snapshot, PrioritisedBook))] -> B.ByteString
encodeRegGridLOB dt = encodeByName regGridLOBHeader . toRegGridLOB dt
