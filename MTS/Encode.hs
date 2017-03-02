{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}

module MTS.Encode (encodeDepth3LOB, encodeLvl1LOB) where

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
import GHC.Exts (groupWith, the)
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
extractBestNBid n = take n . (++ repeat (0.0,0.0)) . M.toDescList 

extractBestNAsk :: Int -> AskSide -> [(Price, Quantity)]
-- | Extract best N ask. If there are less levels than N, the result is padded with zeros.
extractBestNAsk n = take n . (++ repeat (0.0,0.0)) . M.toAscList 


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
