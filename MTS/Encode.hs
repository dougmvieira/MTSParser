{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module MTS.Encode (encodeDepth3LOB) where

import Data.Time (TimeOfDay)
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
import MTS.LOB (Price,
                Quantity,
        Bid,
        Ask,
        LOBSide,
        BidSide,
        AskSide,
        Snapshot)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C

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

encodeDepth3LOB :: M.Map TimeOfDay Snapshot -> B.ByteString
-- encodeDepth3LOB = encodeByName depth3LOBHeader . map snapshotToDepth3LOB . M.toAscList
encodeDepth3LOB = encodeByName depth3LOBHeader . map snapshotToDepth3LOB . M.toAscList

