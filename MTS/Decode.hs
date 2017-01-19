{-# LANGUAGE DeriveGeneric #-}

module MTS.Decode (parseProposal,
		   parseFill,
		   parseOrder) where

import MTS.Types
import Data.Char (ord)
import Data.Csv (FromField,
                 FromRecord,
		 parseField,
		 decode,
		 decodeWith,
                 defaultDecodeOptions,
		 decDelimiter,
		 HasHeader(NoHeader),
		 Parser)
import Data.Fixed (Pico)
import Data.Text (Text,
                  pack)
import Data.Time (defaultTimeLocale,
                  parseTimeM,
		  Day,
		  TimeOfDay(..))
import Data.Vector (Vector())

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B

class FromRecord a => MTSTickData a

instance FromField MTSDay where
  parseField = fmap MTSDay . parseTimeM False defaultTimeLocale "%d/%m/%Y" . C.unpack
-- The example in documentation is not padded, but data is.

instance FromField MTSTime where
  parseField = fmap MTSTime . parseTimeM False defaultTimeLocale "%X" . C.unpack

instance FromField MTSPico where
  parseField = fmap (MTSPico . (/1000000) . fromIntegral) . (parseField :: C.ByteString -> Parser Int)
-- The documentation says UpdTimeMsec is in milliseconds, but data suggests microseconds.

instance FromField MTSStatus where
  parseField = parseStatus . C.unpack where
    parseStatus :: String -> Parser MTSStatus
    parseStatus "0" = return Active
    parseStatus "1" = return Suspended
    parseStatus "8" = return Unknown
    parseStatus _ = fail "Failed to parse MTSStatus."

instance FromField MTSSide where
  parseField = parseSide . C.unpack where
    parseSide :: String -> Parser MTSSide
    parseSide "Double Side" = return BothSides
    parseSide "Ask" = return AskOnly
    parseSide "Bid" = return BidOnly
    parseSide _ = fail "Failed to parse MTSSide."

instance FromField MTSQty where
  parseField = fmap MTSQty . parseField . C.filter (/= ',')

instance FromField MTSYield where
  parseField = fmap MTSYield . parseField . C.pack . fixInitDot . C.unpack where
    fixInitDot ('-':'.':s) = "-0." ++ s
    fixInitDot ('.':s) = "0." ++ s
    fixInitDot s = s

instance FromField Verb where
  parseField = parseSide . C.unpack where
    parseSide :: String -> Parser Verb
    parseSide "0" = return Buy
    parseSide "1" = return Sell
    parseSide _ = fail "Failed to parse Verb."

instance FromField OrderType where
  parseField = parseSide . C.unpack where
    parseSide :: String -> Parser OrderType
    parseSide "FillAndKill" = return FillAndKill
    parseSide "AllOrNone" = return AllOrNone
    parseSide _ = fail "Failed to parse Verb."


instance MTSTickData Proposal
instance FromRecord Proposal
instance MTSTickData Fill
instance FromRecord Fill
instance MTSTickData Order
instance FromRecord Order

parseMTSTickData :: MTSTickData a => B.ByteString -> Either String (Vector a)
parseMTSTickData = decodeWith options NoHeader where
  options = defaultDecodeOptions {decDelimiter = fromIntegral $ ord ';'}

--parseMTSTickDataToList :: MTSTickData a => B.ByteString -> [a]
  
parseProposal :: B.ByteString -> Either String (Vector Proposal)
parseProposal = parseMTSTickData

parseFill :: B.ByteString -> Either String (Vector Fill)
parseFill = parseMTSTickData

parseOrder :: B.ByteString -> Either String (Vector Order)
parseOrder = parseMTSTickData
