{-# LANGUAGE DeriveGeneric #-}

module MTS.Types (MTSDay(..),
                  MTSTime(..),
                  MTSPico(..),
                  MTSStatus(..),
                  MTSSide(..),
                  MTSQty(..),
                  MTSLots(..),
                  MTSYield(..),
                  Verb(..),
                  OrderType(..),
                  MTSEvent(..),
                  Proposal(..),
                  Fill(..),
                  Order(..)) where

import Data.Text (Text)
import Data.Fixed (Pico)
import Data.Time (Day(), TimeOfDay(..))
import Data.Map (Map())
import GHC.Generics (Generic)


newtype MTSDay = MTSDay { getMTSDay :: Day } deriving Show
newtype MTSTime = MTSTime { getMTSTime :: TimeOfDay } deriving Show
newtype MTSPico = MTSPico { getMTSPico :: Pico } deriving Show
data MTSStatus = Active | Suspended | Unknown deriving (Eq, Show)
data MTSSide = BothSides | AskOnly | BidOnly deriving (Eq, Show)
newtype MTSQty = MTSQty { getMTSQty :: Double } deriving Show
newtype MTSLots = MTSLots { qtyFromLots :: Double } deriving Show
newtype MTSYield = MTSYield { getMTSYield :: Double } deriving Show
data Verb = Buy | Sell deriving (Eq, Show)
data OrderType = AllOrNone | FillAndKill deriving (Eq, Show)

class MTSEvent a where
   date       :: a -> Day
   time       :: a -> TimeOfDay
   marketCode :: a -> Text
   bondCode   :: a -> Text
   bondType   :: a -> Text
   eventID    :: a -> Int

data Proposal = Proposal { pMarketCode  :: Text
                         , pRefDate     :: MTSDay
                         , pUpdTime     :: MTSTime
                         , pUpdTimeMsec :: MTSPico
                         , pEndTime     :: MTSTime
                         , pEndTimeMsec :: MTSPico
                         , pBondCode    :: Text
                         , pBondType    :: Text
                         , pCheck_Logon :: Int
                         , pStatus      :: MTSStatus
                         , pBidPrice    :: Double
                         , pBidQty      :: MTSLots
                         , pBidEbmQty   :: MTSLots
                         , pBidDomQty   :: MTSLots
                         , pAskPrice    :: Double
                         , pAskQty      :: MTSLots
                         , pAskEbmQty   :: MTSLots
                         , pAskDomQty   :: MTSLots
                         , pBidYield    :: MTSYield
                         , pAskYield    :: MTSYield
                         , pProposalID  :: Int
                         , pQuotingSide :: MTSSide } deriving (Show, Generic)

instance MTSEvent Proposal where
   date = getMTSDay . pRefDate
   time = addPico <$> getMTSTime . pUpdTime <*> getMTSPico . pUpdTimeMsec
   marketCode = pMarketCode
   bondCode = pBondCode
   bondType = pBondType
   eventID = pProposalID

data Fill = Fill { fRefDate         :: MTSDay
                 , fMarketCode      :: Text
                 , fBondCode        :: Text
                 , fTime            :: MTSTime
                 , fTimeMsec        :: MTSPico 
                 , fVerb            :: Int
                 , fPrice           :: Double
                 , fQuantity        :: MTSQty
                 , fYield           :: MTSYield
                 , fOrderSeqNo      :: Int
                 , fOrderStatus     :: Text
                 , fAggProfile      :: Text
                 , fMultMarketFlag  :: Int
                 , fContractNo      :: Int
                 , fCCPFlag         :: Text
                 , fBondType        :: Text
                 , fSettlDate       :: Text
                 , fTransactionType :: Text } deriving (Show, Generic)

instance MTSEvent Fill where
   date = getMTSDay . fRefDate
   time = addPico <$> getMTSTime . fTime <*> getMTSPico . fTimeMsec
   marketCode = fMarketCode
   bondCode = fBondCode
   bondType = fBondType
   eventID = fOrderSeqNo

data Order = Order { oMarketCode  :: Text
                   , oRefDate     :: MTSDay
                   , oRefTime     :: MTSTime
                   , oRefTimeMsec :: MTSPico 
                   , oBondCode    :: Text
                   , oBondType    :: Text
                   , oOrderSeqNo  :: Int
                   , oOrderStatus :: Text
                   , oVerb        :: Verb
                   , oPrice       :: Double
                   , oQuantity    :: MTSQty
                   , oFillNo      :: Double
                   , oOrderType   :: OrderType } deriving (Show, Generic)

instance MTSEvent Order where
   date = getMTSDay . oRefDate
   time = addPico <$> getMTSTime . oRefTime <*> getMTSPico . oRefTimeMsec
   marketCode = oMarketCode
   bondCode = oBondCode
   bondType = oBondType
   eventID = oOrderSeqNo

addPico :: TimeOfDay -> Pico -> TimeOfDay
addPico (TimeOfDay h m p) p' = TimeOfDay h m (p + p')
