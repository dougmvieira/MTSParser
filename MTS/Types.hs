{-# LANGUAGE DeriveGeneric #-}

module MTS.Types (MTSEvent(..),
                  MTSOneSidedEvent(..),

                  Quantity(),
                  Price(),
                  MTSStatus(..),
                  MTSSide(..),
                  Verb(..),
                  OrderType(..),

                  MTSDay(..),
                  MTSTime(..),
                  MTSPico(..),
                  MTSQty(..),
                  MTSLots(..),
                  MTSYield(..),

                  Proposal(..),
                  Fill(..),
                  Order(..),

                  bidPrice,
                  signedBidPrice,
                  askPrice,
                  bidQty,
                  askQty) where


import Data.Text (Text)
import Data.Fixed (Pico)
import Data.Time (Day(), TimeOfDay(..))
import Data.Map (Map())
import GHC.Generics (Generic)


class MTSEvent a where
   date       :: a -> Day
   time       :: a -> TimeOfDay
   marketCode :: a -> Text
   bondCode   :: a -> Text
   bondType   :: a -> Text
   eventID    :: a -> Int

class MTSOneSidedEvent a where
   qty         :: a -> Quantity
   price       :: a -> Price
   verb        :: a -> Verb
   signedPrice :: a -> Price
   signedPrice x = case verb x of Sell -> negate . price $ x
                                  Buy  -> price x

type Quantity = Double
type Price = Double

data MTSStatus = Active | Suspended | Unknown deriving (Eq, Show)
data MTSSide = BothSides | AskOnly | BidOnly deriving (Eq, Show)
data Verb = Buy | Sell deriving (Eq, Show)
data OrderType = AllOrNone | FillAndKill deriving (Eq, Show)

newtype MTSDay = MTSDay { getMTSDay :: Day } deriving Show
newtype MTSTime = MTSTime { getMTSTime :: TimeOfDay } deriving Show
newtype MTSPico = MTSPico { getMTSPico :: Pico } deriving Show
newtype MTSQty = MTSQty { getMTSQty :: Double } deriving Show
newtype MTSLots = MTSLots { qtyFromLots :: Double } deriving Show
newtype MTSYield = MTSYield { getMTSYield :: Double } deriving Show


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
                         , pBidPrice    :: Price
                         , pBidQty      :: MTSLots
                         , pBidEbmQty   :: MTSLots
                         , pBidDomQty   :: MTSLots
                         , pAskPrice    :: Price
                         , pAskQty      :: MTSLots
                         , pAskEbmQty   :: MTSLots
                         , pAskDomQty   :: MTSLots
                         , pBidYield    :: MTSYield
                         , pAskYield    :: MTSYield
                         , pProposalID  :: Int
                         , pQuotingSide :: MTSSide } deriving (Show, Generic)

bidPrice :: Proposal -> Price
bidPrice = pBidPrice

signedBidPrice :: Proposal -> Price
signedBidPrice = negate . pBidPrice

askPrice :: Proposal -> Price
askPrice = pAskPrice

bidQty :: Proposal -> Quantity
bidQty = qtyFromLots . pBidQty

askQty :: Proposal -> Quantity
askQty = qtyFromLots . pAskQty

instance MTSEvent Proposal where
   date = getMTSDay . pRefDate
   time = addPico <$> getMTSTime . pUpdTime <*> getMTSPico . pUpdTimeMsec
   marketCode = pMarketCode
   bondCode = pBondCode
   bondType = pBondType
   eventID = pProposalID

instance MTSOneSidedEvent Proposal where
   qty p = case pQuotingSide p of BothSides -> error "Proposal is not one sided."
                                  AskOnly -> askQty p
                                  BidOnly -> bidQty p
   price p = case pQuotingSide p of BothSides -> error "Proposal is not one sided."
                                    AskOnly -> askPrice p
                                    BidOnly -> bidPrice p
   verb p = case pQuotingSide p of BothSides -> error "Proposal is not one sided."
                                   AskOnly -> Sell
                                   BidOnly -> Buy

data Fill = Fill { fRefDate         :: MTSDay
                 , fMarketCode      :: Text
                 , fBondCode        :: Text
                 , fTime            :: MTSTime
                 , fTimeMsec        :: MTSPico 
                 , fVerb            :: Verb
                 , fPrice           :: Price
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

instance MTSOneSidedEvent Fill where
   qty = getMTSQty . fQuantity
   price = fPrice
   verb = fVerb


data Order = Order { oMarketCode  :: Text
                   , oRefDate     :: MTSDay
                   , oRefTime     :: MTSTime
                   , oRefTimeMsec :: MTSPico 
                   , oBondCode    :: Text
                   , oBondType    :: Text
                   , oOrderSeqNo  :: Int
                   , oOrderStatus :: Text
                   , oVerb        :: Verb
                   , oPrice       :: Price
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

instance MTSOneSidedEvent Order where
   qty = getMTSQty . oQuantity
   price = oPrice
   verb = oVerb


addPico :: TimeOfDay -> Pico -> TimeOfDay
addPico (TimeOfDay h m p) p' = TimeOfDay h m (p + p')
