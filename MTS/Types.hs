{-# LANGUAGE DeriveGeneric #-}

module MTS.Types (MTSDay(..),
                  MTSTime(..),
		  MTSPico(..),
		  MTSStatus(..),
		  MTSSide(..),
                  MTSQty(..),
		  MTSYield(..),
		  Proposal(..),
		  Fill(..),
		  Order(..)) where

import Data.Text (Text)
import Data.Fixed (Pico)
import GHC.Generics (Generic)
import Data.Time (Day(), TimeOfDay())
import Data.Map (Map())

newtype MTSDay = MTSDay { getMTSDay :: Day } deriving Show
newtype MTSTime = MTSTime { getMTSTime :: TimeOfDay } deriving Show
newtype MTSPico = MTSPico { getMTSPico :: Pico } deriving Show
data MTSStatus = Active | Suspended | Unknown deriving (Eq, Show)
data MTSSide = BothSides | AskOnly | BidOnly deriving (Eq, Show)
newtype MTSQty = MTSQty { getMTSQty :: Double } deriving Show
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
                         , pBidPrice    :: Double
                         , pBidQty      :: MTSQty
                         , pBidEbmQty   :: MTSQty
                         , pBidDomQty   :: MTSQty
                         , pAskPrice    :: Double
                         , pAskQty      :: MTSQty
                         , pAskEbmQty   :: MTSQty
                         , pAskDomQty   :: MTSQty
                         , pBidYield    :: MTSYield
                         , pAskYield    :: MTSYield
			 , pProposalID  :: Int
			 , pQuotingSide :: MTSSide } deriving (Show, Generic)

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

data Order = Order { oMarketCode  :: Text
                   , oRefDate     :: MTSDay
                   , oRefTime     :: MTSTime
                   , oRefTimeMsec :: MTSPico 
                   , oBondCode    :: Text
                   , oBondType    :: Text
		   , oOrderSeqNo  :: Int
                   , oOrderStatus :: Text
		   , oVerb        :: Int
                   , oPrice       :: Double
                   , oQuantity    :: MTSQty
		   , oFillNo      :: Double
		   , oOrderType   :: Text } deriving (Show, Generic)
