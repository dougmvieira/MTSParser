module MTS.Query ( findProposalByID
                 , findProposalsInASecond
		 , findFillsByBondAndDay
		 , findOrdersByBondAndDay) where

import MTS.Types

import Data.Time (Day, TimeOfDay)
import Data.Text (pack)

findProposalByID :: Int -> [Proposal] -> [Proposal]
findProposalByID i = filter ((==i) . pProposalID)

findProposalsInASecond :: TimeOfDay -> [Proposal] -> [Proposal]
findProposalsInASecond t = filter ((==t) . getMTSTime . pUpdTime)

findFillsByBondAndDay :: String -> Day -> [Fill] -> [Fill]
findFillsByBondAndDay code d = filter (\f -> fBondCode f == (pack code) && (getMTSDay . fRefDate) f == d)

findOrdersByBondAndDay :: String -> Day -> [Order] -> [Order]
findOrdersByBondAndDay code d = filter (\o -> oBondCode o == (pack code) && (getMTSDay . oRefDate) o == d)

