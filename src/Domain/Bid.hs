{-# LANGUAGE DeriveGeneric     #-}
module Domain.Bid where
import Money
import Domain.Prelude
import GHC.Generics
import Data.Time

data Bid = Bid { bidId :: BidId,
  auction :: AuctionId,
  bidder :: UserId,
  at :: UTCTime,
  amount :: Amount
} deriving (Generic, Show)
