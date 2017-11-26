{-# LANGUAGE DeriveGeneric     #-}
module Domain.Bids where
import Money
import Domain.Prelude
import GHC.Generics
import Data.Time

data Bid = Bid { bidId :: BidId,
  forAuction :: AuctionId,
  bidder :: UserId,
  at :: UTCTime,
  bidAmount :: Amount
} deriving (Generic, Show)

