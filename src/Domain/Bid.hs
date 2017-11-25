{-# LANGUAGE DeriveGeneric     #-}
module Domain.Bid where
import Money
import Domain.Prelude
import GHC.Generics

data Bid = Bid { bidId :: BidId,
  auction :: AuctionId,
  bidder :: UserId,
  at :: DateTime,
  amount :: Amount
} deriving (Generic, Show)
