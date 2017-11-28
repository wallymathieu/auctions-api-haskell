{-# LANGUAGE DeriveGeneric     #-}
module Domain.Commands (module Domain.Commands) where
import Money
import Domain.Prelude
import Domain.Auctions
import Domain.Bids
import GHC.Generics
import Data.Time

data Command = 
  AddAuction UTCTime Auction
  | PlaceBid UTCTime Bid
  deriving (Generic, Show)

data CommandSuccess = 
  AuctionAdded UTCTime Auction
  | BidAccepted UTCTime Bid
  deriving (Generic, Show)
