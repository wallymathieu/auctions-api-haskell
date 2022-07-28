{-# LANGUAGE DeriveGeneric     #-}
module Domain.Commands (module Domain.Commands) where
import Domain.Auctions
import Domain.Bids
import GHC.Generics
import Data.Time
import Data.Aeson

data Command = 
  AddAuction UTCTime Auction
  | PlaceBid UTCTime Bid
  deriving (Generic, Show)

data CommandSuccess = 
  AuctionAdded UTCTime Auction
  | BidAccepted UTCTime Bid
  deriving (Generic, Show)

instance ToJSON Command
instance FromJSON Command

instance ToJSON CommandSuccess
instance FromJSON CommandSuccess
