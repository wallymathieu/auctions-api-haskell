{-# LANGUAGE DeriveGeneric,OverloadedStrings   #-}
module AuctionSite.Domain.Commands where
import AuctionSite.Domain.Auctions
import AuctionSite.Domain.Bids
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

instance ToJSON CommandSuccess where
  toJSON (AuctionAdded time auction) = object ["$type".= String "AuctionAdded", "at" .= toJSON time, "auction" .= toJSON auction ]
  toJSON (BidAccepted time bid) = object ["$type".= String "BidAccepted", "at" .= toJSON time, "bid" .= toJSON bid ]
