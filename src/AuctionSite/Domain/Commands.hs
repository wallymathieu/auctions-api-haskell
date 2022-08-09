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
  deriving (Generic, Show, Eq)

data CommandSuccess =
  AuctionAdded UTCTime Auction
  | BidAccepted UTCTime Bid
  deriving (Generic, Show, Eq)


instance ToJSON Command where
  toJSON (AddAuction time auction) =  object ["$type" .= String "AddAuction", "at" .= time, "auction" .= auction]
  toJSON (PlaceBid time bid) =  object ["$type" .= String "PlaceBid", "at" .= time, "bid" .= bid]

instance FromJSON Command where
  parseJSON (Object v) = do
    typ' <-v .: "$type"
    case typ' of
      String "AddAuction" -> AddAuction <$> v .: "at" <*> v .: "auction"
      String "PlaceBid"   -> PlaceBid <$> v .: "at" <*> v .: "bid"
      _                   -> fail "Unknown command type"
  parseJSON _ = fail "Unexpected json command"

instance ToJSON CommandSuccess where
  toJSON (AuctionAdded time auction) =  object ["$type" .= String "AuctionAdded", "at" .= time, "auction" .= auction]
  toJSON (BidAccepted time bid) =  object ["$type" .= String "BidAccepted", "at" .= time, "bid" .= bid]
