{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.Bids (module Domain.Bids) where
import Money
import Domain.Prelude
import GHC.Generics
import Data.Time
import Data.Aeson

data Bid = Bid { bidId :: BidId,
  forAuction :: AuctionId,
  bidder :: UserId,
  at :: UTCTime,
  bidAmount :: Amount
} deriving (Eq, Generic, Show, Read)

instance ToJSON Bid where
  toJSON (Bid bidId forAuction bidder at bidAmount) = object ["id" .= bidId, "auction" .= forAuction, "user" .=bidder, "at".=at, "amount".=bidAmount]

instance FromJSON Bid where
  parseJSON = withObject "Bid" $ \v -> Bid <$> v .: "id" <*> v .: "auction" <*> v .: "user" <*> v .: "at" <*> v .: "amount"
