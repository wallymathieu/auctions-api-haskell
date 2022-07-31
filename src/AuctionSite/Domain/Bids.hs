{-# LANGUAGE DeriveGeneric     #-}
module AuctionSite.Domain.Bids where
import AuctionSite.Money
import AuctionSite.Domain.Prelude
import GHC.Generics
import Data.Time
import Data.Aeson

data Bid = Bid { bidId :: BidId,
  forAuction :: AuctionId,
  bidder :: UserId,
  at :: UTCTime,
  bidAmount :: Amount
} deriving (Eq, Generic, Show)

instance ToJSON Bid
instance FromJSON Bid  
