{-# LANGUAGE DeriveGeneric     #-}
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
} deriving (Eq, Generic, Show)

instance ToJSON Bid
instance FromJSON Bid  
