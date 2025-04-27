{-# LANGUAGE OverloadedStrings,DeriveGeneric     #-}
module AuctionSite.Domain.Bids where
import AuctionSite.Money
import AuctionSite.Domain.Core
import GHC.Generics
import Data.Time
import Data.Aeson

data Bid = Bid {
  forAuction :: AuctionId,
  bidder :: User,
  at :: UTCTime,
  bidAmount :: AmountValue
} deriving (Eq, Generic, Show)

instance ToJSON Bid where
  toJSON Bid { forAuction=auctionId', bidder=bidder', at=at', bidAmount=bidAmount' } =
    object [ "auction".=auctionId',
             "user" .= bidder',
             "at" .= at',
             "amount" .= bidAmount' ]

instance FromJSON Bid where
  parseJSON = withObject "Bid" $ \obj -> do
    auctionId' <- obj .: "auction"
    bidder' <- obj .: "user"
    at' <- obj .: "at"
    bidAmount' <- obj .: "amount"
    return (Bid { forAuction = auctionId', bidder = bidder', at = at', bidAmount = bidAmount' })
