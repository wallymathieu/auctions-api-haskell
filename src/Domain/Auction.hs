{-# LANGUAGE DeriveGeneric     #-}
module Domain.Auction where

import Money
import Domain.Prelude
import Domain.TimedAscending as TimedAscending
import Domain.SingleSealedBid as SingleSealedBid
import GHC.Generics
import Data.Time

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TimedAscending.Options
  | SingleSealedBid SingleSealedBid.Options
  deriving (Generic, Show)
  
data Auction = Auction { auctionId :: AuctionId,
  startsAt :: UTCTime,
  title :: String,
  -- initial expiry
  expiry :: UTCTime,
  seller :: UserId,
  typ :: AuctionType,
  currency :: Currency
} deriving (Generic, Show)
