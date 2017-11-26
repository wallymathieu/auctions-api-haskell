{-# LANGUAGE DeriveGeneric  #-}
module Domain.Auctions where

import Money
import Domain.Prelude
import Domain.Bids
import qualified Domain.TimedAscending as TA
import qualified Domain.SingleSealedBid as SB
import GHC.Generics
import Data.Time
import Data.Either

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TA.Options
  | SingleSealedBid SB.Options
  deriving (Generic, Show)

data Auction = Auction { auctionId :: AuctionId,
  startsAt :: UTCTime,
  title :: String,
  -- initial expiry
  expiry :: UTCTime,
  seller :: UserId,
  typ :: AuctionType,
  auctionCurrency :: Currency
} deriving (Generic, Show)

validateBid bid auction
  | bidder bid == seller auction =
    Left (SellerCannotPlaceBids (bidder bid, auctionId auction))
  | amountCurrency (bidAmount bid) /= auctionCurrency auction =
    Left (BidCurrencyConversion (bidId bid, auctionCurrency auction))
  | otherwise = Right ()

type AuctionState = Either SB.State TA.State
  
emptyState :: Auction -> AuctionState
emptyState a =
  case typ a of
  SingleSealedBid opt -> Left (SB.emptyState (expiry a) opt)
  TimedAscending opt -> Right (TA.emptyState (startsAt a) (expiry a) opt)

