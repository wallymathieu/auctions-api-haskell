{-# LANGUAGE DeriveGeneric  #-}
module AuctionSite.Domain.Auctions where

import AuctionSite.Money
import AuctionSite.Domain.Core
import AuctionSite.Domain.Bids
import qualified AuctionSite.Domain.TimedAscending as TA
import qualified AuctionSite.Domain.SingleSealedBid as SB
import GHC.Generics
import Data.Time
import Data.Aeson

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TA.Options
  | SingleSealedBid SB.Options
  deriving (Eq, Generic, Show)

data Auction = Auction { auctionId :: AuctionId,
  startsAt :: UTCTime,
  title :: String,
  -- initial expiry
  expiry :: UTCTime,
  seller :: UserId,
  typ :: AuctionType,
  auctionCurrency :: Currency
} deriving (Eq, Generic, Show)

validateBid:: Bid->Auction->Either Errors ()
validateBid bid auction
  | bidder bid == seller auction =
    Left (SellerCannotPlaceBids (bidder bid, auctionId auction))
  | amountCurrency (bidAmount bid) /= auctionCurrency auction =
    Left (CurrencyConversion (auctionCurrency auction))
  | otherwise = Right ()

type AuctionState = Either SB.State TA.State
  
emptyState :: Auction -> AuctionState
emptyState a =
  case typ a of
  SingleSealedBid opt -> Left (SB.emptyState (expiry a) opt)
  TimedAscending opt -> Right (TA.emptyState (startsAt a) (expiry a) opt)



instance ToJSON AuctionType
instance FromJSON AuctionType
instance ToJSON Auction
instance FromJSON Auction
