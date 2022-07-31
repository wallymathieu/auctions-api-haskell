module AuctionSite.Domain.Prelude where

import AuctionSite.Money

type UserId = String
type BidId = String
type AuctionId = Integer 

data Errors = 
  UnknownAuction AuctionId
  | UnknownBid BidId
  | BidAlreadyExists BidId
  | AuctionAlreadyExists AuctionId
  | AuctionHasEnded AuctionId
  | AuctionHasNotStarted AuctionId
  | AuctionNotFound AuctionId
  | SellerCannotPlaceBids (UserId , AuctionId)
  | BidCurrencyConversion (BidId , Currency)
  | CurrencyConversion
  | InvalidUserData String
  | MustPlaceBidOverHighestBid Amount
  | AlreadyPlacedBid
  deriving (Eq,Show)
  