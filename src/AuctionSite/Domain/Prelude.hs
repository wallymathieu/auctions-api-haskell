module AuctionSite.Domain.Prelude where

import AuctionSite.Money

type UserId = String
type AuctionId = Integer 

data Errors = 
  UnknownAuction AuctionId
  | AuctionAlreadyExists AuctionId
  | AuctionHasEnded AuctionId
  | AuctionHasNotStarted AuctionId
  | SellerCannotPlaceBids (UserId , AuctionId)
  | CurrencyConversion Currency
  | InvalidUserData String
  | MustPlaceBidOverHighestBid Amount
  | AlreadyPlacedBid
  deriving (Eq,Show)
  