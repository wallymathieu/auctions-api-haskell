module Domain.Prelude (module Domain.Prelude) where

import Money
import Data.Time

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
