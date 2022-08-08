module AuctionSite.Domain.Core where
import AuctionSite.Money

type UserId = String
data User =
  BuyerOrSeller UserId * String
  | Support UserId

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
  