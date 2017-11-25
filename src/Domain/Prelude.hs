module Domain.Prelude where
    
import Money
type UserId = String

type BidId = String

type AuctionId = Integer

-- milliseconds to begin with
type TimeSpan = Integer 
-- milliseconds to begin with
type DateTime = Integer

data Bid = Bid { bidId :: BidId,
  auction :: AuctionId,
  bidder :: UserId,
  at :: DateTime,
  amount :: Amount
}

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
  | InvalidUserData String
  | MustPlaceBidOverHighestBid Amount
  | AlreadyPlacedBid

class State a where
  inc :: DateTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())