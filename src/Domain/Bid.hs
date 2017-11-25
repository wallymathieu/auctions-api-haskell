module Domain.Bid where
import Money
import Domain.Prelude
    
data Bid = Bid { bidId :: BidId,
  auction :: AuctionId,
  bidder :: UserId,
  at :: DateTime,
  amount :: Amount
}
