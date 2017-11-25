module Domain.Auction where
    
import Money
import Domain.Prelude
import Domain.TimedAscending as TimedAscending
import Domain.SingleSealedBid as SingleSealedBid

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TimedAscending.Options
  | SingleSealedBid SingleSealedBid.Options

data Auction = Auction { auctionId :: AuctionId,
  startsAt :: DateTime,
  title :: String,
  -- initial expiry
  expiry :: DateTime,
  seller :: UserId,
  typ :: AuctionType,
  currency :: Currency
}
