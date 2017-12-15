module SampleData where
import Domain.Prelude
import Domain.Bids
import Domain.Auctions
import Domain.SingleSealedBid

import Money
import Data.Time


sampleAuctionId = 1::AuctionId
sampleBidId = "calfless-bloc-able-lose-cobblebefell"::BidId
sampleTitle = "auction"
sampleStartsAt = read "2016-01-01 08:28:00.607875 UTC"::UTCTime 
sampleEndsAt = read "2016-02-01 08:28:00.607875 UTC"::UTCTime
sampleBidTime = read "2016-02-01 07:28:00.607875 UTC"::UTCTime
sampleSeller=  "x1_Seller"::UserId 
sampleBuyer = "x2_Buyer"::UserId

sampleAuctionOfTyp:: AuctionType -> Auction
sampleAuctionOfTyp typ = Auction { auctionId = sampleAuctionId,
  title = sampleTitle,
  startsAt = sampleStartsAt,
  expiry = sampleEndsAt,
  seller = sampleSeller,
  auctionCurrency= SEK,
  typ= typ
}

sampleAuction:: ()-> Auction
sampleAuction ()=sampleAuctionOfTyp (SingleSealedBid Vickrey)

sek =Amount SEK

sampleBid = Bid { bidId =sampleBidId,
  forAuction =sampleAuctionId,
  bidder = sampleBuyer,
  at = sampleBidTime,
  bidAmount = sek 100
}

