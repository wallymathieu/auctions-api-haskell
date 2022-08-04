{-# LANGUAGE OverloadedStrings #-}
module SampleData where
import           Data.Time
import           Data.Aeson (Value(..), object, (.=))

import           AuctionSite.Domain.Prelude
import           AuctionSite.Domain.Bids
import           AuctionSite.Domain.Auctions
import           AuctionSite.Domain.SingleSealedBid
import           AuctionSite.Money


sampleAuctionId ::AuctionId
sampleAuctionId = 1::AuctionId
sampleTitle :: String
sampleTitle = "auction"
sampleStartsAt :: UTCTime
sampleStartsAt = read "2016-01-01 08:28:00.607875 UTC"::UTCTime 
sampleEndsAt :: UTCTime
sampleEndsAt = read "2016-02-01 08:28:00.607875 UTC"::UTCTime
sampleBidTime :: UTCTime
sampleBidTime = read "2016-02-01 07:28:00.607875 UTC"::UTCTime
sampleSeller :: UserId
sampleSeller=  "Sample_Seller"::UserId 
sampleBuyer :: UserId
sampleBuyer = "Sample_Buyer"::UserId

sampleAuctionOfTyp:: AuctionType -> Auction
sampleAuctionOfTyp typ' = Auction { auctionId = sampleAuctionId,
  title = sampleTitle,
  startsAt = sampleStartsAt,
  expiry = sampleEndsAt,
  seller = sampleSeller,
  auctionCurrency= SEK,
  typ= typ'
}

sampleAuction:: Auction
sampleAuction =sampleAuctionOfTyp (SingleSealedBid Vickrey)

sek :: Integer -> Amount
sek = Amount SEK
sampleBid :: Bid
sampleBid = Bid {
  forAuction =sampleAuctionId,
  bidder = sampleBuyer,
  at = sampleBidTime,
  bidAmount = sek 100
}
buyer1::UserId
buyer1 = "Buyer_1"::UserId
buyer2::UserId
buyer2 = "Buyer_2"::UserId
buyer3::UserId
buyer3 = "Buyer_3"::UserId
bidAmount1 :: Amount
bidAmount1 = sek 10
bid1 :: Bid
bid1 =  Bid {
  bidder = buyer1,
  bidAmount = bidAmount1,
  forAuction = sampleAuctionId,
  at = addUTCTime (toEnum 1) sampleStartsAt
}
bidAmount2 :: Amount
bidAmount2 = sek 12
bid2 :: Bid
bid2 = Bid {
  bidder = buyer2,
  bidAmount = bidAmount2,
  forAuction = sampleAuctionId,
  at = addUTCTime (toEnum 2) sampleStartsAt
}
bid_less_than_2 :: Bid
bid_less_than_2 = Bid {
  bidder = buyer3,
  bidAmount = sek 11,
  forAuction = sampleAuctionId,
  at = addUTCTime (toEnum 3) sampleStartsAt
}
