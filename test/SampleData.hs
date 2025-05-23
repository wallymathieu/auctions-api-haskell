{-# LANGUAGE OverloadedStrings #-}
module SampleData where
import           AuctionSite.Domain
import           AuctionSite.Domain.SingleSealedBid
import           AuctionSite.Money

import           Data.Time

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
sampleSeller :: User
sampleSeller=  BuyerOrSeller "Sample_Seller" "Seller"
sampleBuyer :: User
sampleBuyer = BuyerOrSeller "Sample_Buyer" "Buyer"

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
  bidAmount = 100
}
buyer1::User
buyer1 = BuyerOrSeller "Buyer_1" "Buyer 1"
buyer2::User
buyer2 = BuyerOrSeller "Buyer_2" "Buyer 2"
buyer3::User
buyer3 = BuyerOrSeller "Buyer_3" "Buyer 3"
bidAmount1 :: AmountValue
bidAmount1 = 10
bid1 :: Bid
bid1 =  Bid {
  bidder = buyer1,
  bidAmount = bidAmount1,
  forAuction = sampleAuctionId,
  at = addUTCTime (toEnum 1) sampleStartsAt
}
bidAmount2 :: AmountValue
bidAmount2 = 12
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
  bidAmount = 11,
  forAuction = sampleAuctionId,
  at = addUTCTime (toEnum 3) sampleStartsAt
}
