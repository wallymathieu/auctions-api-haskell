{-# LANGUAGE OverloadedStrings #-}
module ApiSerializationSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import SampleData
import Data.Time
import AuctionSite.Web.Types as T
import AuctionSite.Money

spec:: ()->SpecWith ()
spec ()=do

  describe "auction deserialization" $ do
    let startsAt = read "2016-01-01 00:00:00.000000 UTC"::UTCTime 
    let endsAt = read "2016-02-01 00:00:00.000000 UTC"::UTCTime 
    let firstAuction = decode "{ \"id\":1,\"startsAt\":\"2016-01-01T00:00:00.000Z\",\"endsAt\":\"2016-02-01T00:00:00.000Z\",\"title\":\"First auction\", \"currency\":\"VAC\" }" :: Maybe AddAuctionReq

    it "can understand first auction" $
      let expected = AddAuctionReq { T.id = sampleAuctionId,
                                     title = "First auction",
                                     startsAt = startsAt, endsAt = endsAt,
                                     currency = VAC,
                                     typ = Nothing } in
        firstAuction `shouldBe` Just expected

  describe "bid deserialization" $ do
    let bidOfTen = decode "{ \"amount\":10 }" :: Maybe BidReq

    it "can understand first auction" $
      let expected = BidReq { amount = 10 } in
        bidOfTen `shouldBe` Just expected
