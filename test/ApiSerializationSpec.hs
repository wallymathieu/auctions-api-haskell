{-# LANGUAGE OverloadedStrings #-}
module ApiSerializationSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import SampleData
import Data.Time
import qualified AuctionSite.Domain.Auctions as A
import qualified AuctionSite.Web.Types as T
import qualified AuctionSite.Domain.TimedAscending as DT
import qualified AuctionSite.Money as M

spec:: ()->SpecWith ()
spec ()=do
  let zeroVAC = M.Amount M.VAC 0

  describe "auction deserialization" $ do
    let startsAt = read "2016-01-01 00:00:00.000000 UTC"::UTCTime 
    let endsAt = read "2016-02-01 00:00:00.000000 UTC"::UTCTime 
    let firstAuction = decode "{ \"id\":1,\"startsAt\":\"2016-01-01T00:00:00.000Z\",\"endsAt\":\"2016-02-01T00:00:00.000Z\",\"title\":\"First auction\" }" :: Maybe T.AddAuctionReq

    it "can understand first auction" $
      let expected = T.AddAuctionReq { T.id = sampleAuctionId,
                                     T.title = "First auction",
                                     T.startsAt = startsAt, T.endsAt = endsAt,
                                     T.currency = M.VAC,
                                     T.typ = A.TimedAscending $ DT.Options { DT.reservePrice = zeroVAC, DT.minRaise = zeroVAC, DT.timeFrame = 0.0 } }
      in firstAuction `shouldBe` Just expected

  describe "bid deserialization" $ do
    let bidOfTen = decode "{ \"amount\":10 }" :: Maybe T.BidReq

    it "can understand first auction" $
      let expected = T.BidReq { T.amount = 10 }
      in bidOfTen `shouldBe` Just expected
