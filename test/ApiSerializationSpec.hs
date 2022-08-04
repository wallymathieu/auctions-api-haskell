{-# LANGUAGE OverloadedStrings #-}
module ApiSerializationSpec where
import Data.Aeson hiding (json)
import Test.Hspec
import SampleData
import Data.Time
import           AuctionSite.Domain
import           AuctionSite.Web.Types
import qualified AuctionSite.Domain.TimedAscending as DT
import qualified AuctionSite.Money as M

spec:: ()->SpecWith ()
spec ()=do
  let zeroVAC = M.Amount M.VAC 0

  describe "auction deserialization" $ do
    let startsAt' = read "2016-01-01 00:00:00.000000 UTC"::UTCTime 
    let endsAt = read "2016-02-01 00:00:00.000000 UTC"::UTCTime 
    let firstAuction = decode "{ \"id\":1,\"startsAt\":\"2016-01-01T00:00:00.000Z\",\"endsAt\":\"2016-02-01T00:00:00.000Z\",\"title\":\"First auction\" }" :: Maybe AddAuctionReq

    it "can understand first auction" $
      let expected = AddAuctionReq { reqId = sampleAuctionId,
                                     reqTitle = "First auction",
                                     reqStartsAt = startsAt', reqEndsAt = endsAt,
                                     reqCurrency = M.VAC,
                                     reqTyp = TimedAscending $ DT.Options { DT.reservePrice = zeroVAC, DT.minRaise = zeroVAC, DT.timeFrame = 0.0 } }
      in firstAuction `shouldBe` Just expected

  describe "bid deserialization" $ do
    let bidOfTen = decode "{ \"amount\":10 }" :: Maybe BidReq

    it "can understand first auction" $
      let expected = BidReq { amount = 10 }
      in bidOfTen `shouldBe` Just expected
