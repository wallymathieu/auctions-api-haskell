module EnglishAuctionSpec where
import           AuctionSite.Domain.Core
import           AuctionSite.Domain
import qualified AuctionSite.Domain.States as S
import qualified AuctionSite.Domain.TimedAscending as TA

import           SampleData
import           AuctionStateSpecs

import           Test.Hspec
import           Text.Read (readMaybe)
import           Data.Time (secondsToNominalDiffTime)

spec:: ()->SpecWith ()
spec ()=do
  let timedAscAuction = sampleAuctionOfTyp (TimedAscending TA.defaultOptions)
  let emptyAscAuctionState = emptyState timedAscAuction
  let emptyEndedAscAuctionState = S.inc sampleEndsAt emptyAscAuctionState

  describe "english auction states" $ do
    let (stateWith1Bid,result1) = S.addBid bid1 emptyAscAuctionState

    it "can add bid to empty state" $
      result1 `shouldBe` Right ()

    let (stateWith2Bids,result2) = S.addBid bid2 stateWith1Bid

    it "can add second bid" $
      result2 `shouldBe` Right ()

    it "can end" $
      emptyEndedAscAuctionState `shouldBe` Right (TA.HasEnded [] sampleEndsAt TA.defaultOptions)

    let stateEndedAfterTwoBids = S.inc sampleEndsAt stateWith2Bids

    it "ended with two bids" $
      stateEndedAfterTwoBids `shouldBe` Right (TA.HasEnded [ bid2, bid1 ] sampleEndsAt TA.defaultOptions)

    it "cant bid after auction has ended" $
      let errAfterEnded=snd (S.addBid sampleBid stateEndedAfterTwoBids)
      in errAfterEnded `shouldBe` Left (AuctionHasEnded 1)

    it "can get winner and price from an auction" $
      let maybeAmountAndWinner = S.tryGetAmountAndWinner stateEndedAfterTwoBids
      in maybeAmountAndWinner `shouldBe` Just (bidAmount2, userId buyer2)

    let (_,maybeFail) = S.addBid bid_less_than_2 stateWith2Bids
    it "can't place bid lower than highest bid" $
      maybeFail `shouldBe` Left (MustPlaceBidOverHighestBid bidAmount2)

    incrementSpec emptyAscAuctionState
  describe "english auction type serialization" $ do
    let sampleTypStr = "English|0|0|0"
    let sampleTyp = TA.defaultOptions
    it "can deserialize sample typ" $
      let parsed = readMaybe sampleTypStr :: Maybe TA.Options
      in parsed `shouldBe` Just sampleTyp
    it "can serialize sample typ" $
      show sampleTyp `shouldBe` sampleTypStr

    let sampleWithValuesTypStr = "English|10|20|30"
    let sampleWithValuesTyp = TA.Options 10 20 (secondsToNominalDiffTime 30)
    it "can deserialize sample with values typ" $
      let parsed = readMaybe sampleWithValuesTypStr :: Maybe TA.Options
      in parsed `shouldBe` Just sampleWithValuesTyp
    it "can serialize sample with values typ" $
      show sampleWithValuesTyp `shouldBe` sampleWithValuesTypStr
