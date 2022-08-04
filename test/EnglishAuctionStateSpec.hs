module EnglishAuctionStateSpec where
import AuctionSite.Domain.Prelude
import AuctionSite.Domain.Auctions
import SampleData
import AuctionSite.Money
import qualified AuctionSite.Domain.States as S
import qualified AuctionSite.Domain.TimedAscending as TA
import Test.Hspec
import AuctionStateSpecs

spec:: ()->SpecWith ()
spec ()=do
  let timedAscAuction = sampleAuctionOfTyp (TimedAscending (TA.defaultOptions SEK)) 
  let emptyAscAuctionState = emptyState timedAscAuction
  let emptyEndedAscAuctionState = S.inc sampleEndsAt emptyAscAuctionState
  
  describe "english auction states" $ do
    let (stateWith1Bid,result1) = S.addBid bid1 emptyAscAuctionState 
            
    it "can add bid to empty state" $
      result1 `shouldBe` Right ()

    let (stateWith2Bids,result2) = S.addBid bid2 stateWith1Bid 

    it "can add second bid" $
      result2 `shouldBe` Right ()

    let stateEnded = S.inc sampleEndsAt emptyAscAuctionState 

    it "can end" $
      stateEnded `shouldBe` Right (TA.HasEnded [] sampleEndsAt (TA.defaultOptions SEK))

    let stateEndedAfterTwoBids = S.inc sampleEndsAt stateWith2Bids

    it "ended with two bids" $ 
      stateEndedAfterTwoBids `shouldBe` Right (TA.HasEnded [ bid2, bid1 ] sampleEndsAt (TA.defaultOptions SEK))

    it "cant bid after auction has ended" $
      let errAfterEnded=snd (S.addBid sampleBid stateEndedAfterTwoBids)
      in errAfterEnded `shouldBe` Left (AuctionHasEnded 1)
  
    it "can get winner and price from an auction" $
      let maybeAmountAndWinner = S.tryGetAmountAndWinner stateEndedAfterTwoBids
      in maybeAmountAndWinner `shouldBe` Just (bidAmount2, buyer2)

    let (_,maybeFail) = S.addBid bid_less_than_2 stateWith2Bids
    it "can't place bid lower than highest bid" $
      maybeFail `shouldBe` Left (MustPlaceBidOverHighestBid bidAmount2)

    incrementSpec emptyAscAuctionState

