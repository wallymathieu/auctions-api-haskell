module VickreyAuctionStateSpec where
import AuctionSite.Domain.Prelude
import AuctionSite.Domain.Auctions
import AuctionSite.Domain.Bids
import SampleData
import Data.Time
import qualified AuctionSite.Domain.States as S
import qualified AuctionSite.Domain.SingleSealedBid as SB
import Test.Hspec
import AuctionStateSpecs

spec:: ()->SpecWith ()
spec ()=do

  let vickreyAuction = sampleAuctionOfTyp (SingleSealedBid SB.Vickrey) 
  let emptyVickreyAuctionState = emptyState vickreyAuction
  describe "vickrey auction states" $ do
    let (stateWith1Bid,result1) = S.addBid bid1 emptyVickreyAuctionState 
            
    it "can add bid to empty state" $
      result1 `shouldBe` Right ()

    let (stateWith2Bids,result2) = S.addBid bid2 stateWith1Bid 
            
    it "can add second bid" $
      result2 `shouldBe` Right ()

    let stateEndedAfterTwoBids = S.inc sampleEndsAt stateWith2Bids
      
    it "can end" $
      stateEndedAfterTwoBids `shouldBe` Left (SB.DisclosingBids [ bid2, bid1 ] sampleEndsAt SB.Vickrey)

    it "can get winner and price from an ended auction" $
      let maybeAmountAndWinner = S.tryGetAmountAndWinner stateEndedAfterTwoBids in
      maybeAmountAndWinner `shouldBe` Just (bidAmount1, buyer2)

    incrementSpec emptyVickreyAuctionState
