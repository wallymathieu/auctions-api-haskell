module BlindAuctionStateSpec where
import Domain.Prelude
import Domain.Auctions
import Domain.Bids
import SampleData
import Data.Time
import qualified Domain.States as S
import qualified Domain.SingleSealedBid as SB
import Test.Hspec
import AuctionStateSpecs 

spec:: ()->SpecWith ()
spec ()=do

  let blindAuction = sampleAuctionOfTyp (SingleSealedBid SB.Blind) 
  let emptyBlindAuctionState = emptyState blindAuction
  describe "blind auction states" $ do
    let (stateWith1Bid,result1) = S.addBid bid1 emptyBlindAuctionState 
            
    it "can add bid to empty state" $
      result1 `shouldBe` Right ()

    let (stateWith2Bids,result2) = S.addBid bid2 stateWith1Bid 
            
    it "can add second bid" $
      result2 `shouldBe` Right ()

    let stateEndedAfterTwoBids = S.inc sampleEndsAt stateWith2Bids
      
    it "can end" $
      stateEndedAfterTwoBids `shouldBe` Left (SB.DisclosingBids [ bid2, bid1 ] sampleEndsAt SB.Blind)

    it "can get winner and price from an ended auction" $
      let maybeAmountAndWinner = S.tryGetAmountAndWinner stateEndedAfterTwoBids in
      maybeAmountAndWinner `shouldBe` Just (bidAmount2, buyer2)

    incrementSpec emptyBlindAuctionState
