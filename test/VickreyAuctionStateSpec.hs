module VickreyAuctionStateSpec where
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

  let vickreyAuction = sampleAuctionOfTyp (SingleSealedBid SB.Vickrey) 
  let emptyVickreyAuctionState = emptyState vickreyAuction
  describe "vickrey auction states" $ do
    it "can increment twice" $
      let s= S.inc sampleBidTime emptyVickreyAuctionState in
      let s2= S.inc sampleBidTime s in
          s `shouldBe` s2

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

    it "wont end just after start" $
      let state = S.inc (addUTCTime (toEnum 1) sampleStartsAt) emptyVickreyAuctionState in
      S.hasEnded state `shouldBe` False

    it "wont end just before end" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleEndsAt) emptyVickreyAuctionState in
      S.hasEnded state `shouldBe` False

    it "wont end just before start" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleStartsAt) emptyVickreyAuctionState in
      S.hasEnded state `shouldBe` False

    it "will have ended just after end" $
      let state = S.inc (addUTCTime (toEnum 1) sampleEndsAt) emptyVickreyAuctionState in
      S.hasEnded state `shouldBe` True

    incrementSpec emptyVickreyAuctionState
