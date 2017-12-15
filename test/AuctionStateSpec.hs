module AuctionStateSpec where
import Domain.Prelude
import Domain.Auctions
import Domain.Bids
import SampleData(sek,sampleAuctionId,sampleStartsAt,sampleEndsAt,sampleAuctionOfTyp,sampleBidTime)
import Data.Time
import Money
import qualified Domain.States as S
import qualified Domain.TimedAscending as TA
import Test.Hspec
--import Debug.Trace


{-
addBidsToState state=
  let (s, e) = S.addBid (bid1()) state in 
  let (s2, e2) = S.addBid (bid2()) s in
  trace ("addBidsToState 1:"++show s)
  trace ("addBidsToState 2:"++show s2)
  s2
-}
-- let's start out with english auctions

    {-it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
      -}
spec:: ()->SpecWith ()
spec ()=do
  let buyer1 = "x2_Buyer"::UserId
  let buyer2 = "x3_Buyer"::UserId
  let bid1 =  Bid { 
    bidId= "baseless-leaf-olds-fade-sledsdebases"::BidId,
    bidder = buyer1,
    bidAmount = sek 10,
    forAuction = sampleAuctionId,
    at = addUTCTime (toEnum 1) sampleStartsAt
  }
  
  let bid2= Bid { 
    bidId= "doesbead-olds-base-ease-addedblesses"::BidId,
    bidder = buyer2,
    bidAmount = sek 12,
    forAuction = sampleAuctionId,
    at = addUTCTime (toEnum 2) sampleStartsAt
  }

  let timedAscAuction = sampleAuctionOfTyp (TimedAscending (TA.defaultOptions SEK)) 
  let emptyAscAuction = emptyState timedAscAuction
    
  describe "auction states" $ do
    it "can increment twice" $
      let s= S.inc sampleBidTime emptyAscAuction in
      let s2= S.inc sampleBidTime s in
          s `shouldBe` s2

    let (stateWith1Bid,result1) = S.addBid bid1 emptyAscAuction 
            
    it "can add bid to empty state" $
      result1 `shouldBe` Right ()

    let (stateWith2Bids,result2) = S.addBid bid2 stateWith1Bid 

    it "can add second bid" $
      result2 `shouldBe` Right ()

    let stateEnded = S.inc sampleEndsAt emptyAscAuction 

    it "can end" $
      stateEnded `shouldBe` Right (TA.HasEnded [] sampleEndsAt (TA.defaultOptions SEK))

    it "ended with two bids" $ 
      let stateEndedAfterTwoBids = S.inc sampleEndsAt stateWith2Bids in
        stateEndedAfterTwoBids `shouldBe` Right (TA.HasEnded [ bid2, bid1 ] sampleEndsAt (TA.defaultOptions SEK))

      {-
    it "cant bid after auction has ended" $
      let s=snd (S.addBid sampleBid (timedAscState() )) in
        s `shouldBe` Left (AuctionHasEnded 1)
-}
{-
  [<Fact>]
  let ``bid after auction has ended``() = 
    Assert.Equal( Error (AuctionHasEnded 1L), timedAscState |> S.addBid bid |> snd )

  [<Fact>]
  let ``english auction winner and price``() = 
    let maybeAmountAndWinner = S.tryGetAmountAndWinner timedAscState 
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``vickrey auction winner and price``() = 
    let state= auctionOfTyp (SingleSealedBid Vickrey) 
               |> Auction.emptyState 
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state 
    Assert.Equal(Some(bid1.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``blind auction winner and price``() = 
    let state= auctionOfTyp (SingleSealedBid Blind) 
               |> Auction.emptyState 
               |> addBidsToState
               |> S.inc endsAt
    let maybeAmountAndWinner = S.tryGetAmountAndWinner state 
    Assert.Equal(Some(bid2.amount,bid2.user),maybeAmountAndWinner)

  [<Fact>]
  let ``english auction Can't place bid lower than highest bid``() = 
    // setup
    let (state,res) = Auction.emptyState timedAscAuction
                       |> S.addBid {bid with at=startsAt.AddHours(1.0)}
    let nextBid = {bid with at=startsAt.AddHours(2.0); id=Guid.NewGuid()}
    
    // act
    let res = S.addBid nextBid state |> snd 
    //printf "%A" res
    Assert.Equal(Error(MustPlaceBidOverHighestBid bid.amount), res)

  [<Fact>]
  let ``date within interval``() = 
    let state = Auction.emptyState timedAscAuction
                |> S.inc (startsAt.AddHours(1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  let ``date just before end``() = 
    let state = Auction.emptyState timedAscAuction
                |> S.inc (endsAt.AddHours(-1.0))
    Assert.False(S.hasEnded state)

  [<Fact>]
  let ``date just after end``() = 
    let state = Auction.emptyState timedAscAuction
                |> S.inc (endsAt.AddHours(1.0))
    Assert.True(S.hasEnded state)

-}
