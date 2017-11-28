{-# LANGUAGE DeriveGeneric     #-}
module Domain.SingleSealedBid (module Domain.SingleSealedBid) where
import Money
import Domain.Prelude
import qualified Domain.States as S
import Domain.Bids
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics
import Data.Time

data Options =
  {- Sealed first-price auction 
    In this type of auction all bidders simultaneously submit sealed bids so that no bidder knows the bid of any
    other participant. The highest bidder pays the price they submitted.
    This type of auction is distinct from the English auction, in that bidders can only submit one bid each.-}
  Blind
  {- Also known as a sealed-bid second-price auction.
    This is identical to the sealed first-price auction except that the winning bidder pays the second-highest bid
    rather than his or her own -}
  |Vickrey deriving (Generic, Show)

data State =
  AcceptingBids { bidsMap:: Map.Map UserId Bid, firstExpiry:: UTCTime, opt::Options }
  | DisclosingBids { bids:: [Bid], expired:: UTCTime , opt::Options }

emptyState :: UTCTime -> Options -> State
emptyState expiry opt=AcceptingBids{ bidsMap= Map.empty, firstExpiry= expiry, opt= opt}

instance S.State State where
  inc now state = case state of
           AcceptingBids { bidsMap=bids, firstExpiry=expiry, opt=opt }-> 
            if now>=expiry then
              DisclosingBids { bids=Map.elems bids, expired=expiry, opt=opt}
            else
              state
           DisclosingBids {}-> state

  addBid bid state = 
          let auctionId= forAuction bid in
          let user= bidder bid in
          case state of
           AcceptingBids { bidsMap=bids, firstExpiry=expiry, opt=opt }-> 
             case (at bid>=expiry, Map.member user bids ) of
             (False,True) ->
              (state, Left AlreadyPlacedBid)
             (False,False) ->
              (AcceptingBids { bidsMap=Map.insert user bid bids, firstExpiry=expiry, opt=opt }, Right ())
             (True,_) ->
              (DisclosingBids { bids=Map.elems bids, expired=expiry, opt=opt},Left (AuctionHasEnded auctionId))
  
           DisclosingBids {}-> (state,Left (AuctionHasEnded auctionId))
  
  getBids state = 
    case state of
      AcceptingBids { bidsMap=bids }-> Map.elems bids
      DisclosingBids { bids=bids} -> bids

  tryGetAmountAndWinner state = 
    case state of
      AcceptingBids { } -> Nothing
      DisclosingBids { bids= highestBid : (secondHighest : _), 
                       opt=Vickrey } -> Just (bidAmount secondHighest, bidder highestBid)
      DisclosingBids { bids= [highestBid],
                       opt=Vickrey} -> Just (bidAmount highestBid, bidder highestBid)
      DisclosingBids { bids= highestBid : _,
                       opt=Blind} -> Just (bidAmount highestBid, bidder highestBid)
      _ -> Nothing

  hasEnded state = 
    case state of
      AcceptingBids { }-> False
      DisclosingBids { } -> True
  