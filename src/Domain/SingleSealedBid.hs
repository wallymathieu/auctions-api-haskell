{-# LANGUAGE DeriveGeneric     #-}
module Domain.SingleSealedBid where
import Money
import Domain.Prelude
import Domain.State
import Domain.Bid
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics

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
  AcceptingBids { bidsMap:: Map.Map UserId Bid, expiry:: DateTime, opt::Options }
  | DisclosingBids { bids:: [Bid], expired:: DateTime , opt::Options }
  
instance Domain.State.State Domain.SingleSealedBid.State where
  -- inc :: DateTime -> S -> S
  inc now state = case state of
           AcceptingBids { bidsMap=bids,expiry=expiry,opt=opt }-> 
            if now>=expiry then
              let bs = Map.toList bids in
              let bids= List.map snd bs in
              DisclosingBids { bids=bids, expired=expiry, opt=opt}
            else
              state
           DisclosingBids {}-> state
  
  -- addBid :: Bid -> State -> (State, Either Errors ())
  addBid bid state = 
          let auctionId= auction bid in
          let user= bidder bid in
          case state of
           AcceptingBids { bidsMap=bids,expiry=expiry,opt=opt }-> 
             case (at bid>=expiry, Map.member user bids ) of
             (False,True) ->
              (state, Left AlreadyPlacedBid)
             (False,False) ->
              (AcceptingBids { bidsMap=Map.insert user bid bids, expiry=expiry, opt=opt }, Right ())
             (True,_) ->
              let bs = Map.toList bids in
              let bids= List.map snd bs in
              (DisclosingBids { bids=bids, expired=expiry, opt=opt},Left (AuctionHasEnded auctionId))
  
           DisclosingBids {}-> (state,Left (AuctionHasEnded auctionId))
  