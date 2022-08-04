{-# LANGUAGE DeriveGeneric     #-}
module AuctionSite.Domain.SingleSealedBid where
import AuctionSite.Domain.Core
import qualified AuctionSite.Domain.States as S
import AuctionSite.Domain.Bids
import qualified Data.Map as Map
import qualified Data.List as List
import GHC.Generics
import Data.Time
import qualified Data.Aeson as A
import Data.Ord

data Options =
  {- Sealed first-price auction 
    In this type of auction all bidders simultaneously submit sealed bids so that no bidder knows the bid of any
    other participant. The highest bidder pays the price they submitted.
    This type of auction is distinct from the English auction, in that bidders can only submit one bid each.-}
  Blind
  {- Also known as a sealed-bid second-price auction.
    This is identical to the sealed first-price auction except that the winning bidder pays the second-highest bid
    rather than his or her own -}
  |Vickrey deriving (Eq, Generic, Show)

data State =
  AcceptingBids (Map.Map UserId Bid) UTCTime Options
  | DisclosingBids [Bid] UTCTime Options
  deriving(Eq, Generic, Show)
  
emptyState :: UTCTime -> Options -> State
emptyState=AcceptingBids Map.empty

instance S.State State where

  inc now state = case state of
    AcceptingBids bids expiry opt-> 
      if now>=expiry then
        DisclosingBids (List.sortOn (Down . bidAmount ) $ Map.elems bids) expiry opt
      else
        state
    DisclosingBids {}-> state

  addBid bid state = 
    let auctionId= forAuction bid
        user= bidder bid
    in
    case state of
      AcceptingBids bids expiry opt-> 
        case (at bid>=expiry, Map.member user bids ) of
          (False,True) ->
            (state, Left AlreadyPlacedBid)
          (False,False) ->
            (AcceptingBids (Map.insert user bid bids) expiry opt, Right ())
          (True,_) ->
            (DisclosingBids (List.sortOn (Down . bidAmount ) $ Map.elems bids) expiry opt,Left (AuctionHasEnded auctionId))

      DisclosingBids {}-> (state,Left (AuctionHasEnded auctionId))
  
  getBids state = 
    case state of
      AcceptingBids  bids _ _-> Map.elems bids
      DisclosingBids bids _ _ -> bids

  tryGetAmountAndWinner state = 
    case state of
      AcceptingBids { } -> Nothing
      DisclosingBids  (highestBid : (secondHighest : _))  _ Vickrey  -> 
        Just (bidAmount secondHighest, bidder highestBid)
      DisclosingBids [highestBid] _ Vickrey -> 
        Just (bidAmount highestBid, bidder highestBid)
      DisclosingBids (highestBid : _) _ Blind ->
        Just (bidAmount highestBid, bidder highestBid)
      _ -> Nothing

  hasEnded state = 
    case state of
      AcceptingBids { }-> False
      DisclosingBids { } -> True
  

instance A.ToJSON Options
instance A.FromJSON Options

