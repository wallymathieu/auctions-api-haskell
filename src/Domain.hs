{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances  #-}
module Domain where
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Domain.States as S
import qualified Either as E
import Data.Either
import Domain.Prelude
import Domain.Auctions
import Domain.Bids
import Domain.Commands
import Domain.States
import qualified Domain.SingleSealedBid as SingleSealedBid
import qualified Domain.TimedAscending as TimedAscending

instance S.State AuctionState where
  inc now = E.mapBoth (S.inc now) (S.inc now)

  addBid bid state =
    let res = E.mapBoth (S.addBid bid) (S.addBid bid) state in 
      E.splitFstJoinSnd res
  getBids state=
    let res = E.mapBoth S.getBids S.getBids state in
      E.join res
  tryGetAmountAndWinner state=
    let res = E.mapBoth S.tryGetAmountAndWinner S.tryGetAmountAndWinner state in
      E.join res
  hasEnded state=
    let res = E.mapBoth S.hasEnded S.hasEnded state in
      E.join res

  
type Repository = Map.Map AuctionId (Auction, AuctionState)

auctions::Repository -> [Auction]
auctions r = List.map fst (Map.elems r)

handle:: Command -> Repository -> Either Errors (Repository,CommandSuccess)
handle state r =
  case state of 
  AddAuction time auction ->
    let aId = auctionId auction in
    if not (Map.member aId r) then
      let empty = emptyState auction in
      let newR= Map.insert aId (auction, empty) r in
      Right (newR, AuctionAdded time auction)
    else
      Left (AuctionAlreadyExists aId)
  PlaceBid time bid ->
    let aId = forAuction bid in
    case Map.lookup aId r of
    Just (auction,state) ->
      case validateBid bid auction of
      Right () ->
        let (next, res)= addBid bid state in
        let newR= Map.insert aId (auction, next) r in
        fmap ( \ () -> (newR, BidAccepted time bid) ) res
      Left err ->
        Left err
    Nothing -> 
      Left (UnknownAuction aId)


