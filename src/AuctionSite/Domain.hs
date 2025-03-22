module AuctionSite.Domain (
  -- auctions
  AuctionType (..),
  Auction (..),
  emptyState,
  validateBid,
  AuctionState,
  -- bids
  Bid (..),
  -- core
  Errors (..),
  UserId,
  AuctionId,
  User (..),
  userId,
  -- state
  State (..),
  Repository,
  -- commands
  Command (..),
  Event (..),
  -- domain
  auctions,
  handle
) where
import qualified Data.Map as Map
import qualified Data.List as List
import AuctionSite.Domain.Core
import AuctionSite.Domain.Auctions
import AuctionSite.Domain.Bids
import AuctionSite.Domain.Commands
import AuctionSite.Domain.States

type Repository = Map.Map AuctionId (Auction, AuctionState)

auctions::Repository -> [Auction]
auctions r = List.map fst (Map.elems r)
handle:: Command -> Repository -> (Either Errors Event, Repository)
handle state repository =
  case state of
  AddAuction time auction ->
    let aId = auctionId auction
    in
    if not (Map.member aId repository) then
      let empty = emptyState auction
          nextRepository= Map.insert aId (auction, empty) repository
      in successOf (AuctionAdded time auction) nextRepository
    else
      failureOf $ AuctionAlreadyExists aId
  PlaceBid time bid ->
    let aId = forAuction bid
    in
    case Map.lookup aId repository of
    Just (auction,state') ->
      case validateBid bid auction of
      Right _ ->
        let (nextAuctionState, bidResult) = addBid bid state'
            nextRepository = Map.insert aId (auction, nextAuctionState) repository
        in
          case bidResult of
          Right _ -> successOf (BidAccepted time bid) nextRepository
          Left err -> failureOf err
      Left err -> failureOf err
    Nothing ->
      failureOf $ UnknownAuction aId
  where
    failureOf failure = (Left failure, repository)
    successOf success nextRepo = (Right success, nextRepo)

