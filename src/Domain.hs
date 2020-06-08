module Domain (
  module Domain,
  module Domain.Prelude,
  module Domain.Auctions,
  module Domain.Bids,
  module Domain.Commands,
  module Domain.States
) where
import qualified Data.Map as Map
import qualified Data.List as List
import Domain.Prelude
import Domain.Auctions
import Domain.Bids
import Domain.Commands
import Domain.States

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
    Just (auction,state') ->
      validateBid bid auction >>= (\()->
        let (next, res)= addBid bid state' in
        let newR= Map.insert aId (auction, next) r in
          res >>= (\() -> Right (newR, BidAccepted time bid) )
      )
    Nothing -> 
      Left (UnknownAuction aId)
