{-# LANGUAGE DeriveGeneric     #-}
module Domain.TimedAscending where
import Money
import Domain.Prelude
import Domain.State
import Domain.Bid
import GHC.Generics

data Options = Options { 
  {- the seller has set a minimum sale price in advance (the 'reserve' price) 
    and the final bid does not reach that price the item remains unsold
  If the reserve price is 0, that is the equivalent of not setting it. -}
  reservePrice:: Amount,
  {- Sometimes the auctioneer sets a minimum amount by which the next bid must exceed the current highest bid.
     Having min raise equal to 0 is the equivalent of not setting it.-}
  minRaise:: Amount,
  {- If no competing bidder challenges the standing bid within a given time frame, 
     the standing bid becomes the winner, and the item is sold to the highest bidder 
     at a price equal to his or her bid. -}
  timeFrame:: TimeSpan
} deriving (Generic, Show)


data State =
  AwaitingStart { start:: DateTime , startingExpiry:: DateTime , opt::Options }
  | OnGoing { bids:: [Bid] , nextExpiry:: DateTime , opt::Options }
  | HasEnded { bids:: [Bid] , expired:: DateTime , opt::Options }

instance Domain.State.State Domain.TimedAscending.State where
  -- inc :: DateTime -> State -> State
  inc now state = case state of 
                      AwaitingStart {start=start, startingExpiry=startingExpiry, opt=opt} ->
                          case (now > start, now < startingExpiry) of 
                              (True,True) -> OnGoing{bids=[],nextExpiry=startingExpiry, opt=opt}
                              (True, False) -> HasEnded{bids=[],expired=startingExpiry, opt=opt}
                              _ -> state
                      OnGoing {bids=bids,nextExpiry=nextExpiry, opt=opt} ->
                          if now<nextExpiry then
                              state
                            else
                              HasEnded{bids=bids, expired=nextExpiry, opt=opt}
                      HasEnded { } ->
                          state

  -- addBid :: Bid -> State -> (State, Either Errors ())
  addBid bid state = 
                  let now = at bid in
                  let auctionId = auction bid in
                  let bidAmount = amount bid in
                  case state of 
                      AwaitingStart {start=start, startingExpiry=startingExpiry, opt=opt} ->
                          case (now > start, now < startingExpiry) of 
                              (True,True) -> 
                                  let nextExpiry = max startingExpiry (now + timeFrame opt) in
                                  (OnGoing{bids=[bid], nextExpiry=nextExpiry, opt=opt}, Right ())
                              (True, False) -> (HasEnded{bids=[],expired=startingExpiry, opt=opt}, Right ())
                              _ -> (state,Left (AuctionHasEnded auctionId))
                      OnGoing {bids=bids,nextExpiry=nextExpiry, opt=opt} ->
                          if now<nextExpiry then
                              case bids of
                              [] -> 
                                  let nextExpiry = max nextExpiry (now + timeFrame opt) in
                                  (OnGoing { bids=bid:bids, nextExpiry=nextExpiry, opt=opt}, Right())
                              highestBid:xs -> 
                                  -- you cannot bid lower than the "current bid"
                                  let highestBidAmount = amount highestBid in
                                  let nextExpiry = max nextExpiry (now + timeFrame opt) in
                                  let minRaiseAmount = minRaise opt in
                                  if bidAmount > amountAdd highestBidAmount minRaiseAmount then
                                      (OnGoing { bids=bid:bids, nextExpiry=nextExpiry, opt=opt}, Right())
                                  else 
                                      (state, Left (MustPlaceBidOverHighestBid highestBidAmount))
                          else
                              (HasEnded{bids=bids, expired=nextExpiry, opt=opt}, Left (AuctionHasEnded auctionId))
                      HasEnded { } ->
                          (state, Left (AuctionHasEnded auctionId))
