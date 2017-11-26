{-# LANGUAGE DeriveGeneric     #-}
module Domain.TimedAscending where
import Money
import Domain.Prelude
import qualified Domain.States as S
import Domain.Bids
import GHC.Generics
import Data.Time

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
  timeFrame:: NominalDiffTime
} deriving (Generic, Show)

data State =
  AwaitingStart { start:: UTCTime , startingExpiry:: UTCTime , opt::Options }
  | OnGoing { bids:: [Bid] , nextExpiry:: UTCTime , opt::Options }
  | HasEnded { bids:: [Bid] , expired:: UTCTime , opt::Options }

emptyState :: UTCTime -> UTCTime -> Options -> State
emptyState startsAt expiry opt=AwaitingStart{ start=startsAt, startingExpiry= expiry, opt= opt}
  
instance S.State Domain.TimedAscending.State where

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

  addBid bid state = 
                  let now = at bid in
                  let auctionId = forAuction bid in
                  let bAmount = bidAmount bid in
                  case state of 
                      AwaitingStart {start=start, startingExpiry=startingExpiry, opt=opt} ->
                          case (now > start, now < startingExpiry) of 
                              (True,True) -> 
                                  let nextExpiry = max startingExpiry (addUTCTime (timeFrame opt) now) in
                                  (OnGoing{bids=[bid], nextExpiry=nextExpiry, opt=opt}, Right ())
                              (True, False) -> (HasEnded{bids=[],expired=startingExpiry, opt=opt}, Right ())
                              _ -> (state,Left (AuctionHasEnded auctionId))
                      OnGoing {bids=bids,nextExpiry=nextExpiry, opt=opt} ->
                          if now<nextExpiry then
                              case bids of
                              [] -> 
                                  let nextExpiry = max nextExpiry (addUTCTime (timeFrame opt) now) in
                                  (OnGoing { bids=bid:bids, nextExpiry=nextExpiry, opt=opt}, Right())
                              highestBid:xs -> 
                                  -- you cannot bid lower than the "current bid"
                                  let highestBidAmount = bidAmount highestBid in
                                  let nextExpiry = max nextExpiry (addUTCTime (timeFrame opt) now) in
                                  let minRaiseAmount = minRaise opt in
                                  if bAmount > amountAdd highestBidAmount minRaiseAmount then
                                      (OnGoing { bids=bid:bids, nextExpiry=nextExpiry, opt=opt}, Right())
                                  else 
                                      (state, Left (MustPlaceBidOverHighestBid highestBidAmount))
                          else
                              (HasEnded{bids=bids, expired=nextExpiry, opt=opt}, Left (AuctionHasEnded auctionId))
                      HasEnded { } ->
                          (state, Left (AuctionHasEnded auctionId))

  tryGetAmountAndWinner state = 
    case state of
    HasEnded {bids=bid:rest,expired=expired, opt=opt} -> 
      if reservePrice opt < bidAmount bid then
        Just (bidAmount bid, bidder bid)
      else
        Nothing
    _ -> Nothing

  getBids state=
    case state of
    OnGoing {bids=bids}->bids
    HasEnded {bids=bids}->bids
    AwaitingStart {} ->[]

  hasEnded state=
    case state of
    HasEnded {} -> True 
    _ -> False
