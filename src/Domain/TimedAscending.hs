{-# LANGUAGE DeriveGeneric     #-}
module Domain.TimedAscending (module Domain.TimedAscending) where
import Money
import Domain.Prelude
import qualified Domain.States as S
import Domain.Bids
import GHC.Generics
import Data.Time
import Debug.Trace

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
} deriving (Eq, Generic, Show)

defaultOptions:: Currency->Options
defaultOptions c = Options { reservePrice =Amount c 0, minRaise =Amount c 0, timeFrame = 0::NominalDiffTime }

data State =
  AwaitingStart { start:: UTCTime , startingExpiry:: UTCTime , opt::Options }
  | OnGoing  [Bid] UTCTime Options
  | HasEnded [Bid] UTCTime Options
  deriving(Eq, Generic, Show)

emptyState :: UTCTime -> UTCTime -> Options -> State
emptyState startsAt expiry opt=AwaitingStart{ start=startsAt, startingExpiry= expiry, opt= opt}
  
instance S.State Domain.TimedAscending.State where

  inc now state = 
    case state of 
        AwaitingStart {start=start, startingExpiry=startingExpiry, opt=opt} ->
            case (now > start, now < startingExpiry) of 
                (True,True) ->trace "inc: AwaitingStart -> OnGoing " OnGoing [] startingExpiry opt
                (True, False) ->trace "inc: AwaitingStart -> HasEnded " HasEnded [] startingExpiry opt
                _ -> trace "inc: AwaitingStart -> AwaitingStart"
                  state
        OnGoing bids nextExpiry opt ->
            if now<nextExpiry then
                trace "inc: OnGoing -> OnGoing"
                state
              else
                trace "inc: OnGoing -> HasEnded"
                HasEnded bids nextExpiry opt
        HasEnded {} ->
          trace "inc: HasEnded -> HasEnded"
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
                    trace "addBid: AwaitingStart -> OnGoing B++"
                    (OnGoing [bid] nextExpiry opt, Right ())
                (True, False) -> (HasEnded [] startingExpiry opt, Right ())
                _ -> trace "addBid: AwaitingStart -> AwaitingStart !"
                  (state,Left (AuctionHasNotStarted auctionId))
        OnGoing bids nextExpiry opt ->
            if now<nextExpiry then
                case bids of
                [] -> 
                    let nextExpiry = max nextExpiry (addUTCTime (timeFrame opt) now) in
                      trace "addBid: OnGoing -> OnGoing B++"
                      (OnGoing (bid:bids) nextExpiry opt, Right())
                highestBid:xs -> 
                    -- you cannot bid lower than the "current bid"
                    let highestBidAmount = bidAmount highestBid in
                    let nextExpiry = max nextExpiry (addUTCTime (timeFrame opt) now) in
                    let minRaiseAmount = minRaise opt in
                    if bAmount > amountAdd highestBidAmount minRaiseAmount then
                        trace "addBid: OnGoing -> OnGoing B++"
                        (OnGoing (bid:bids) nextExpiry opt, Right())
                    else 
                      trace "addBid: OnGoing -> OnGoing !"
                      (state, Left (MustPlaceBidOverHighestBid highestBidAmount))
            else
              trace "addBid: OnGoing -> HasEnded !"
              (HasEnded bids nextExpiry opt, Left (AuctionHasEnded auctionId))
        HasEnded { } ->
          trace "addBid: HasEnded -> HasEnded !"
          (state, Left (AuctionHasEnded auctionId))

  tryGetAmountAndWinner state = 
    case state of
    HasEnded (bid:rest) expired opt -> 
      if reservePrice opt < bidAmount bid then
        Just (bidAmount bid, bidder bid)
      else
        Nothing
    _ -> Nothing

  getBids state=
    case state of
    OnGoing bids _ _ -> bids
    HasEnded bids _ _ -> bids
    AwaitingStart {} ->[]

  hasEnded state=
    case state of
    HasEnded {} -> True 
    _ -> False
