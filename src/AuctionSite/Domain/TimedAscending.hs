{-# LANGUAGE DeriveGeneric, OverloadedStrings     #-}
module AuctionSite.Domain.TimedAscending where
import AuctionSite.Money
import AuctionSite.Domain.Core
import qualified AuctionSite.Domain.States as S
import AuctionSite.Domain.Bids
import Prelude hiding ((+))
import qualified Data.Aeson as A
import Text.Printf (printf)
import qualified Data.Text as T
import AuctionSite.Aeson
import Data.Ratio (numerator)
import Data.Time (NominalDiffTime, UTCTime, nominalDiffTimeToSeconds, addUTCTime, secondsToNominalDiffTime)
import GHC.Generics (Generic)
import Text.Read (readMaybe)

data Options = Options {
  {- | the seller has set a minimum sale price in advance (the 'reserve' price)
    and the final bid does not reach that price the item remains unsold
  If the reserve price is 0, that is the equivalent of not setting it. -}
  reservePrice:: Amount,
  {- | Sometimes the auctioneer sets a minimum amount by which the next bid must exceed the current highest bid.
     Having min raise equal to 0 is the equivalent of not setting it.-}
  minRaise:: Amount,
  {- | If no competing bidder challenges the standing bid within a given time frame,
     the standing bid becomes the winner, and the item is sold to the highest bidder
     at a price equal to his or her bid. -}
  timeFrame:: NominalDiffTime

} deriving (Eq, Generic)
instance Show Options where
  show Options { reservePrice=reservePrice', minRaise=minRaise', timeFrame=timeFrame' } =
    let seconds = toRational ( nominalDiffTimeToSeconds timeFrame' )
        s = numerator seconds
    in printf "English|%s|%s|%i" (show reservePrice') (show minRaise') s
instance Read Options where
  readsPrec _ value = interpret $ T.splitOn "|" (T.pack value)
    where
      interpret :: [T.Text] -> [(Options,String)]
      interpret ["English",a,b,c] =
        let reservePrice' = readMaybe $ T.unpack a
            minRaise'     = readMaybe $ T.unpack b
            timeframe'    = readMaybe $ T.unpack c
        in
          case (reservePrice', minRaise', timeframe') of
            (Just reservePrice'', Just minRaise'', Just timeframe'') ->
              [(Options {reservePrice= reservePrice'', minRaise= minRaise'', timeFrame = secondsToNominalDiffTime timeframe''},"")]
            _ -> []
      interpret _ = []

defaultOptions:: Currency->Options
defaultOptions c = Options { reservePrice =Amount c 0, minRaise =Amount c 0, timeFrame = 0::NominalDiffTime }

data State =
  AwaitingStart UTCTime UTCTime Options
  | OnGoing  [Bid] UTCTime Options
  | HasEnded [Bid] UTCTime Options
  deriving(Eq, Generic, Show)

emptyState :: UTCTime -> UTCTime -> Options -> State
emptyState =AwaitingStart

instance S.State State where

  inc now state =
    case state of
      AwaitingStart start startingExpiry opt ->
        case (now > start, now < startingExpiry) of
          (True,True) -> -- AwaitingStart -> OnGoing
            OnGoing [] startingExpiry opt
          (True, False) -> -- AwaitingStart -> HasEnded
            HasEnded [] startingExpiry opt
          _ -> -- AwaitingStart -> AwaitingStart
            state
      OnGoing bids nextExpiry opt ->
        if now<nextExpiry then -- OnGoing -> OnGoing
          state
        else -- OnGoing -> HasEnded
          HasEnded bids nextExpiry opt
      HasEnded {} ->
        -- HasEnded -> HasEnded
        state

  addBid bid state =
    let now = at bid
        auctionId = forAuction bid
        bAmount = bidAmount bid
        next = S.inc now state
    in
    case next of
      AwaitingStart {} ->
        (next,Left (AuctionHasNotStarted auctionId))
      OnGoing bids nextExpiry opt ->
        case bids of
        [] ->
          let nextExpiry' = max nextExpiry (addUTCTime (timeFrame opt) now)
          in -- OnGoing -> OnGoing B++
          (OnGoing (bid:bids) nextExpiry' opt, Right ())
        highestBid:_ ->
          -- you cannot bid lower than the "current bid"
          let highestBidAmount = bidAmount highestBid
              nextExpiry' = max nextExpiry (addUTCTime (timeFrame opt) now)
              minRaiseAmount = minRaise opt
          in
          if bAmount > highestBidAmount + minRaiseAmount then
            -- OnGoing -> OnGoing B++
            (OnGoing (bid:bids) nextExpiry' opt, Right ())
          else
            -- OnGoing -> OnGoing !
            (next, Left (MustPlaceBidOverHighestBid highestBidAmount))
      HasEnded { } ->
        -- HasEnded -> HasEnded !
        (next, Left (AuctionHasEnded auctionId))

  tryGetAmountAndWinner state =
    case state of
    HasEnded (bid:_) _ opt ->
      if reservePrice opt < bidAmount bid then
        Just (bidAmount bid, userId $ bidder bid)
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

instance A.ToJSON Options where
  toJSON = toJsonOfShow

instance A.FromJSON Options where
  parseJSON = ofJsonOfRead "TimedAscendingOptions"
