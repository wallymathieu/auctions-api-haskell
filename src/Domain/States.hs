module Domain.States where
import Domain.Prelude
import Domain.Bids
import Data.Time
import Money
import qualified Either as E

class State a where
  inc :: UTCTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
  getBids :: a -> [Bid]
  tryGetAmountAndWinner:: a -> Maybe (Amount , UserId)
  hasEnded:: a -> Bool

instance (State a , State b) => (State (Either a b)) where
  inc now = E.mapBoth (inc now) (inc now)

  addBid bid state =
    let res = E.mapBoth (addBid bid) (addBid bid) state in 
      E.splitFstJoinSnd res
  getBids state=
    let res = E.mapBoth getBids getBids state in
      E.join res
  tryGetAmountAndWinner state=
    let res = E.mapBoth tryGetAmountAndWinner tryGetAmountAndWinner state in
      E.join res
  hasEnded state=
    let res = E.mapBoth hasEnded hasEnded state in
      E.join res
