module Domain.States (module Domain.States) where
import Domain.Prelude
import Domain.Bids
import Data.Time
import Data.Bifunctor
import Money
import qualified Either as E
import Debug.Trace

class State a where
  inc :: UTCTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
  getBids :: a -> [Bid]
  tryGetAmountAndWinner:: a -> Maybe (Amount , UserId)
  hasEnded:: a -> Bool

-- composing two State types as a State by using Either
instance (State a , State b) => (State (Either a b)) where
  inc now = trace "E.inc"
        bimap (inc now) (inc now) 

  addBid bid state =
    {- 
    split a tuple of (Either (a,b) (c,b)) where the second element is of the same type into
    ((Either a c),b)
    -}
    let splitFstJoinSnd c = case c of
                            (Left (a,b)) -> (Left a, b)
                            (Right (a,b)) -> (Right a, b) in
    
    let res = bimap (addBid bid) (addBid bid) state in 
      trace "E.addBid"
      splitFstJoinSnd res
  getBids state=
    let res = bimap getBids getBids state in
      trace "E.getBids"
      E.join res
  tryGetAmountAndWinner state=
    let res = bimap tryGetAmountAndWinner tryGetAmountAndWinner state in
      trace "E.tryGetAmountAndWinner"
      E.join res
  hasEnded state=
    let res = bimap hasEnded hasEnded state in
      trace "E.hasEnded"
      E.join res
