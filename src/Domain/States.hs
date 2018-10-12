module Domain.States (module Domain.States) where
import Domain.Prelude
import Domain.Bids
import Data.Time
import Data.Bifunctor
import Money

class State a where
  inc :: UTCTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
  getBids :: a -> [Bid]
  tryGetAmountAndWinner:: a -> Maybe (Amount , UserId)
  hasEnded:: a -> Bool

-- composing two State types as a State by using Either
instance (State a , State b) => (State (Either a b)) where
  inc now = bimap (inc now) (inc now) 

  addBid bid state =
    {- 
    split a tuple of (Either (a,b) (c,b)) where the second element is of the same type into
    ((Either a c),b)
    -}
    let splitFstJoinSnd c = case c of
                            (Left (a,b)) -> (Left a, b)
                            (Right (a,b)) -> (Right a, b) in
    
    let res = bimap (addBid bid) (addBid bid) state in 
    splitFstJoinSnd res
  getBids state=
    let res = bimap getBids getBids state in
      either id id res
  tryGetAmountAndWinner state=
    let res = bimap tryGetAmountAndWinner tryGetAmountAndWinner state in
      either id id res
  hasEnded state=
    let res = bimap hasEnded hasEnded state in
      either id id res
