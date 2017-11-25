module Domain.State where
import Domain.Prelude
import Domain.Bid

class State a where
  inc :: DateTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
