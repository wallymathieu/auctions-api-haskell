module Domain.State where
import Domain.Prelude
import Domain.Bid
import Data.Time

class State a where
  inc :: UTCTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
