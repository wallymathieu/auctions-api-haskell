module Domain.States where
import Domain.Prelude
import Domain.Bids
import Data.Time
import Money

class State a where
  inc :: UTCTime -> a -> a
  addBid :: Bid -> a -> (a, Either Errors ())
  getBids :: a -> [Bid]
  tryGetAmountAndWinner:: a -> Maybe (Amount , UserId)
  hasEnded:: a -> Bool
  