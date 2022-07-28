{-# LANGUAGE DeriveGeneric     #-}
module ApiJson where
import           Data.Aeson       hiding (json)
import           GHC.Generics
import           Prelude
import           Data.Time        (UTCTime)

-- import qualified Money as M
import qualified Domain.Prelude   as DP
import qualified Domain.Auctions  as A
import qualified Domain.SingleSealedBid as DS
import qualified Domain.TimedAscending as DT
-- import qualified Domain.Bids as B

newtype ApiError = ApiError { 
  message:: String
} deriving (Generic, Show, Eq)

instance ToJSON ApiError
instance FromJSON ApiError

newtype BidReq = BidReq {
  amount:: Integer
} deriving (Generic, Show, Eq)

instance ToJSON BidReq
instance FromJSON BidReq

data AddAuctionReq = AddAuctionReq {
  id :: DP.AuctionId,
  startsAt :: UTCTime,
  title :: String,
  endsAt :: UTCTime,
  currency :: String,
  -- TODO should be either of the options in the same format as other implementation:
  typ:: Maybe (Either DS.Options DT.Options) 
} deriving (Generic, Show, Eq)

instance ToJSON AddAuctionReq
instance FromJSON AddAuctionReq

