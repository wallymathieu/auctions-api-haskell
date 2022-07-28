{-# LANGUAGE DeriveGeneric     #-}
module ApiJson where
import           Data.Aeson       hiding (json)
import           GHC.Generics
import           Prelude

-- import qualified Money as M
import qualified Domain.Prelude as DP
import qualified Domain.Auctions as A
-- import qualified Domain.Bids as B

data ApiError = ApiError { 
  message:: String
} deriving (Generic, Show)

instance ToJSON ApiError
instance FromJSON ApiError

data BidReq = BidReq { 
  amount:: Integer 
} deriving (Generic, Show)

instance ToJSON BidReq
instance FromJSON BidReq

data AddAuctionReq = AddAuctionReq {
  id :: DP.AuctionId,
  startsAt :: String,
  title :: String,
  endsAt :: String,
  currency :: String,
  typ:: String
} deriving (Generic, Show)

instance ToJSON AddAuctionReq
instance FromJSON AddAuctionReq

