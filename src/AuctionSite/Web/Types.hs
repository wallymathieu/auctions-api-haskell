{-# LANGUAGE DeriveGeneric     #-}
module AuctionSite.Web.Types where
import           Data.Aeson       hiding (json)
import           GHC.Generics
import           GHC.Conc         (TVar)
import           Prelude
import           Data.Time        (UTCTime)
import           Web.Spock        (SpockM, SpockAction)

import qualified AuctionSite.Domain.Prelude   as DP
import qualified AuctionSite.Domain.Auctions  as A
import qualified AuctionSite.Domain.Bids      as B
import qualified AuctionSite.Domain.SingleSealedBid as DS
import qualified AuctionSite.Domain.TimedAscending as DT
import qualified AuctionSite.Money as M

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
  currency :: M.Currency,
  -- TODO should be either of the options in the same format as other implementation:
  typ:: Maybe (Either DS.Options DT.Options)
} deriving (Generic, Show, Eq)

instance ToJSON AddAuctionReq
instance FromJSON AddAuctionReq

data AppAuctionState = AppAuctionState { auction:: A.Auction, auctionState:: A.AuctionState, auctionBids:: [ B.Bid ] }
newtype AppState = AppState { auctions :: TVar [AppAuctionState] }

type Api = SpockM () () AppState ()

type ApiAction a = SpockAction () () AppState a
