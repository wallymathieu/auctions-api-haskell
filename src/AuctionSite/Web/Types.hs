{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module AuctionSite.Web.Types where
import           Data.Aeson       hiding (json)
import           GHC.Generics
import           GHC.Conc         (TVar)
import           Prelude
import           Data.Time        (UTCTime)
import           Web.Spock        (SpockM, SpockAction)
import           Control.Applicative
import           Control.Monad    (mzero)
import           Data.Aeson.Types (Parser)
import qualified Data.Text       as T

import qualified AuctionSite.Domain           as D
import qualified AuctionSite.Domain.Prelude   as DP
import qualified AuctionSite.Domain.Auctions  as A
import qualified AuctionSite.Domain.TimedAscending as DT
import qualified AuctionSite.Money as M

newtype ApiError = ApiError {
  message:: T.Text
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
  typ:: A.AuctionType
} deriving (Generic, Show, Eq)


instance FromJSON AddAuctionReq where
 parseJSON (Object v) =
      AddAuctionReq <$> v .: "id"
                    <*> v .: "startsAt"
                    <*> v .: "title"
                    <*> v .: "endsAt"
                    <*> parseCurrency
                    <*> parseTyp
    where
      parseCurrency = (v .: "currency") <|> pure M.VAC
      parseTyp :: Parser A.AuctionType
      parseTyp = parseCurrency >>= (\c -> (v .: "typ") <|> pure (A.TimedAscending $ DT.defaultOptions c))
 parseJSON _ = mzero


newtype AppState = AppState { auctions :: TVar D.Repository }

type Api = SpockM () () AppState ()

type ApiAction a = SpockAction () () AppState a
