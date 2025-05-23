{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module AuctionSite.Web.Types where
import           Data.Aeson
import           GHC.Generics
import           GHC.Conc         (TVar)
import           Prelude
import           Data.Time        (UTCTime)
import           Web.Spock        (SpockM, SpockAction)
import           Control.Applicative
import           Control.Monad    (mzero)
import           Data.Aeson.Types (Parser)
import qualified Data.Text       as T

import           AuctionSite.Domain
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
  reqId :: AuctionId,
  reqStartsAt :: UTCTime,
  reqTitle :: String,
  reqEndsAt :: UTCTime,
  reqCurrency :: M.Currency,
  -- TODO should be either of the options in the same format as other implementation:
  reqTyp :: AuctionType
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
      parseTyp :: Parser AuctionType
      parseTyp = (v .: "typ") <|> pure (TimedAscending DT.defaultOptions)
 parseJSON _ = mzero


newtype AppState = AppState { appAuctions :: TVar Repository }

type Api = SpockM () () AppState ()

type ApiAction a = SpockAction () () AppState a
