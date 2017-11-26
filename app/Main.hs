{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
  
import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (Text, pack)
import           GHC.Generics
import qualified Prelude as P

import Lib
import qualified Money as M
import qualified Domain.Prelude as DP
import qualified Domain.Auction as A
import qualified Domain.Bid as B
import qualified Domain.Commands as C
import qualified Domain.SingleSealedBid as DS
import qualified Domain.TimedAscending as DT

newtype BidReq = BidReq { 
  amount:: P.Integer 
} deriving (Generic, P.Show)

instance ToJSON BidReq
instance FromJSON BidReq

data AddAuctionReq = AddAuctionReq {
  id :: DP.AuctionId,
  startsAt :: P.String,
  title :: P.String,
  endsAt :: P.String,
  currency :: P.String,
  typ:: P.String
} deriving (Generic, P.Show)

instance ToJSON AddAuctionReq
instance FromJSON AddAuctionReq

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: P.IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

app :: Api
app = do
  get "auction-req" P.$ do
    json P.$ AddAuctionReq { id = 1, startsAt = "2017-1-1", title= "title", endsAt="2018-1-1", currency="SEK", typ="" }
  post "auction" P.$ do
    auction1 <- jsonBody' :: ApiAction AddAuctionReq
    text P.$ "Parsed: " <> pack (P.show auction1)
