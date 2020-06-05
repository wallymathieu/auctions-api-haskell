{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
  
import           Web.Scotty
import           Data.Aeson
import           Data.Monoid      ((<>))
import           Data.Text        (pack)
import           GHC.Generics
import           Prelude
--import qualified Prelude as P

-- import qualified Money as M
import qualified Domain.Prelude as DP
-- import qualified Domain.Auctions as A
-- import qualified Domain.Bids as B
-- import qualified Domain.Commands as C
-- import qualified Domain.SingleSealedBid as DS
-- import qualified Domain.TimedAscending as DT

newtype BidReq = BidReq { 
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

main :: IO ()
main = do
  scotty 8080 $ do
    {-get "auction-req" $ do
      json $ AddAuctionReq { id = 1, startsAt = "2017-1-1", title= "title", endsAt="2018-1-1", currency="SEK", typ="" }
      -}
    {-
    post "auction" $ do
      auction1 <- jsonBody' :: ApiAction AddAuctionReq
      text $ "Parsed: " <> pack (show auction1)
      -}
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
