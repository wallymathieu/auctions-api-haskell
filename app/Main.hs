{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
  
import           Web.Scotty
import           Data.Monoid      ((<>))
import           Data.Text        (pack)
import           GHC.Generics
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import           Network.HTTP.Types.Status (status404)
import qualified Data.Aeson as A
import qualified Domain.Prelude as DP
import qualified Domain.Auctions as A
import qualified Domain.Commands as C

newtype BidReq = BidReq { 
  amount:: Integer 
} deriving (Generic, Show)

instance A.ToJSON BidReq
instance A.FromJSON BidReq

data AddAuctionReq = AddAuctionReq {
  id :: DP.AuctionId,
  startsAt :: String,
  title :: String,
  endsAt :: String,
  currency :: String,
  typ:: String
} deriving (Generic, Show)

instance A.ToJSON AddAuctionReq
instance A.FromJSON AddAuctionReq

main :: IO ()
main = do
  scotty 8080 $ do
    get "/auctions" $ do
      auctions <- liftIO readAuctions
      json auctions
    get "/auction/:id" $ do
      {-
      pid <- param "id"
      case pid of 
        Just tid  -> do
                       Just auction <- liftIO $ readAuction tid
                       json auction
        Nothing -> status status404
        -}
      status status404 
    post "/auction" $ do
      {-auctionReq <- jsonData
      res <- liftIO $ insertAuction auctionReq
      json res-}
      status status404 
    post "/auction/:id/bid" $ do
      {-pid <- param "id"
      addBidReq <- jsonData-}
      fail "!"
  where
    readAuctions :: IO [A.Auction]
    readAuctions = fail "!"
    readAuction :: Integer -> IO (Maybe A.Auction)
    readAuction id = fail "!"
    insertAuction :: AddAuctionReq -> IO (Either String C.CommandSuccess)
    insertAuction _ = fail "!"

