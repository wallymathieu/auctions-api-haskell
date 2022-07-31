{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.App where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           GHC.Generics
import           GHC.Conc        (TVar,newTVar,readTVar,readTVarIO,atomically)
import           GHC.Conc.Sync   (STM)
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text       as T
import qualified Data.List       as L
import           Data.Foldable (find)

import qualified AuctionSite.Domain.Prelude  as DP
import qualified AuctionSite.Domain.Auctions as A
import qualified AuctionSite.Domain.Bids     as B
import qualified AuctionSite.Domain.Commands as C
import           AuctionSite.Web.Types

notFound msg = do
  setStatus Http.status404
  json (ApiError { message = msg })

readAuction :: Integer -> ApiAction (Maybe A.Auction)
readAuction id = do
  L.find (\a-> A.auctionId a == id) <$> readAuctions

addBid :: (A.Auction, BidReq) -> ApiAction (Either String C.CommandSuccess)
addBid _ = error "not implemented"

readAuctions :: ApiAction [A.Auction]
readAuctions = do
  data' <- getState
  auctions' <- liftIO $ readTVarIO $ auctions data'
  return (map auction auctions')

insertAuction :: AddAuctionReq -> ApiAction (Either String C.CommandSuccess)
insertAuction auctionReq = do
  data' <- getState
  liftIO (repoInsertAuction auctionReq data')

repoInsertAuction ::  AddAuctionReq -> AppState -> IO (Either String C.CommandSuccess)
repoInsertAuction auctionReq state = error "not implemented"

createBidAction :: Integer -> ApiAction a
createBidAction tid = do
    addBidReq <- jsonBody' :: ApiAction BidReq
    maybeAuction <- readAuction tid
    case maybeAuction of
      Nothing -> notFound ""
      Just auction ->
        addBid (auction, addBidReq) >>= json

getAuctionAction :: Integer -> ApiAction a
getAuctionAction tid = do
    maybeAuction <- readAuction tid
    case maybeAuction of
      Nothing -> notFound ""
      Just auction -> json auction

createAuctionAction :: ApiAction a
createAuctionAction = do
    auctionReq <- jsonBody' :: ApiAction AddAuctionReq
    res <- insertAuction auctionReq
    json res

getAuctionsAction :: ApiAction a
getAuctionsAction = do
    auctions <- readAuctions
    json auctions

app :: Api
app = do
  get "auctions" getAuctionsAction
  get ("auction" <//> var) getAuctionAction
  post "auction" createAuctionAction
  post ("auction" <//> var <//> "bid") createBidAction
