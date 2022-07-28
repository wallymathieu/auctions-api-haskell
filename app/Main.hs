{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (pack)
import           GHC.Generics
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text as T

import qualified Domain.Prelude as DP
import qualified Domain.Auctions as A
import qualified Domain.Commands as C
import ApiJson

notFound msg = do
  setStatus Http.status404
  json (ApiError { message = msg })


type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase ()
  runSpock 8080 (spock spockCfg app)

readAuction :: Integer -> IO (Maybe A.Auction)
readAuction id = fail "!"
addBid :: (A.Auction, BidReq) -> IO (Either String C.CommandSuccess)
addBid _ = fail "!"
readAuctions :: IO [A.Auction]
readAuctions = fail "!"
insertAuction :: AddAuctionReq -> IO (Either String C.CommandSuccess)
insertAuction _ = fail "!"

textToInteger :: T.Text -> Integer
textToInteger t = read $ T.unpack t

createBidAction :: T.Text -> ApiAction a
createBidAction tid = do
    addBidReq <- jsonBody' :: ApiAction BidReq
    maybeAuction <- liftIO $ readAuction (textToInteger tid)
    case maybeAuction of
      Nothing -> notFound ""
      Just auction ->
        liftIO (addBid (auction, addBidReq)) >>= json
getAuctionAction :: T.Text -> ApiAction a
getAuctionAction tid = do
    maybeAuction <- liftIO $ readAuction (textToInteger tid)
    case maybeAuction of
      Nothing -> notFound ""
      Just auction -> json auction
createAuctionAction :: ApiAction a
createAuctionAction = do
    auctionReq <- jsonBody' :: ApiAction AddAuctionReq
    res <- liftIO $ insertAuction auctionReq
    json res
getAuctionsAction :: ApiAction a
getAuctionsAction = do
    auctions <- liftIO readAuctions
    json auctions

app :: Api
app = do
  get "auctions" getAuctionsAction
  get ("auction" <//> var) getAuctionAction
  post "auction" createAuctionAction
  post ("auction" <//> var <//> "bid") createBidAction
