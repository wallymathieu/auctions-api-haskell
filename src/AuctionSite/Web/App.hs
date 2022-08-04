{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.App where

import           Web.Spock

import           Data.Aeson       hiding (json)
import           GHC.Conc        (newTVar, readTVarIO, atomically)
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text       as T
import qualified Data.List       as L
import qualified Data.Map        as Map
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Control.Concurrent.STM (stateTVar)

import           AuctionSite.Domain
import qualified AuctionSite.Money           as M
import           AuctionSite.Web.Types

notFoundJson :: T.Text -> ApiAction a
notFoundJson msg = setStatus Http.status404 >> json (ApiError { message = msg })

readAuction :: Integer -> ApiAction (Maybe Auction)
readAuction aId = do
  L.find (\a-> auctionId a == aId) <$> readAuctions

readAuctions :: ApiAction [Auction]
readAuctions = do
  data' <- getState
  auctions' <- liftIO $ readTVarIO $ appAuctions data'
  return (map fst (Map.elems auctions'))

auctionNotFound :: T.Text
auctionNotFound = "Auction not found"

createBidAction :: AuctionId -> ApiAction a
createBidAction tid = do
  req <- jsonBody' :: ApiAction BidReq
  AppState { appAuctions = auctions' } <- getState
  res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState req) )
  case res of
    Left (UnknownAuction _)-> setStatus Http.status404 >> text auctionNotFound
    Left err-> setStatus Http.status400 >> json (show err)
    Right ok-> json ok
  where
  mutateState :: BidReq->UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState BidReq { amount=amount' } now current =
    let bid = Bid { bidder="TODO2", at=now, bidAmount=M.Amount M.VAC amount', forAuction=tid }
        command = PlaceBid now bid
    in handle command current


getAuctionAction :: AuctionId -> ApiAction a
getAuctionAction tid = do
  maybeAuction <- readAuction tid
  case maybeAuction of
    Nothing -> notFoundJson auctionNotFound
    Just auction -> json (toAuctionJson auction)

createAuctionAction :: ApiAction a
createAuctionAction = do
  auctionReq <- jsonBody' :: ApiAction AddAuctionReq
  AppState { appAuctions=auctions' } <- getState
  res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState auctionReq) )
  case res of
    Left err-> setStatus Http.status400 >> json (show err)
    Right ok-> json ok

  where
  mutateState :: AddAuctionReq->UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState AddAuctionReq { reqId=auctionId', reqStartsAt=startsAt', reqTitle=title', reqEndsAt=endsAt', reqCurrency=cur, reqTyp=typ'} now current =
    let auction = (Auction { auctionId = auctionId', startsAt = startsAt', title = title', expiry = endsAt',
                             seller = "TODO3", typ = typ', auctionCurrency = cur} )
        command = AddAuction now auction
    in handle command current

getAuctionsAction :: ApiAction a
getAuctionsAction = do
  auctions' <- readAuctions
  json (map toAuctionJson auctions')

toAuctionJson :: Auction -> Data.Aeson.Value
toAuctionJson Auction { auctionId = aId, startsAt = startsAt', title = title', expiry = expiry', auctionCurrency = currency' } =
  object [ "id" .= aId, "startsAt" .= startsAt', "title" .= title', "expiry" .= expiry', "currency" .= currency' ]

app :: Api
app = do
  get "auctions" getAuctionsAction
  get ("auction" <//> var) getAuctionAction
  post "auction" createAuctionAction
  post ("auction" <//> var <//> "bid") createBidAction

initAppState :: IO AppState
initAppState = atomically $ do
  auctions' <- newTVar Map.empty
  return (AppState {appAuctions=auctions'})
