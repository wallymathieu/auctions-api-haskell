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

import qualified AuctionSite.Domain.Prelude  as DP -- TODO: make it more like a domain prelude
import qualified AuctionSite.Domain          as D
import qualified AuctionSite.Domain.Auctions as A
import qualified AuctionSite.Domain.Bids     as B
import qualified AuctionSite.Domain.Commands as C
import qualified AuctionSite.Money           as M
import           AuctionSite.Web.Types       as Types

notFoundJson :: T.Text -> ApiAction a
notFoundJson msg = setStatus Http.status404 >> json (ApiError { message = msg })

readAuction :: Integer -> ApiAction (Maybe A.Auction)
readAuction aId = do
  L.find (\a-> A.auctionId a == aId) <$> readAuctions

readAuctions :: ApiAction [A.Auction]
readAuctions = do
  data' <- getState
  auctions' <- liftIO $ readTVarIO $ auctions data'
  return (map fst (Map.elems auctions'))

auctionNotFound :: T.Text
auctionNotFound = "Auction not found"

createBidAction :: Integer -> ApiAction a
createBidAction tid = do
  req <- jsonBody' :: ApiAction BidReq
  AppState { auctions = auctions' } <- getState
  res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState req) )
  case res of
    Left (DP.UnknownAuction _)-> setStatus Http.status404 >> text auctionNotFound
    Left err-> setStatus Http.status400 >> json (show err)
    Right ok-> json ok
  where
  auctionId = tid::DP.AuctionId
  mutateState :: BidReq->UTCTime -> D.Repository -> (Either DP.Errors C.CommandSuccess, D.Repository)
  mutateState BidReq { amount=amount' } now current =
    let bid = B.Bid { B.bidder="TODO2", B.at=now, B.bidAmount=M.Amount M.VAC amount', B.forAuction=auctionId }
        command = C.PlaceBid now bid
    in case D.handle command current of
       Left err->(Left err,current)
       Right (next, result) -> (Right result,next)


getAuctionAction :: Integer -> ApiAction a
getAuctionAction tid = do
  maybeAuction <- readAuction tid
  case maybeAuction of
    Nothing -> notFoundJson auctionNotFound
    Just auction -> json (toAuctionJson auction)

createAuctionAction :: ApiAction a
createAuctionAction = do
  auctionReq <- jsonBody' :: ApiAction AddAuctionReq
  AppState { auctions=auctions' } <- getState
  res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState auctionReq) )
  case res of
    Left err-> setStatus Http.status400 >> json (show err)
    Right ok-> json ok

  where
  mutateState :: AddAuctionReq->UTCTime -> D.Repository -> (Either DP.Errors C.CommandSuccess, D.Repository)
  mutateState AddAuctionReq { Types.id=auctionId, startsAt=startsAt, title=title, endsAt=endsAt, currency=cur, typ=typ} now current =
    let auction = (A.Auction {A.auctionId = auctionId, A.startsAt = startsAt, A.title = title, A.expiry = endsAt,
                              A.seller = "TODO3", A.typ = typ, A.auctionCurrency = cur} )
        command = D.AddAuction now auction
    in case D.handle command current of
       Left err->(Left err, current)
       Right (next, result) -> (Right result, next)

getAuctionsAction :: ApiAction a
getAuctionsAction = do
  auctions' <- readAuctions
  json (map toAuctionJson auctions')

toAuctionJson :: A.Auction -> Data.Aeson.Value
toAuctionJson A.Auction { A.auctionId = aId, A.startsAt = startsAt, A.title = title, A.expiry = expiry, A.auctionCurrency = currency } =
  object [ "id" .= aId, "startsAt" .= startsAt, "title" .= title, "expiry" .= expiry, "currency" .= currency ]

app :: Api
app = do
  get "auctions" getAuctionsAction
  get ("auction" <//> var) getAuctionAction
  post "auction" createAuctionAction
  post ("auction" <//> var <//> "bid") createBidAction

initAppState :: IO AppState
initAppState = atomically $ do
  auctions' <- newTVar Map.empty
  return (AppState {auctions=auctions'})
