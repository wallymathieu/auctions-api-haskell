{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.App where

import           Web.Spock

import           Data.Aeson                hiding (json)
import           Data.Aeson.Types          (parseMaybe)
import           GHC.Conc                  (newTVar, readTVarIO, atomically)
import           Prelude
import           Control.Monad.IO.Class    (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text                 as T
import qualified Data.List                 as L
import qualified Data.Map                  as Map
import           Data.Time.Clock           (UTCTime, getCurrentTime)
import           Control.Concurrent.STM    (stateTVar)
import qualified Data.ByteString.Base64    as B64
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LB
import           AuctionSite.Domain
import qualified AuctionSite.Money         as M
import           AuctionSite.Web.Types

notFoundJson :: T.Text -> ApiAction a
notFoundJson msg = setStatus Http.status404 >> json (ApiError { message = msg })

withXAuth :: (UserId -> ApiAction a) -> ApiAction a
withXAuth onAuth= do
  auth <- rawHeader "x-jwt-payload"
  case (auth >>= readAndDecodeBase64) :: Maybe UserId of
    Just userId -> onAuth userId
    Nothing -> setStatus Http.status401 >> text "Unauthorized"
  where
    readAndDecodeBase64 :: ByteString -> Maybe UserId
    readAndDecodeBase64 v = decodeBase64 v >>=  decode >>= tryReadUserId
    tryReadUserId :: Value -> Maybe UserId
    tryReadUserId = parseMaybe $ withObject "User" $ \o -> o .: "sub"

    decodeBase64 :: ByteString -> Maybe LB.ByteString
    decodeBase64 v =  case B64.decode v of
                      Right b -> pure (LB.fromStrict b)
                      Left _ -> Nothing

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
  withXAuth (onAuth req)
  where
  onAuth :: BidReq -> UserId -> ApiAction a
  onAuth req userId = do
    AppState { appAuctions = auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState req userId) )
    case res of
      Left (UnknownAuction _)-> setStatus Http.status404 >> text auctionNotFound
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> json ok
  mutateState :: BidReq -> UserId -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState BidReq { amount=amount' } bidder' now current =
    let bid = Bid { bidder=bidder', at=now, bidAmount=M.Amount M.VAC amount', forAuction=tid }
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
  withXAuth (onAuth auctionReq)
  where
  onAuth :: AddAuctionReq -> UserId -> ApiAction a
  onAuth auctionReq userId = do
    AppState { appAuctions=auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState auctionReq userId) )
    case res of
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> json ok

  mutateState :: AddAuctionReq -> UserId -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState AddAuctionReq { reqId=auctionId', reqStartsAt=startsAt', reqTitle=title', reqEndsAt=endsAt', reqCurrency=cur, reqTyp=typ'} userId now current =
    let auction = (Auction { auctionId = auctionId', startsAt = startsAt', title = title', expiry = endsAt',
                             seller = userId, typ = typ', auctionCurrency = cur} )
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
