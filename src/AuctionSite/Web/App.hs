{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.App where

import           Web.Spock

import           Data.Aeson
import           GHC.Conc                  (newTVar, readTVarIO, atomically)
import           Prelude
import           Control.Monad.IO.Class    (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text                 as T
import qualified Data.Map                  as Map
import           Data.Time.Clock           (UTCTime)
import           Control.Concurrent.STM    (stateTVar)

import           AuctionSite.Domain
import           AuctionSite.Web.Types
import           AuctionSite.Web.Jwt       as Jwt

notFoundJson :: T.Text -> ApiAction a
notFoundJson msg = setStatus Http.status404 >> json (ApiError { message = msg })

{- | Invoke onAuth if there is a base64 encoded x-jwt-payload header -}
withXAuth :: (User -> ApiAction a) -> ApiAction a
withXAuth onAuth= do
  auth <- rawHeader "x-jwt-payload"
  case (auth >>= Jwt.decodeJwtUser) :: Maybe JwtUser of
    Just user' -> onAuth $ unWrapJwtUser user'
    Nothing -> setStatus Http.status401 >> text "Unauthorized"

readAuction :: Integer -> ApiAction (Maybe (Auction, AuctionState))
readAuction aId = do
  data' <- getState
  auctions' <- liftIO $ readTVarIO $ appAuctions data'
  return (Map.lookup aId auctions')

readAuctions :: ApiAction [Auction]
readAuctions = do
  data' <- getState
  auctions' <- liftIO $ readTVarIO $ appAuctions data'
  return (map fst (Map.elems auctions'))

auctionNotFound :: T.Text
auctionNotFound = "Auction not found"

createBidOnAction :: (Event-> IO ()) -> IO UTCTime -> AuctionId -> ApiAction a
createBidOnAction onEvent getCurrentTime tid = do
  req <- jsonBody' :: ApiAction BidReq
  withXAuth $ \userId' -> do
    AppState { appAuctions = auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState req userId') )
    case res of
      Left (UnknownAuction _)-> setStatus Http.status404 >> text auctionNotFound
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> do
        liftIO $ onEvent ok
        json ok
  where
  mutateState :: BidReq -> User -> UTCTime -> Repository -> (Either Errors Event, Repository)
  mutateState BidReq { amount=amount' } bidder' now current =
    let bid = Bid { bidder=bidder', at=now, bidAmount= amount', forAuction=tid }
        command = PlaceBid now bid
    in handle command current


getAuctionAction :: AuctionId -> ApiAction a
getAuctionAction tid = do
  maybeAuction <- readAuction tid
  case maybeAuction of
    Nothing -> notFoundJson auctionNotFound
    Just (auction,auctionState) ->
      let maybeAmountAndWinner = tryGetAmountAndWinner auctionState
          amount' = fst <$> maybeAmountAndWinner
          winner = snd <$> maybeAmountAndWinner
      in json (object (["bids" .= map toAuctionBidJson (getBids auctionState), "winner" .= winner, "winnerPrice" .= amount' ] ++ toAuctionListItemKV auction) )

createAuctionAction ::  (Event-> IO ()) -> IO UTCTime -> ApiAction a
createAuctionAction onEvent getCurrentTime = do
  auctionReq <- jsonBody' :: ApiAction AddAuctionReq
  withXAuth $ \userId' -> do
    AppState { appAuctions=auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState auctionReq userId') )
    case res of
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> do
        liftIO $ onEvent ok
        json ok
  where
  mutateState :: AddAuctionReq -> User -> UTCTime -> Repository -> (Either Errors Event, Repository)
  mutateState AddAuctionReq { reqId=auctionId', reqStartsAt=startsAt', reqTitle=title', reqEndsAt=endsAt', reqCurrency=cur, reqTyp=typ'} userId' now current =
    let auction = (Auction { auctionId = auctionId', startsAt = startsAt', title = title', expiry = endsAt',
                             seller = userId', typ = typ', auctionCurrency = cur} )
        command = AddAuction now auction
    in handle command current

getAuctionsAction :: ApiAction a
getAuctionsAction = do
  auctions' <- readAuctions
  json (map (object . toAuctionListItemKV) auctions')

toAuctionListItemKV :: KeyValue e a => Auction -> [a]
toAuctionListItemKV Auction { auctionId = aId, startsAt = startsAt', title = title', expiry = expiry', auctionCurrency = currency' } =
  [ "id" .= aId, "startsAt" .= startsAt', "title" .= title', "expiry" .= expiry', "currency" .= currency' ]

toAuctionBidJson :: Bid -> Value
toAuctionBidJson Bid { bidAmount=amount', bidder=bidder' } =
  object [ "amount" .= amount', "bidder" .= bidder' ]

app :: (Event-> IO ()) -> IO UTCTime -> Api
app onEvent getCurrentTime = do
  get "auctions" getAuctionsAction
  get ("auctions" <//> var) getAuctionAction
  post "auctions" (createAuctionAction onEvent getCurrentTime)
  post ("auctions" <//> var <//> "bids") (createBidOnAction onEvent getCurrentTime)

initAppState :: Map.Map AuctionId (Auction, AuctionState) -> IO AppState
initAppState initialAuctions = atomically $ do
  auctions' <- newTVar initialAuctions
  return (AppState {appAuctions=auctions'})
