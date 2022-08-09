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
import qualified Data.Map                  as Map
import           Data.Time.Clock           (UTCTime)
import           Control.Concurrent.STM    (stateTVar)
import qualified Data.ByteString.Base64    as B64
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Aeson.Types          as ATyp
import           AuctionSite.Domain
import qualified AuctionSite.Money         as M
import           AuctionSite.Web.Types

notFoundJson :: T.Text -> ApiAction a
notFoundJson msg = setStatus Http.status404 >> json (ApiError { message = msg })

{- | Invoke onAuth if there is a base64 encoded x-jwt-payload header -}
withXAuth :: (User -> ApiAction a) -> ApiAction a
withXAuth onAuth= do
  auth <- rawHeader "x-jwt-payload"
  case (auth >>= readAndDecodeBase64) :: Maybe User of
    Just user' -> onAuth user'
    Nothing -> setStatus Http.status401 >> text "Unauthorized"
  where
    readAndDecodeBase64 :: ByteString -> Maybe User
    readAndDecodeBase64 v = decodeBase64 v >>=  decode >>= tryReadUserId
    tryReadUserId :: Value -> Maybe User
    tryReadUserId = parseMaybe $ withObject "User" $ \o -> do
      sub' <- o .: "sub"
      name' <- o .:? "name"
      uTyp' <- o .: "u_typ"
      create sub' name' uTyp'
    create :: UserId -> Maybe T.Text -> T.Text -> ATyp.Parser User
    create sub (Just name) "0" = pure $ BuyerOrSeller sub name
    create sub _           "1" = pure $ Support sub
    create _   _           _   = ATyp.prependFailure "parsing User failed, " (fail "could not interpret values")
    decodeBase64 :: ByteString -> Maybe LB.ByteString
    decodeBase64 v =  case B64.decode v of
                      Right b -> pure (LB.fromStrict b)
                      Left _ -> Nothing

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

createBidAction :: IO UTCTime -> AuctionId -> ApiAction a
createBidAction getCurrentTime tid = do
  req <- jsonBody' :: ApiAction BidReq
  withXAuth $ \userId' -> do
    AppState { appAuctions = auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState req userId') )
    case res of
      Left (UnknownAuction _)-> setStatus Http.status404 >> text auctionNotFound
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> json ok
  where
  mutateState :: BidReq -> User -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState BidReq { amount=amount' } bidder' now current =
    let bid = Bid { bidder=bidder', at=now, bidAmount=M.Amount M.VAC amount', forAuction=tid }
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

createAuctionAction :: IO UTCTime -> ApiAction a
createAuctionAction getCurrentTime = do
  auctionReq <- jsonBody' :: ApiAction AddAuctionReq
  withXAuth $ \userId' -> do
    AppState { appAuctions=auctions' } <- getState
    res <- liftIO ( getCurrentTime >>= (atomically . stateTVar auctions' . mutateState auctionReq userId') )
    case res of
      Left err-> setStatus Http.status400 >> json (show err)
      Right ok-> json ok
  where
  mutateState :: AddAuctionReq -> User -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState AddAuctionReq { reqId=auctionId', reqStartsAt=startsAt', reqTitle=title', reqEndsAt=endsAt', reqCurrency=cur, reqTyp=typ'} userId' now current =
    let auction = (Auction { auctionId = auctionId', startsAt = startsAt', title = title', expiry = endsAt',
                             seller = userId', typ = typ', auctionCurrency = cur} )
        command = AddAuction now auction
    in handle command current

getAuctionsAction :: ApiAction a
getAuctionsAction = do
  auctions' <- readAuctions
  json (map (object . toAuctionListItemKV) auctions')

toAuctionListItemKV :: KeyValue a => Auction -> [a]
toAuctionListItemKV Auction { auctionId = aId, startsAt = startsAt', title = title', expiry = expiry', auctionCurrency = currency' } =
  [ "id" .= aId, "startsAt" .= startsAt', "title" .= title', "expiry" .= expiry', "currency" .= currency' ]

toAuctionBidJson :: Bid -> Value 
toAuctionBidJson Bid { bidAmount=amount', bidder=bidder' } =
  object [ "amount" .= amount', "bidder" .= bidder' ]
app :: IO UTCTime -> Api
app getCurrentTime = do
  get "auctions" getAuctionsAction
  get ("auction" <//> var) getAuctionAction
  post "auction" (createAuctionAction getCurrentTime)
  post ("auction" <//> var <//> "bid") (createBidAction getCurrentTime)

initAppState :: IO AppState
initAppState = atomically $ do
  auctions' <- newTVar Map.empty
  return (AppState {appAuctions=auctions'})
