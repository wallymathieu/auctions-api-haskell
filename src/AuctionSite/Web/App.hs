{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.App where

import           Web.Scotty
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Data.Aeson
import           Data.Aeson.Types          (parseMaybe)
import           GHC.Conc                  (newTVar, readTVarIO, atomically)
import           Prelude
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Map                  as Map
import           Data.Time.Clock           (UTCTime)
import           Control.Concurrent.STM    (stateTVar)
import qualified Data.ByteString.Base64    as B64
import qualified Data.Aeson.Types          as ATyp
import           AuctionSite.Domain
import qualified AuctionSite.Money         as M
import           AuctionSite.Web.Types

-- Convert status code to Scotty
notFoundJson :: TL.Text -> ActionM ()
notFoundJson msg = status Http.status404 >> json (ApiError { message = TL.toStrict msg })

-- Helper to handle JSON errors
raiseError :: Int -> String -> ActionM ()
raiseError code msg = status (Http.mkStatus code "") >> json msg

{- | Invoke onAuth if there is a base64 encoded x-jwt-payload header -}
withXAuth :: (User -> ActionM ()) -> ActionM ()
withXAuth onAuth = do
  auth <- header "x-jwt-payload"
  case auth >>= convertToUser of
    Just user' -> onAuth user'
    Nothing -> status Http.status401 >> text "Unauthorized"
  where
    convertToUser :: TL.Text -> Maybe User
    convertToUser txt = 
      let bytes = LB.toStrict (TL.encodeUtf8 txt)
      in case B64.decode bytes of
          Right decodedBytes -> decode (LB.fromStrict decodedBytes) >>= tryReadUserId
          Left _ -> Nothing
    
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

readAuction :: AppState -> Integer -> ActionM (Maybe (Auction, AuctionState))
readAuction appState aId = do
  auctions' <- liftIO $ readTVarIO $ appAuctions appState
  return (Map.lookup aId auctions')

readAuctions :: AppState -> ActionM [Auction]
readAuctions appState = do
  auctions' <- liftIO $ readTVarIO $ appAuctions appState
  return (map fst (Map.elems auctions'))

auctionNotFound :: TL.Text
auctionNotFound = "Auction not found"

createBidAction :: AppState -> IO UTCTime -> AuctionId -> ActionM ()
createBidAction appState getCurrentTime tid = do
  req <- jsonData :: ActionM BidReq
  withXAuth $ \userId' -> do
    res <- liftIO (getCurrentTime >>= (atomically . stateTVar (appAuctions appState) . mutateState req userId'))
    case res of
      Left (UnknownAuction _) -> status Http.status404 >> text auctionNotFound
      Left err -> status Http.status400 >> json (show err)
      Right ok -> json ok
  where
  mutateState :: BidReq -> User -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState BidReq { amount=amount' } bidder' now current =
    let bid = Bid { bidder=bidder', at=now, bidAmount=M.Amount M.VAC amount', forAuction=tid }
        command = PlaceBid now bid
    in handle command current

getAuctionAction :: AppState -> AuctionId -> ActionM ()
getAuctionAction appState tid = do
  maybeAuction <- readAuction appState tid
  case maybeAuction of
    Nothing -> notFoundJson auctionNotFound
    Just (auction, auctionState) ->
      let maybeAmountAndWinner = tryGetAmountAndWinner auctionState
          amount' = fst <$> maybeAmountAndWinner
          winner = snd <$> maybeAmountAndWinner
      in json (object (["bids" .= map toAuctionBidJson (getBids auctionState), "winner" .= winner, "winnerPrice" .= amount' ] ++ toAuctionListItemKV auction))

createAuctionAction :: AppState -> IO UTCTime -> ActionM ()
createAuctionAction appState getCurrentTime = do
  auctionReq <- jsonData :: ActionM AddAuctionReq
  withXAuth $ \userId' -> do
    res <- liftIO (getCurrentTime >>= (atomically . stateTVar (appAuctions appState) . mutateState auctionReq userId'))
    case res of
      Left err -> status Http.status400 >> json (show err)
      Right ok -> json ok
  where
  mutateState :: AddAuctionReq -> User -> UTCTime -> Repository -> (Either Errors CommandSuccess, Repository)
  mutateState AddAuctionReq { reqId=auctionId', reqStartsAt=startsAt', reqTitle=title', reqEndsAt=endsAt', reqCurrency=cur, reqTyp=typ'} userId' now current =
    let auction = (Auction { auctionId = auctionId', startsAt = startsAt', title = title', expiry = endsAt',
                             seller = userId', typ = typ', auctionCurrency = cur} )
        command = AddAuction now auction
    in handle command current

getAuctionsAction :: AppState -> ActionM ()
getAuctionsAction appState = do
  auctions' <- readAuctions appState
  json (map (object . toAuctionListItemKV) auctions')

toAuctionListItemKV :: KeyValue e a => Auction -> [a]
toAuctionListItemKV Auction { auctionId = aId, startsAt = startsAt', title = title', expiry = expiry', auctionCurrency = currency' } =
  [ "id" .= aId, "startsAt" .= startsAt', "title" .= title', "expiry" .= expiry', "currency" .= currency' ]

toAuctionBidJson :: Bid -> Value 
toAuctionBidJson Bid { bidAmount=amount', bidder=bidder' } =
  object [ "amount" .= amount', "bidder" .= bidder' ]

-- Configure Scotty application
app :: AppState -> IO UTCTime -> ScottyM ()
app appState getCurrentTime = do
  -- Add middleware for logging
  middleware logStdoutDev
  
  -- Define routes
  get "/auctions" $ getAuctionsAction appState
  get "/auction/:id" $ do
    pAuctionId <- pathParam "id"
    getAuctionAction appState pAuctionId
  post "/auction" $ createAuctionAction appState getCurrentTime
  post "/auction/:id/bid" $ do
    pAuctionId <- pathParam "id"
    createBidAction appState getCurrentTime pAuctionId

initAppState :: IO AppState
initAppState = atomically $ do
  auctions' <- newTVar Map.empty
  return (AppState {appAuctions=auctions'})