-- src/AuctionSite/Web/API.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- Add this pragma
{-# LANGUAGE InstanceSigs #-}
module AuctionSite.Web.API where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson
import Data.Proxy
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map
import GHC.Generics
import Network.Wai
import Servant
import qualified Data.Text.Encoding as TE

import AuctionSite.Domain hiding (Repository)
import AuctionSite.Money
import AuctionSite.Web.Types (BidReq(..), AddAuctionReq(..), Repository)

-- API specification
type AuctionAPI =
  "auctions" :> Get '[JSON] [AuctionListItem]
  :<|> "auction" :> Capture "id" AuctionId :> Get '[JSON] AuctionDetails
  :<|> "auction" :> Header "x-jwt-payload" User :> ReqBody '[JSON] AddAuctionReq :> Post '[JSON] CommandSuccess
  :<|> "auction" :> Capture "id" AuctionId :> "bid" :> Header "x-jwt-payload" User :> ReqBody '[JSON] BidReq :> Post '[JSON] CommandSuccess

-- Data types for responses
data AuctionListItem = AuctionListItem
  { aliId :: AuctionId
  , aliTitle :: String
  , aliStartsAt :: UTCTime
  , aliExpiry :: UTCTime
  , aliCurrency :: Currency
  } deriving (Generic, Show)

instance ToJSON AuctionListItem where
  toJSON AuctionListItem{aliId, aliTitle, aliStartsAt, aliExpiry, aliCurrency} = object
    [ "id" .= aliId
    , "title" .= aliTitle
    , "startsAt" .= aliStartsAt
    , "expiry" .= aliExpiry
    , "currency" .= aliCurrency
    ]

data AuctionDetails = AuctionDetails
  { adId :: AuctionId
  , adTitle :: String
  , adStartsAt :: UTCTime
  , adExpiry :: UTCTime
  , adCurrency :: Currency
  , adBids :: [AuctionBid]
  , adWinner :: Maybe UserId
  , adWinnerPrice :: Maybe Amount
  } deriving (Generic, Show)

data AuctionBid = AuctionBid
  { abAmount :: Amount
  , abBidder :: User
  } deriving (Generic, Show)

instance ToJSON AuctionBid where
  toJSON AuctionBid{abAmount, abBidder} = object
    [ "amount" .= abAmount
    , "bidder" .= abBidder
    ]

instance ToJSON AuctionDetails where
  toJSON AuctionDetails{adId, adTitle, adStartsAt, adExpiry, adCurrency, adBids, adWinner, adWinnerPrice} = object
    [ "id" .= adId
    , "title" .= adTitle
    , "startsAt" .= adStartsAt
    , "expiry" .= adExpiry
    , "currency" .= adCurrency
    , "bids" .= adBids
    , "winner" .= adWinner
    , "winnerPrice" .= adWinnerPrice
    ]

-- App environment and monad type
data AppEnv = AppEnv
  { appAuctions :: TVar Repository
  , getCurrentTime :: IO UTCTime
  } -- No deriving here to make it accessible from other modules

type AppM = ReaderT AppEnv Handler

-- Servant API
auctionAPI :: Proxy AuctionAPI
auctionAPI = Proxy

-- Handlers
getAuctionsHandler :: AppM [AuctionListItem]
getAuctionsHandler = do
  env <- ask
  let appAuctions' = appAuctions env
  auctions' <- liftIO $ readTVarIO appAuctions'
  return $ map auctionToListItem (auctions auctions')
  where
    auctionToListItem :: Auction -> AuctionListItem
    auctionToListItem Auction{auctionId, title, startsAt, expiry, auctionCurrency} = AuctionListItem
      { aliId = auctionId
      , aliTitle = title
      , aliStartsAt = startsAt
      , aliExpiry = expiry
      , aliCurrency = auctionCurrency
      }

getAuctionHandler :: AuctionId -> AppM AuctionDetails
getAuctionHandler auctionId' = do
  env <- ask
  let appAuctions' = appAuctions env
  auctions' <- liftIO $ readTVarIO appAuctions'
  case Map.lookup auctionId' auctions' of
    Nothing -> throwError err404 { errBody = "Auction not found" }
    Just (auction, state) -> do
      let bids' = getBids state
          maybeAmountAndWinner = tryGetAmountAndWinner state
          winnerAmount = fst <$> maybeAmountAndWinner
          winnerId = snd <$> maybeAmountAndWinner
      return AuctionDetails
        { adId = auctionId auction
        , adTitle = title auction
        , adStartsAt = startsAt auction
        , adExpiry = expiry auction
        , adCurrency = auctionCurrency auction
        , adBids = map bidToAuctionBid bids'
        , adWinner = winnerId
        , adWinnerPrice = winnerAmount
        }
  where
    bidToAuctionBid :: Bid -> AuctionBid
    bidToAuctionBid Bid{bidAmount, bidder} = AuctionBid
      { abAmount = bidAmount
      , abBidder = bidder
      }

createAuctionHandler :: Maybe User -> AddAuctionReq -> AppM CommandSuccess
createAuctionHandler maybeUser req = do
  env <- ask
  let appAuctions' = appAuctions env
      getCurrentTime' = getCurrentTime env
  case maybeUser of
    Nothing -> throwError err401 { errBody = "Unauthorized" }
    Just userId' -> do
      now <- liftIO getCurrentTime'
      let auction = Auction
            { auctionId = reqId req
            , startsAt = reqStartsAt req
            , title = reqTitle req
            , expiry = reqEndsAt req
            , seller = userId'
            , typ = reqTyp req
            , auctionCurrency = reqCurrency req
            }
          command = AddAuction now auction
      result <- liftIO $ atomically $ stateTVar appAuctions' (handle command)
      case result of
        Left err -> throwError err400 { errBody = LB.fromStrict $ TE.encodeUtf8 $ T.pack $ show err }
        Right success -> return success

createBidHandler :: AuctionId -> Maybe User -> BidReq -> AppM CommandSuccess
createBidHandler auctionId' maybeUser req = do
  env <- ask
  let appAuctions' = appAuctions env
      getCurrentTime' = getCurrentTime env
  case maybeUser of
    Nothing -> throwError err401 { errBody = "Unauthorized" }
    Just userId' -> do
      now <- liftIO getCurrentTime'
      let bid = Bid
            { forAuction = auctionId'
            , bidder = userId'
            , at = now
            , bidAmount = Amount VAC (amount req)
            }
          command = PlaceBid now bid
      result <- liftIO $ atomically $ stateTVar appAuctions' (handle command)
      case result of
        Left (UnknownAuction _) -> throwError err404 { errBody = LB.fromStrict $ TE.encodeUtf8 "Auction not found" }
        Left err -> throwError err400 { errBody = LB.fromStrict $ TE.encodeUtf8 $ T.pack $ show err }
        Right success -> return success

-- Convert AppM to Handler
nt :: AppEnv -> AppM a -> Handler a
nt env = flip runReaderT env

-- Server
server :: AppEnv -> Server AuctionAPI
server env =
  hoistServer auctionAPI (nt env) $
    getAuctionsHandler
    :<|> getAuctionHandler
    :<|> createAuctionHandler
    :<|> createBidHandler

-- Initialize the app state
initAppState :: IO UTCTime -> IO AppEnv
initAppState getTime = do
  auctions' <- newTVarIO Map.empty
  return AppEnv
    { appAuctions = auctions'
    , getCurrentTime = getTime
    }

-- Main app
app :: IO UTCTime -> IO Application
app getTime = do
  env <- initAppState getTime
  return $ serve auctionAPI (server env)