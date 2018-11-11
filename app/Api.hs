{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -fcontext-stack=328 #-}

module Auction.API
  -- * Client and Server
  ( ServerConfig(..)
  , AuctionBackend
  , createAuctionClient
  , runAuctionServer
  , runAuctionClient
  , runAuctionClientWithManager
  , AuctionClient
  -- ** Servant
  , AuctionAPI
  ) where

import Auction.Types

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp as Warp
import Servant (ServantErr, serve)
import Servant.API
import Servant.API.Verbs (StdMethod(..), Verb)
import Servant.Client (Scheme(Http), ServantError, client)
import Servant.Common.BaseUrl (BaseUrl(..))
import Web.HttpApiData




-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result -> Left $ T.unpack result
        Right result -> Right $ result

-- | Servant type-level API, generated from the Swagger spec for Auction.
type AuctionAPI
    =    "auction" :> ReqBody '[JSON] AddAuction :> Verb 'POST 200 '[JSON] () -- 'addAuction' route
    :<|> "auction" :> Capture "id" Integer :> "bid" :> ReqBody '[JSON] AddBid :> Verb 'POST 200 '[JSON] () -- 'addBid' route
    :<|> "auction" :> Capture "id" Integer :> Verb 'GET 200 '[JSON] Auction -- 'getAuction' route
    :<|> "auctions" :> QueryParam "searchString" Text :> QueryParam "skip" Int :> QueryParam "limit" Int :> Verb 'GET 200 '[JSON] [Auction] -- 'searchAuctions' route

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Backend for Auction.
-- The backend can be used both for the client and the server. The client generated from the Auction Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createAuctionClient@). Alternatively, provided
-- a backend, the API can be served using @runAuctionServer@.
data AuctionBackend m = AuctionBackend
  { addAuction :: AddAuction -> m (){- ^ Adds an auction to the system -}
  , addBid :: Integer -> AddBid -> m (){- ^ Adds a bid to an auction -}
  , getAuction :: Integer -> m Auction{- ^ Get a single auction  -}
  , searchAuctions :: Maybe Text -> Maybe Int -> Maybe Int -> m [Auction]{- ^ By passing in the appropriate options, you can search for available auctions in the system  -}
  }

newtype AuctionClient a = AuctionClient
  { runClient :: Manager -> BaseUrl -> ExceptT ServantError IO a
  } deriving Functor

instance Applicative AuctionClient where
  pure x = AuctionClient (\_ _ -> pure x)
  (AuctionClient f) <*> (AuctionClient x) =
    AuctionClient (\manager url -> f manager url <*> x manager url)

instance Monad AuctionClient where
  (AuctionClient a) >>= f =
    AuctionClient (\manager url -> do
      value <- a manager url
      runClient (f value) manager url)

instance MonadIO AuctionClient where
  liftIO io = AuctionClient (\_ _ -> liftIO io)

createAuctionClient :: AuctionBackend AuctionClient
createAuctionClient = AuctionBackend{..}
  where
    ((coerce -> addAuction) :<|>
     (coerce -> addBid) :<|>
     (coerce -> getAuction) :<|>
     (coerce -> searchAuctions)) = client (Proxy :: Proxy AuctionAPI)

-- | Run requests in the AuctionClient monad.
runAuctionClient :: ServerConfig -> AuctionClient a -> ExceptT ServantError IO a
runAuctionClient clientConfig cl = do
  manager <- liftIO $ newManager defaultManagerSettings
  runAuctionClientWithManager manager clientConfig cl

-- | Run requests in the AuctionClient monad using a custom manager.
runAuctionClientWithManager :: Manager -> ServerConfig -> AuctionClient a -> ExceptT ServantError IO a
runAuctionClientWithManager manager clientConfig cl =
  runClient cl manager $ BaseUrl Http (configHost clientConfig) (configPort clientConfig) ""

-- | Run the Auction server at the provided host and port.
runAuctionServer :: MonadIO m => ServerConfig -> AuctionBackend (ExceptT ServantErr IO)  -> m ()
runAuctionServer ServerConfig{..} backend =
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy AuctionAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend AuctionBackend{..} =
      (coerce addAuction :<|>
       coerce addBid :<|>
       coerce getAuction :<|>
       coerce searchAuctions)
