module AuctionSite.Web.App where

import Control.Concurrent.STM
import Data.Map as Map
import Data.Time.Clock (UTCTime)

import AuctionSite.Web.API (app, AppEnv(..))
import AuctionSite.Web.Types (Repository)

-- | Initialize app state
initAppState :: IO (TVar Repository)
initAppState = newTVarIO Map.empty

-- | Initialize the application environment
initAppEnv :: IO UTCTime -> IO AppEnv
initAppEnv getTime = do
  auctions' <- initAppState
  return AppEnv
    { appAuctions = auctions'
    , getCurrentTime = getTime
    }