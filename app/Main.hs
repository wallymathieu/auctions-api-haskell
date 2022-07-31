{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Web.Spock
import           Web.Spock.Config

import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           GHC.Generics
import           GHC.Conc        (TVar,newTVar,readTVar,readTVarIO,atomically)
import           GHC.Conc.Sync   (STM)
import           Prelude
import           Control.Monad.IO.Class (liftIO)
import qualified Network.HTTP.Types.Status as Http
import qualified Data.Text       as T
import qualified Data.List       as L

import qualified AuctionSite.Domain.Prelude  as DP
import qualified AuctionSite.Domain.Auctions as A
import qualified AuctionSite.Domain.Bids     as B
import qualified AuctionSite.Domain.Commands as C
import Data.Foldable (find)


main :: IO ()
main = do
  spockCfg <- defaultSpockCfg () PCNoDatabase (newTVar [])
  runSpock 8080 (spock spockCfg app)
