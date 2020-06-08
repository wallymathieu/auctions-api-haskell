{-# LANGUAGE RecordWildCards #-}
module Main where
  
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (pack)
import           GHC.Generics
import qualified Prelude as P

-- import qualified Money as M
import qualified Domain.Prelude as DP
import qualified Domain.Auctions as A
import qualified Domain.Commands as C


-- Run a Auction server on localhost:8080
main :: IO ()
main = do
  let server = AuctionBackend{..}
  runAuctionServer (ServerConfig "localhost" 8080) server
