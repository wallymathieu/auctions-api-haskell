{-# LANGUAGE RecordWildCards #-}
module Main where
  
import           Data.Aeson       hiding (json)
import           Data.Monoid      ((<>))
import           Data.Text        (pack)
import           GHC.Generics
import qualified Prelude as P
import Auctions.Api
import Auctions.Handlers

-- import qualified Money as M
import qualified Domain.Prelude as DP
-- import qualified Domain.Auctions as A
-- import qualified Domain.Bids as B
-- import qualified Domain.Commands as C
-- import qualified Domain.SingleSealedBid as DS
-- import qualified Domain.TimedAscending as DT


-- Run a Auction server on localhost:8080
main :: IO ()
main = do
  let server = AuctionBackend{..}
  runAuctionServer (ServerConfig "localhost" 8080) server
