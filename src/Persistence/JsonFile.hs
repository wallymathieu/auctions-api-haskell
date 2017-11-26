module Persistence.JsonFile where
import Data.Aeson

import qualified Money as M
import qualified Domain.Prelude as DP
import qualified Domain.Auctions as A
import qualified Domain.Bids as B
import qualified Domain.Commands as C
import qualified Domain.SingleSealedBid as DS
import qualified Domain.TimedAscending as DT
import qualified Data.ByteString.Lazy.Char8 as BS

instance ToJSON M.Currency
instance FromJSON M.Currency

instance ToJSON M.Amount
instance FromJSON M.Amount

instance ToJSON DS.Options
instance FromJSON DS.Options

instance ToJSON DT.Options
instance FromJSON DT.Options

instance ToJSON A.AuctionType
instance FromJSON A.AuctionType

instance ToJSON A.Auction
instance FromJSON A.Auction

instance ToJSON C.Command
instance FromJSON C.Command  
  
instance ToJSON B.Bid
instance FromJSON B.Bid  

readCommands :: FilePath -> IO (Maybe [C.Command])
readCommands path=do
  c <- readFile path
  return $ decode $ BS.pack c 

writeCommands :: FilePath -> [C.Command] -> IO ()
writeCommands path commands=writeFile path c
  where c = BS.unpack $ encode commands
