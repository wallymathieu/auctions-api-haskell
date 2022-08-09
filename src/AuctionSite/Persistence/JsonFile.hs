module AuctionSite.Persistence.JsonFile where
import Data.Aeson

import qualified AuctionSite.Domain.Commands as C
import qualified Data.ByteString.Lazy.Char8 as BS

readCommands :: FilePath -> IO (Maybe [C.Command])
readCommands path=do
  c <- readFile path

  case mapM parseCommand (lines c) of
    Just cs -> return $ Just (concat cs)
    Nothing -> return Nothing
  where parseCommand v = decode $ BS.pack v :: Maybe [C.Command]

writeCommands :: FilePath -> [C.Command] -> IO ()
writeCommands path commands=writeFile path c
  where c = BS.unpack $ encode commands
