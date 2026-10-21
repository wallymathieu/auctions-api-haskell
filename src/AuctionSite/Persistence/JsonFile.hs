module AuctionSite.Persistence.JsonFile where
import Data.Aeson
import System.Directory

import qualified AuctionSite.Domain.Commands as C
import qualified Data.ByteString.Lazy.Char8 as BS

-- Generic read function for any JSON-decodable type
readJsonFile :: FromJSON a => FilePath -> IO (Maybe [a])
readJsonFile path = do
  doesFileExist path >>= \exists ->
    if not exists
    then return Nothing
    else do
      c <- BS.readFile path
      case mapM decode (BS.lines c) of
        Just items -> return $ Just (concat items)
        Nothing -> return Nothing

-- Generic write function for any JSON-encodable type
writeJsonFile :: ToJSON a => FilePath -> [a] -> IO ()
writeJsonFile path items =
  doesFileExist path >>= \exists ->
    if not exists
    then BS.writeFile path c
    else BS.appendFile path (BS.cons '\n' c)
  where c = encode items

-- Specialized functions using the generic implementations
readCommands :: FilePath -> IO (Maybe [C.Command])
readCommands = readJsonFile

writeCommands :: FilePath -> [C.Command] -> IO ()
writeCommands = writeJsonFile

readEvents :: FilePath -> IO (Maybe [C.Event])
readEvents = readJsonFile

writeEvents :: FilePath -> [C.Event] -> IO ()
writeEvents = writeJsonFile
