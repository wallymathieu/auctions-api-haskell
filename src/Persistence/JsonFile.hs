module Persistence.JsonFile where
import Data.Aeson

import qualified Domain.Commands as C
import qualified Data.ByteString.Lazy.Char8 as BS

readCommands :: FilePath -> IO (Maybe [C.Command])
readCommands path=do
  c <- readFile path
  return $ decode $ BS.pack c 

writeCommands :: FilePath -> [C.Command] -> IO ()
writeCommands path commands=writeFile path c
  where c = BS.unpack $ encode commands
