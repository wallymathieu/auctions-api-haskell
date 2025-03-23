module Main where
import           Data.Time (getCurrentTime)
import           Web.Spock
import           Web.Spock.Config
import qualified Data.Map as Map
import           AuctionSite.Web.App
import           AuctionSite.Domain.Commands
import           AuctionSite.Persistence.JsonFile

eventsFile = "tmp/events.jsonl"
onEvent :: Event -> IO ()
onEvent e = writeEvents eventsFile [e]
main :: IO ()
main = do
  events <- readEvents eventsFile >>= maybe (return []) return
  state <- initAppState $ eventsToAuctionStates events
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  runSpock 8080 (spock spockCfg (app onEvent getCurrentTime))
