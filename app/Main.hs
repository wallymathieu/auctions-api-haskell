module Main where
import           Data.Time (getCurrentTime)
import           Web.Scotty
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
  scotty 8080 $ app state onEvent getCurrentTime
