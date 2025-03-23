module Main where
import           AuctionSite.Domain.Commands
import           AuctionSite.Persistence.JsonFile
import           AuctionSite.Web.API (app)

import qualified Data.Time as Time
import           Network.Wai.Handler.Warp (run)
import qualified Data.Map as Map

eventsFile = "tmp/events.jsonl"
onEvent :: Event -> IO ()
onEvent e = writeEvents eventsFile [e]
main :: IO ()
main = do
  events <- readEvents eventsFile >>= maybe (return []) return
  putStrLn "Starting auction site server on port 8080..."
  application <- app (eventsToAuctionStates events) onEvent Time.getCurrentTime
  run 8080 application
