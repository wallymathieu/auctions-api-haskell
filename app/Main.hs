module Main where
import           Data.Time (getCurrentTime)
import           Web.Scotty
import qualified Data.Map as Map
import           AuctionSite.Web.App
import           AuctionSite.Domain.Commands
import           AuctionSite.Persistence.JsonFile
import           AuctionSite.Worker
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Monad (forever)

eventsFile = "tmp/events.jsonl"

main :: IO ()
main = do
  eventQueue <- atomically $ newTBQueue 1000
  worker <- startEventWorker (writeEvents eventsFile) eventQueue
  onEvent <- createEventHandler eventQueue

  events <- readEvents eventsFile >>= maybe (return []) return
  state <- initAppState $ eventsToAuctionStates events
  scotty 8080 $ app state onEvent getCurrentTime

  wait worker
