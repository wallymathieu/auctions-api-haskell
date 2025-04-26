module Main where
import           AuctionSite.Domain.Commands
import           AuctionSite.Persistence.JsonFile
import           AuctionSite.Web.API (app)
import           AuctionSite.Worker

import qualified Data.Time as Time
import           Network.Wai.Handler.Warp (run)
import qualified Data.Map as Map
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
  putStrLn "Starting auction site server on port 8080..."
  application <- app (eventsToAuctionStates events) onEvent Time.getCurrentTime
  run 8080 application

  wait worker
