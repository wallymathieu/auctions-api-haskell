-- app/Main.hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Time as Time
import Network.Wai.Handler.Warp (run)

import AuctionSite.Web.API (app)

main :: IO ()
main = do
  putStrLn "Starting auction site server on port 8080..."
  application <- app Time.getCurrentTime
  run 8080 application
