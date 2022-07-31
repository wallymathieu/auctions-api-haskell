{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Web.Spock
import           Web.Spock.Config
import           AuctionSite.Web.App

main :: IO ()
main = do
  state <- initAppState
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  runSpock 8080 (spock spockCfg app)
