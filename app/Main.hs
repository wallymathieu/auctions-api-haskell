{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Time (getCurrentTime)
import           Web.Scotty
import           AuctionSite.Web.App

main :: IO ()
main = do
  state <- initAppState
  scotty 8080 $ app state getCurrentTime