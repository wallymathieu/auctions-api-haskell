{-# LANGUAGE DeriveGeneric     #-}
module Domain.Commands where
import Money
import Domain.Prelude
import Domain.Auction
import Domain.Bid
import GHC.Generics

data Command = 
  AddAuction DateTime Auction
  | PlaceBid DateTime Bid
  deriving (Generic, Show)


data CommandSuccess = 
  AuctionAdded DateTime Auction
  | BidAccepted DateTime Bid
  deriving (Generic, Show)
