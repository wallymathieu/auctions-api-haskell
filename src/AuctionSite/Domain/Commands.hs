{-# LANGUAGE DeriveGeneric,OverloadedStrings   #-}
module AuctionSite.Domain.Commands where
import           AuctionSite.Domain.Auctions
import           AuctionSite.Domain.Bids
import           AuctionSite.Domain.Core
import           AuctionSite.Domain.States

import           GHC.Generics
import           Data.Time
import           Data.Aeson
import qualified Data.Map as Map


data Command =
  AddAuction UTCTime Auction
  | PlaceBid UTCTime Bid
  deriving (Generic, Show, Eq)

data Event =
  AuctionAdded UTCTime Auction
  | BidAccepted UTCTime Bid
  deriving (Generic, Show, Eq)

-- | Fold to map of auction that contains both the auction and state
eventsToAuctionStates :: [Event] -> Map.Map AuctionId (Auction, AuctionState)
eventsToAuctionStates = foldl folder Map.empty
  where
    folder :: Map.Map AuctionId (Auction, AuctionState) -> Event -> Map.Map AuctionId (Auction, AuctionState)
    folder auctions event = case event of
      AuctionAdded _ auction ->
        let empty = emptyState auction
        in Map.insert (auctionId auction) (auction, empty) auctions
      BidAccepted _ b ->
        case Map.lookup (forAuction b) auctions of
          Just (auction, state) ->
            let (next, _) = addBid b state
            in Map.insert (forAuction b) (auction, next) auctions
          Nothing -> error "could not find auction"

instance ToJSON Command where
  toJSON (AddAuction time auction) =  object ["$type" .= String "AddAuction", "at" .= time, "auction" .= auction]
  toJSON (PlaceBid time bid) =  object ["$type" .= String "PlaceBid", "at" .= time, "bid" .= bid]

instance FromJSON Command where
  parseJSON (Object v) = do
    typ' <-v .: "$type"
    case typ' of
      String "AddAuction" -> AddAuction <$> v .: "at" <*> v .: "auction"
      String "PlaceBid"   -> PlaceBid <$> v .: "at" <*> v .: "bid"
      _                   -> fail "Unknown command type"
  parseJSON _ = fail "Unexpected json command"

instance ToJSON Event where
  toJSON (AuctionAdded time auction) = object ["$type" .= String "AuctionAdded", "at" .= time, "auction" .= auction]
  toJSON (BidAccepted time bid) = object ["$type" .= String "BidAccepted", "at" .= time, "bid" .= bid]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \obj -> do
    eventType <- obj .: "$type"
    case eventType of
      "AuctionAdded" -> do
        at' <- obj .: "at"
        auction <- obj .: "auction"
        return $ AuctionAdded at' auction

      "BidAccepted" -> do
        at' <- obj .: "at"
        bid <- obj .: "bid"
        return $ BidAccepted at' bid

      _ -> fail $ "Unknown event type: " ++ eventType
