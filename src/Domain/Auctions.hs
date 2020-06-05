{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.Auctions (module Domain.Auctions) where

import Money
import Domain.Prelude
import Domain.Bids
import qualified Domain.TimedAscending as TA
import qualified Domain.SingleSealedBid as SB
import GHC.Generics
import Data.Time.Clock (secondsToNominalDiffTime, UTCTime)
import Data.Aeson
import qualified Data.Text as T
import Money
import Data.Fixed (Pico)
import Data.Monoid ((<>))

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TA.Options
  | SingleSealedBid SB.Options
  deriving (Eq, Generic, Show)

instance ToJSON AuctionType where
    toJSON (TimedAscending opt) = 
      String $ T.intercalate "|" ["English",T.pack $ show $ TA.reservePrice opt, T.pack $ show $ TA.minRaise opt, T.pack $ show $ TA.timeFrame opt]
    toJSON (SingleSealedBid SB.Blind) = String "Blind"
    toJSON (SingleSealedBid SB.Vickrey) = String "Vickrey"

instance FromJSON AuctionType where
    parseJSON (String t)  = case T.splitOn "|" t of
        "English": reservePrice: minRaise: timeframe:[] ->
          let t = read $ T.unpack timeframe ::Pico in
            pure $ TimedAscending $ TA.Options {TA.reservePrice =read $ T.unpack reservePrice, TA.minRaise =read $ T.unpack minRaise, TA.timeFrame = secondsToNominalDiffTime t } 
        "Blind":[]   -> pure $ SingleSealedBid SB.Blind
        "Vickrey":[] -> pure $ SingleSealedBid SB.Vickrey 
        _           -> fail $ "Unknown auction type: " <> T.unpack t
    parseJSON _ = fail $ "Unknown auction type"


data Auction = Auction { auctionId :: AuctionId,
  startsAt :: UTCTime,
  title :: String,
  -- initial expiry
  expiry :: UTCTime,
  seller :: UserId,
  typ :: AuctionType,
  auctionCurrency :: Currency
} deriving (Eq, Generic, Show)

validateBid:: Bid->Auction->Either Errors ()
validateBid bid auction
  | bidder bid == seller auction =
    Left (SellerCannotPlaceBids (bidder bid, auctionId auction))
  | amountCurrency (bidAmount bid) /= auctionCurrency auction =
    Left (BidCurrencyConversion (bidId bid, auctionCurrency auction))
  | otherwise = Right ()

type AuctionState = Either SB.State TA.State
  
emptyState :: Auction -> AuctionState
emptyState a =
  case typ a of
  SingleSealedBid opt -> Left (SB.emptyState (expiry a) opt)
  TimedAscending opt -> Right (TA.emptyState (startsAt a) (expiry a) opt)



instance ToJSON Auction
instance FromJSON Auction
