{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module Domain.Auctions (module Domain.Auctions) where

import Money
import Domain.Prelude
import Domain.Bids
import qualified Domain.TimedAscending as TA
import qualified Domain.SingleSealedBid as SB
import GHC.Generics
import Data.Time.Clock (secondsToNominalDiffTime, UTCTime, nominalDiffTimeToSeconds)
import qualified Data.Aeson as A
import           Data.Aeson                     ( object
                                                , (.=)
                                                , (.:)
                                                )
import qualified Data.Text as T
import Money
import Data.Fixed (Pico)
import Data.Monoid ((<>))
import           Data.List                      ( intersperse, intercalate )
import           Text.Read                      ( readMaybe )

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TA.Options
  | SingleSealedBid SB.Options
  deriving (Eq, Generic)
instance Show AuctionType where
  show (TimedAscending opt) = 
    intercalate "|" [ "English", (show $ TA.reservePrice opt),( show $ TA.minRaise opt), ( show $ nominalDiffTimeToSeconds $ TA.timeFrame opt)]
  show (SingleSealedBid SB.Blind) = "Blind"
  show (SingleSealedBid SB.Vickrey) = "Vickrey"


instance A.ToJSON AuctionType where
  toJSON at = A.String $ T.pack $ show at

parseAuctionType :: T.Text -> Maybe AuctionType
parseAuctionType t=
  case T.splitOn "|" t of
    "English": a: b: c:[] ->
      let reservePrice = parseAmount $ T.unpack a
          minRaise     = parseAmount $ T.unpack b
          timeframe    = readMaybe $ T.unpack c :: Maybe Pico
      in
        case (reservePrice,minRaise,timeframe) of
          (Just reservePrice', Just minRaise', Just timeframe')->
            pure $ TimedAscending $ TA.Options {
              TA.reservePrice = reservePrice',
              TA.minRaise = minRaise',
              TA.timeFrame = secondsToNominalDiffTime timeframe'
            }
          _ -> Nothing
    "Blind":[]   -> pure $ SingleSealedBid SB.Blind
    "Vickrey":[] -> pure $ SingleSealedBid SB.Vickrey
    _            -> Nothing

instance A.FromJSON AuctionType where
  parseJSON = A.withText "AuctionType" (\t-> case parseAuctionType t of Just at->pure at; Nothing -> fail "failed to parse auction type")

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


instance A.ToJSON Auction where
  toJSON (Auction auctionId startsAt title expiry seller typ auctionCurrency) = object ["id" .= auctionId, "startsAt" .= startsAt, "title" .= title, "expiry" .=expiry, "user" .=seller, "type".=typ, "currency".=auctionCurrency]

instance A.FromJSON Auction where
  parseJSON = A.withObject "Auction" $ \v -> Auction <$> v .: "id" <*> v .: "startsAt" <*> v .: "title" <*> v .: "expiry" <*> v .: "user" <*> v .: "type" <*> v .: "currency"
