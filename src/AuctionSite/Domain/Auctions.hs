{-# LANGUAGE DeriveGeneric,OverloadedStrings  #-}
module AuctionSite.Domain.Auctions where

import AuctionSite.Money
import AuctionSite.Domain.Core
import AuctionSite.Domain.Bids
import qualified AuctionSite.Domain.TimedAscending as TA
import qualified AuctionSite.Domain.SingleSealedBid as SB
import GHC.Generics
import Data.Time
import Data.Aeson
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import AuctionSite.Aeson (toJsonOfShow, ofJsonOfRead)

data AuctionType=
  {- also known as an open ascending price auction
  The auction ends when no participant is willing to bid further -}
  TimedAscending TA.Options
  | SingleSealedBid SB.Options
  deriving (Eq, Generic)
instance Show AuctionType where
  show (TimedAscending opt) = show opt
  show (SingleSealedBid opt) = show opt
instance Read AuctionType where
  readsPrec _ v = interpret readMaybeTyp
    where
    readMaybeTyp = ( TimedAscending <$> readMaybe v ) <|> ( SingleSealedBid <$> readMaybe v )
    interpret (Just auctionType) = [(auctionType,"")]
    interpret Nothing = []

data Auction = Auction { auctionId :: AuctionId,
  startsAt :: UTCTime,
  title :: String,
  -- | initial expiry
  expiry :: UTCTime,
  seller :: User,
  typ :: AuctionType,
  auctionCurrency :: Currency
} deriving (Eq, Generic, Show)

validateBid:: Bid->Auction->Either Errors ()
validateBid bid auction
  | bidder bid == seller auction =
    Left (SellerCannotPlaceBids (userId $ bidder bid, auctionId auction))
  | otherwise = Right ()

type AuctionState = Either SB.State TA.State

emptyState :: Auction -> AuctionState
emptyState Auction{ typ=SingleSealedBid opt, expiry=expiry' } = Left (SB.emptyState expiry' opt)
emptyState Auction{ typ=TimedAscending opt, expiry=expiry', startsAt=startsAt' } = Right (TA.emptyState startsAt' expiry' opt)

instance ToJSON AuctionType where
  toJSON = toJsonOfShow
instance FromJSON AuctionType where
  parseJSON = ofJsonOfRead "AuctionType"

instance ToJSON Auction where
  toJSON Auction { auctionId=auctionId', startsAt=startsAt', title=title', expiry=expiry', seller=seller', typ=typ', auctionCurrency=auctionCurrency' } =
    object [ "id".=auctionId',
             "startsAt" .= startsAt',
             "title" .= title',
             "expiry" .= expiry',
             "user" .= seller',
             "type" .= typ',
             "currency" .= auctionCurrency' ]
instance FromJSON Auction where
  parseJSON = withObject "Auction" $ \obj -> do
    auctionId' <- obj .: "id"
    startsAt' <- obj .: "startsAt"
    title' <- obj .: "title"
    expiry' <- obj .: "expiry"
    seller' <- obj .: "user"
    typ' <- obj .: "type"
    currency' <- obj .: "currency"
    return (Auction { auctionId=auctionId', startsAt=startsAt', title=title', expiry=expiry', seller=seller', typ=typ', auctionCurrency=currency' })
