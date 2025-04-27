{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}
module AuctionSite.Domain.Core where
import AuctionSite.Money
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as ATyp

type UserId = T.Text
data User =
  BuyerOrSeller UserId T.Text
  | Support UserId
  deriving (Eq, Generic, Show)
userId :: User -> UserId
userId (BuyerOrSeller userId' _) = userId'
userId (Support userId') = userId'

instance ToJSON User where
  toJSON (BuyerOrSeller userId' name) = String $ T.pack ( printf "BuyerOrSeller|%s|%s" userId' name )
  toJSON (Support userId') = String $ T.pack ( printf "Support|%s" userId' )
instance FromJSON User where
  parseJSON = withText "User" (interpret . T.splitOn "|")
    where
      interpret :: [T.Text] -> Parser User
      interpret ["BuyerOrSeller", userId', name'] = pure $ BuyerOrSeller userId' name'
      interpret ["Support", userId'] = pure $ Support userId'
      interpret _ = ATyp.prependFailure "parsing User failed, " (fail "could not interpret values")

type AuctionId = Integer

data Errors =
  UnknownAuction AuctionId
  | AuctionAlreadyExists AuctionId
  | AuctionHasEnded AuctionId
  | AuctionHasNotStarted AuctionId
  | SellerCannotPlaceBids (UserId , AuctionId)
  | InvalidUserData String
  | MustPlaceBidOverHighestBid AmountValue
  | AlreadyPlacedBid
  deriving (Eq,Show)
