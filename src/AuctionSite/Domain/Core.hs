{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}
{-# LANGUAGE InstanceSigs #-}
module AuctionSite.Domain.Core where
import AuctionSite.Money
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import Text.Printf (printf)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as ATyp
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Base64 as B64
import Servant (FromHttpApiData,parseHeader,parseUrlPiece)


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

instance FromHttpApiData User where
  parseHeader :: BS.ByteString -> Either T.Text User
  parseHeader t = 
    case parseUserFromJWT t of
    Just u -> Right u
    Nothing -> Left "unknown"

tryReadJsonUser :: Value -> Maybe User
tryReadJsonUser = ATyp.parseMaybe $ withObject "User" $ \o -> do
  sub' <- o .: "sub"
  name' <- o .:? "name"
  uTyp' <- o .: "u_typ"
  createUser sub' name' uTyp'
  where
    createUser :: UserId -> Maybe T.Text -> T.Text -> Parser User
    createUser sub (Just name) "0" = pure $ BuyerOrSeller sub name
    createUser sub _           "1" = pure $ Support sub
    createUser _   _           _   = ATyp.prependFailure "parsing User failed, " (fail "could not interpret values")

-- Authentication function
parseUserFromJWT :: BS.ByteString -> Maybe User
parseUserFromJWT token = do
  readAndDecodeBase64 token
  where
    readAndDecodeBase64 :: BS.ByteString -> Maybe User
    readAndDecodeBase64 v = decodeBase64 v >>= decode >>= tryReadJsonUser
    decodeBase64 :: BS.ByteString -> Maybe LB.ByteString
    decodeBase64 v =  case B64.decode v of
                      Right b -> pure (LB.fromStrict b)
                      Left _ -> Nothing

type AuctionId = Integer

data Errors =
  UnknownAuction AuctionId
  | AuctionAlreadyExists AuctionId
  | AuctionHasEnded AuctionId
  | AuctionHasNotStarted AuctionId
  | SellerCannotPlaceBids (UserId , AuctionId)
  | CurrencyConversion Currency
  | InvalidUserData String
  | MustPlaceBidOverHighestBid Amount
  | AlreadyPlacedBid
  deriving (Eq,Show)
  