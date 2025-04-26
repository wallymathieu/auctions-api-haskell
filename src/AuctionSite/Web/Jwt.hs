{-# LANGUAGE OverloadedStrings #-}
module AuctionSite.Web.Jwt where

import           Data.Aeson
import           Data.Aeson.Types          (parseMaybe)
import           Prelude
import qualified Data.ByteString.Base64    as B64
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LB
import qualified Data.Aeson.Types          as ATyp
import qualified Data.Text                 as T
import           AuctionSite.Domain

newtype JwtUser = JwtUser { unWrapJwtUser :: User }
instance FromJSON JwtUser where
  parseJSON = withObject "JwtUser" $ \o -> do
    sub' <- o .: "sub"
    name' <- o .:? "name"
    uTyp' <- o .: "u_typ"
    createJwtUser sub' name' uTyp'

createJwtUser :: UserId -> Maybe T.Text -> T.Text -> ATyp.Parser JwtUser
createJwtUser sub (Just name) "0" = pure $ JwtUser $ BuyerOrSeller sub name
createJwtUser sub _           "1" = pure $ JwtUser $ Support sub
createJwtUser _   _           _   = ATyp.prependFailure "parsing User failed, " (fail "could not interpret values")

decodeJwtUser :: ByteString -> Maybe JwtUser
decodeJwtUser byteString = decodeBase64 byteString >>= decode >>= tryReadUserId
  where
    tryReadUserId :: Value -> Maybe JwtUser
    tryReadUserId = parseMaybe parseJSON
    decodeBase64 :: ByteString -> Maybe LB.ByteString
    decodeBase64 v = case B64.decode v of
                      Right b -> pure (LB.fromStrict b)
                      Left _ -> Nothing
