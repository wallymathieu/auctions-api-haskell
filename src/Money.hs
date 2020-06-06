{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Money where
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import           Text.RE.TDFA.String
import           Text.Read                      ( readMaybe )

data Currency = 
  -- virtual acution currency
  VAC
  -- Swedish 'Krona'
  |SEK
  -- Danish 'Krone'
  |DKK
  deriving (Generic, Show, Eq, Ord, Read)

data Amount = Amount Currency Integer
  deriving (Generic, Show, Eq, Ord, Read)

amountCurrency :: Amount -> Currency
amountCurrency (Amount c _) = c

amountValue :: Amount -> Integer
amountValue (Amount _ v) = v

amountAdd :: Amount -> Amount -> Amount
amountAdd (Amount ac av) (Amount bc bv) =
  if ac == bc then
    Amount ac (av + bv)
  else
    error "Cant add two amounts with different currency"

instance ToJSON Currency
instance FromJSON Currency

instance ToJSON Amount where
  toJSON (Amount currency value) =
    String $ T.pack ((show currency) ++ (show value))

instance FromJSON Amount {- where
  parseJSON (String t)  =
    let r =matches $ (T.unpack t) *=~ [re|([A-Z]+)|([0-9]+)|] in
      case r of
        [currency,amount] -> case Amount <$> (readMaybe currency :: Maybe Currency) <*> (readMaybe amount :: Maybe Integer) of 
          Just a -> pure a 
          _ -> fail "failed to parse"
        _ -> fail "failed to extract"
  parseJSON _ = fail "invalid json"
-}
