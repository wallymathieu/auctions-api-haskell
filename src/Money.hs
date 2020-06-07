{-# LANGUAGE DeriveGeneric     #-}
module Money where
import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import           Text.Read                      ( readMaybe )
import           Text.Regex.PCRE                (Regex, matchOnceText, makeRegex)
import qualified Text.Regex.PCRE as R
import           Data.Array

data Currency =
  -- virtual acution currency
  VAC
  -- Swedish 'Krona'
  |SEK
  -- Danish 'Krone'
  |DKK
  deriving (Generic, Show, Eq, Ord, Read)

data Amount = Amount Currency Integer
  deriving (Generic, Eq, Ord, Read)

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

parseAmount :: String -> Maybe Amount
parseAmount t=
    case matchOnceText r t of
      Just (_,captured,_) ->
        let [_,currency,amount] = map fst $ elems captured in
        case Amount <$> (readMaybe currency) <*> (readMaybe amount) of
          Just a -> pure a
          _ -> Nothing
      _ -> Nothing
    where
      r = makeRegex "([A-Z]+)([0-9]+)" :: Regex

instance ToJSON Currency
instance FromJSON Currency
instance Show Amount where
  show (Amount currency value) = (show currency) ++ (show value)

instance ToJSON Amount where
  toJSON amount = String $ T.pack (show amount)

instance FromJSON Amount where
  parseJSON (String t)  =
      case parseAmount (T.unpack t) of
        Just a ->pure a
        _ -> fail "failed to parse amount"
  parseJSON _ = fail "invalid json token for amount"

