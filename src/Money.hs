{-# LANGUAGE DeriveGeneric     #-}
module Money where
import           GHC.Generics
import           Data.Aeson hiding (parse)
import qualified Data.Text as T
import           Text.Read                      ( readMaybe )
import           Text.Regex.PCRE                (Regex, matchOnceText, makeRegex)
import qualified Text.Regex.PCRE as R
import           Data.Array
import qualified Prelude as P
import           Prelude hiding ((+))
import           Parser
infixl 6  +
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

(+) :: Amount -> Amount -> Amount
(+) (Amount ac av) (Amount bc bv) =
  if ac == bc then
    Amount ac (av P.+ bv)
  else
    error "Cant add two amounts with different currency"

instance Parser Amount where
  parse t=
      case matchOnceText regex (T.unpack t) of
        Just (_,captured,_) ->
          let [_,currency,amount] = map fst $ elems captured in
          case Amount <$> readMaybe currency <*> readMaybe amount of Just a -> pure a ; _ -> Nothing
        _ -> Nothing
      where
        regex = makeRegex "([A-Z]+)([0-9]+)" :: Regex

instance ToJSON Currency
instance FromJSON Currency
instance P.Show Amount where
  show (Amount currency value) = show currency ++ show value

instance ToJSON Amount where
  toJSON amount = String $ T.pack (show amount)

instance FromJSON Amount where
  parseJSON (String t)  =
      case parse t of
        Just a ->pure a
        _ -> fail "failed to parse amount"
  parseJSON _ = fail "invalid json token for amount"

