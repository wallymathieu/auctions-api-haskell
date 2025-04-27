{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module AuctionSite.Money (
  Currency(..),
  Amount(..),
  AmountValue,
  amountCurrency,
  amountValue,
  (+)
) where
import           GHC.Generics
import           Data.Aeson
import           Text.Printf (printf)
import           Text.Parsec
import           AuctionSite.Aeson
import           Prelude hiding ((+))
import qualified Prelude as P

data Currency =
  VAC -- ^ virtual acution currency
  |SEK -- ^ Swedish 'Krona'
  |DKK -- ^ Danish 'Krone'
  deriving (Generic, Show, Eq, Ord, Read)

data Amount = Amount Currency Integer
  deriving (Generic, Eq, Ord)
instance Show Amount where
  show (Amount c i) = printf "%s%i" (show c) i
instance Read Amount where
  readsPrec _ v = interpret $ runParser parser () "Amount" v
    where
      interpret (Right val)= [(val,"")]
      interpret (Left _) = []
      currencyParser:: Parsec String st Currency
      currencyParser = read <$> many letter
      amountParser:: Parsec String st AmountValue
      amountParser = read <$> many digit
      parser = Amount <$> currencyParser <*> amountParser

type AmountValue = Integer

amountCurrency :: Amount -> Currency
amountCurrency (Amount c _) = c

amountValue :: Amount -> AmountValue
amountValue (Amount _ v) = v

(+) :: Amount -> Amount -> Amount
(+) (Amount ac av) (Amount bc bv) =
  if ac == bc then
    Amount ac (av P.+ bv)
  else
    error "Cant add two amounts with different currency"


instance ToJSON Currency where
  toJSON = toJsonOfShow
instance FromJSON Currency where
  parseJSON = ofJsonOfRead "Currency"
instance ToJSON Amount where
  toJSON = toJsonOfShow
instance FromJSON Amount where
  parseJSON = ofJsonOfRead "Amount"

