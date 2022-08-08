{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}
module AuctionSite.Money (
  Currency(..),
  Amount(..),
  amountCurrency,
  amountValue,
  amountAdd
) where
import GHC.Generics
import Data.Aeson
import Text.Printf (printf)
import Text.Parsec
import AuctionSite.Aeson

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
  readsPrec _ v = interpret $ runParser parser () "readsPrec" v 
    where
      interpret (Right val)= [(val,"")]
      interpret (Left _) = []
      currencyParser:: Parsec String st Currency
      currencyParser = read <$> many letter
      amountParser:: Parsec String st Integer
      amountParser = read <$> many digit
      parser = Amount <$> currencyParser <*> amountParser

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


instance ToJSON Currency where
  toJSON = toJsonOfShow
instance FromJSON Currency where
  parseJSON = ofJsonOfRead "Currency"
instance ToJSON Amount where
  toJSON = toJsonOfShow
instance FromJSON Amount where
  parseJSON = ofJsonOfRead "Amount"

