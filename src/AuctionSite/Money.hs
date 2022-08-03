{-# LANGUAGE DeriveGeneric     #-}
module AuctionSite.Money where
import GHC.Generics
import Data.Aeson

data Currency = 
  -- virtual acution currency
  VAC
  -- Swedish 'Krona'
  |SEK
  -- Danish 'Krone'
  |DKK
  deriving (Generic, Show, Eq, Ord)

data Amount = Amount Currency Integer
  deriving (Generic, Show, Eq, Ord)

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

instance ToJSON Amount
instance FromJSON Amount
  
