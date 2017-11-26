{-# LANGUAGE DeriveGeneric     #-}
module Money where
import GHC.Generics

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

amountCurrency (Amount c _) = c
  
amountValue (Amount _ v) = v

amountAdd (Amount ac av) (Amount bc bv) =
  if ac == bc then
    Amount ac (av + bv)
  else
    error "Cant add two amounts with different currency"

