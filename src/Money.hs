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

data Amount = Amount { 
  currency :: Currency,
  value :: Integer 
} deriving (Generic, Show, Eq, Ord)

amountAdd Amount{ currency=ac,value=av} Amount{ currency=bc,value=bv} =
  if ac == bc then
    Amount{ currency=ac,value= av + bv}
  else
    error "Cant add two amounts with different currency"

