module Money (Currency, Amount) where

data Currency = 
  -- virtual acution currency
  VAC
  -- Swedish 'Krona'
  |SEK
  -- Danish 'Krone'
  |DKK
  deriving Show

data Amount = Amount { currency :: Currency,
                       value :: Integer } deriving Show
