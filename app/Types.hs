{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Auction.Types (
  AddAuction (..),
  AddBid (..),
  Amount (..),
  Auction (..),
  AuctionId (..),
  Bid (..),
  BuyerOrSeller (..),
  Currency (..),
  ) where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson (Value, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data AddAuction = AddAuction
  { addAuctionId :: AuctionId -- ^ 
  , addAuctionStartsAt :: Integer -- ^ 
  , addAuctionEndsAt :: Integer -- ^ 
  , addAuctionTitle :: Text -- ^ 
  , addAuctionCurrency :: Currency -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AddAuction where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "addAuction")
instance ToJSON AddAuction where
  toJSON = genericToJSON (removeFieldLabelPrefix False "addAuction")

-- | 
data AddBid = AddBid
  { addBidAmount :: Amount -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON AddBid where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "addBid")
instance ToJSON AddBid where
  toJSON = genericToJSON (removeFieldLabelPrefix False "addBid")

-- | 
newtype Amount = Amount Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype Auction = Auction { unAuction :: AddAuction }
  deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype AuctionId = AuctionId Double deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
data Bid = Bid
  { bidBidder :: BuyerOrSeller -- ^ 
  , bidAmount :: Amount -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Bid where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "bid")
instance ToJSON Bid where
  toJSON = genericToJSON (removeFieldLabelPrefix False "bid")

-- | 
newtype BuyerOrSeller = BuyerOrSeller Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | 
newtype Currency = Currency Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
