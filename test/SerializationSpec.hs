{-# LANGUAGE OverloadedStrings #-}
module SerializationSpec where
import Domain.Prelude
import Domain.Bids
import Domain.Auctions
import Domain.SingleSealedBid

import Money
import Data.Time
import Persistence.JsonFile
import Test.Hspec
import Data.Aeson
import qualified SampleData as S 
import qualified Domain.Commands as C
import qualified Data.ByteString.Lazy.Char8 as BS

auction = S.sampleAuction()
addAuction = C.AddAuction S.sampleStartsAt auction

spec:: ()->SpecWith ()
spec () = do
  describe "can read commands from jsonl" $ do
    it "reading commands from jsonl" $ do
      cmds <- readCommands "./test/samples/sample-commands.jsonl"
      cmds `shouldBe` (Just [])

  describe "can write commands" $ do
    it "can serialize add auction" $ do
      let encoded = BS.unpack $ encode addAuction in
        encoded `shouldBe` "test"
