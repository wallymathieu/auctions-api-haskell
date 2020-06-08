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
import Parser
import qualified SampleData as S
import qualified Domain.Commands as C
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.TimedAscending as TA
import qualified Data.Text as T

addAuction = C.AddAuction S.sampleStartsAt S.sampleAuction
bid = C.PlaceBid S.sampleBidTime S.bid1
vac0= Amount VAC 0
timedAscending=TimedAscending (TA.Options vac0 vac0 (0::NominalDiffTime) )
spec:: ()->SpecWith ()
spec () = do
  describe "read json" $ do
    it "reading commands from jsonl" $ do
      cmds <- readCommands "./test/samples/sample-commands.jsonl"
      cmds `shouldNotBe` Nothing
    it "can deserialize type" $
      let
        json    = "\"English|VAC0|VAC0|0\""
        decoded = decode $ BS.pack json :: Maybe AuctionType 
      in
        decoded `shouldBe` Just timedAscending
    it "read add auction" $
      let
        json    = "{\"$type\":\"AddAuction\",\"at\":\"2020-05-17T08:15:16.464Z\",\"auction\":{\"id\":1,\"startsAt\":\"2018-12-01T10:00:00.000Z\",\"title\":\"Some auction\",\"expiry\":\"2020-05-18T10:00:00.000Z\",\"user\":\"BuyerOrSeller|a1|Test\",\"type\":\"English|VAC0|VAC0|0\",\"currency\":\"VAC\"}}"
        decoded = decode $ BS.pack json :: Maybe C.Command
      in
        decoded `shouldNotBe` Nothing
    it "read place bid" $
      let 
        json    = "{\"$type\":\"PlaceBid\",\"at\":\"2020-05-17T08:15:22.948Z\",\"bid\":{\"id\":\"32e692cc3fdb451da9647d6eeca5b2e3\",\"auction\":1,\"user\":\"BuyerOrSeller|a2|Buyer\",\"amount\":\"VAC11\",\"at\":\"2020-05-17T08:15:22.940Z\"}}"
        decoded = decode $ BS.pack json :: Maybe C.Command
      in
        decoded `shouldNotBe` Nothing

  describe "parse" $ do
    it "can parse amount" $
      let decoded = parse "VAC0" :: Maybe Amount
      in
        decoded `shouldBe` Just vac0
    it "can parse and write amount" $
      let decoded = parse $ T.pack $ show vac0 :: Maybe Amount
      in
        decoded `shouldBe` Just vac0
    it "can write auctiontype" $
      let decoded = parse $ T.pack $ show timedAscending :: Maybe AuctionType
      in
        decoded `shouldBe` Just timedAscending
    it "can read auctiontype" $
      let
        text = "English|VAC0|VAC0|0"
        decoded = parse $ T.pack text
      in
        decoded `shouldBe` Just timedAscending

  describe "write json" $ do
    it "can serialize add auction" $
      let
        encoded = BS.unpack $ encode addAuction
        expected = "{\"$type\":\"AddAuction\",\"at\":\"2016-01-01T08:28:00.607875Z\",\"auction\":{\"expiry\":\"2016-02-01T08:28:00.607875Z\",\"startsAt\":\"2016-01-01T08:28:00.607875Z\",\"user\":\"Sample_Seller\",\"currency\":\"SEK\",\"id\":1,\"title\":\"auction\",\"type\":\"Vickrey\"}}"
      in
        encoded `shouldBe` expected
    it "can serialize place bid" $
      let
        encoded = BS.unpack $ encode bid
        json = "{\"$type\":\"PlaceBid\",\"bid\":{\"amount\":\"SEK10\",\"at\":\"2016-01-01T08:28:00.607875000001Z\",\"auction\":1,\"user\":\"Buyer_1\",\"id\":\"baseless-leaf-olds-fade-sledsdebases\"},\"at\":\"2016-02-01T07:28:00.607875Z\"}"
      in
        encoded `shouldBe` json
