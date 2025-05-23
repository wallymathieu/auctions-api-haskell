{-# LANGUAGE OverloadedStrings #-}
module SerializationSpec where
import           AuctionSite.Domain
import qualified AuctionSite.Domain.Commands as C
import qualified AuctionSite.Domain.TimedAscending as TA
import           AuctionSite.Persistence.JsonFile
import           AuctionSite.Money

import qualified SampleData as S

import           Data.Time
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Text.Read (readMaybe)
import           System.Directory


import           Test.Hspec

addAuction = C.AddAuction S.sampleStartsAt S.sampleAuction
bid = C.PlaceBid S.sampleBidTime S.bid1
vac0= Amount VAC 0
timedAscending=TimedAscending (TA.Options 0 0 (0::NominalDiffTime) )
spec:: ()->SpecWith ()
spec () = do
  describe "read json" $ do
    it "reading commands from jsonl" $ do
      cmds <- readCommands "./test/samples/sample-commands.jsonl"
      cmds `shouldNotBe` Nothing
    it "can deserialize type" $
      let
        json    = "\"English|0|0|0\""
        decoded = decode $ BS.pack json :: Maybe AuctionType
      in
        decoded `shouldBe` Just timedAscending
    it "read add auction" $
      let
        json    = "{\"$type\":\"AddAuction\",\"at\":\"2020-05-17T08:15:16.464Z\",\"auction\":{\"id\":1,\"startsAt\":\"2018-12-01T10:00:00.000Z\",\"title\":\"Some auction\",\"expiry\":\"2020-05-18T10:00:00.000Z\",\"user\":\"BuyerOrSeller|a1|Test\",\"type\":\"English|0|0|0\",\"currency\":\"VAC\"}}"
        decoded = decode $ BS.pack json :: Maybe C.Command
      in
        decoded `shouldNotBe` Nothing
    it "read place bid" $
      let
        json    = "{\"$type\":\"PlaceBid\",\"at\":\"2020-05-17T08:15:22.948Z\",\"bid\":{\"id\":\"32e692cc3fdb451da9647d6eeca5b2e3\",\"auction\":1,\"user\":\"BuyerOrSeller|a2|Buyer\",\"amount\":11,\"at\":\"2020-05-17T08:15:22.940Z\"}}"
        decoded = decode $ BS.pack json :: Maybe C.Command
      in
        decoded `shouldNotBe` Nothing

  describe "parse" $ do
    it "can parse amount" $
      let decoded = readMaybe "VAC0" :: Maybe Amount
      in
        decoded `shouldBe` Just vac0
    it "can parse and write amount" $
      let decoded = readMaybe $ show vac0 :: Maybe Amount
      in
        decoded `shouldBe` Just vac0
    it "can write auctiontype" $
      let decoded = readMaybe $ show timedAscending :: Maybe AuctionType
      in
        decoded `shouldBe` Just timedAscending
    it "can read auctiontype" $
      let
        text = "English|0|0|0"
        decoded = readMaybe text
      in
        decoded `shouldBe` Just timedAscending

  describe "write json" $ do
    it "can serialize add auction" $
      let
        encoded = toJSON addAuction
        expected = decode "{\"$type\":\"AddAuction\",\"at\":\"2016-01-01T08:28:00.607875Z\",\"auction\":{\"expiry\":\"2016-02-01T08:28:00.607875Z\",\"startsAt\":\"2016-01-01T08:28:00.607875Z\",\"user\":\"BuyerOrSeller|Sample_Seller|Seller\",\"currency\":\"SEK\",\"id\":1,\"title\":\"auction\",\"type\":\"Vickrey\"}}"
      in
        Just encoded `shouldBe` expected
    it "can serialize place bid" $
      let
        encoded = toJSON bid
        json = decode "{\"$type\":\"PlaceBid\",\"at\":\"2016-02-01T07:28:00.607875Z\",\"bid\":{\"amount\":10,\"at\":\"2016-01-01T08:28:00.607875000001Z\",\"auction\":1,\"user\":\"BuyerOrSeller|Buyer_1|Buyer 1\"}}"
      in
        Just encoded `shouldBe` json
  describe "read and write json" $ do
    it "reading commands from jsonl" $ do
      removeIfExists tmpSampleCommands
      cmds <- readCommands "./test/samples/sample-commands.jsonl"
      case cmds of
        Just cmds' -> do
          let (a,b) = splitAt 4 cmds'
          writeCommands tmpSampleCommands a
          writeCommands tmpSampleCommands b
          cmds2 <- readCommands tmpSampleCommands
          cmds2 `shouldBe` cmds
        Nothing -> error "no commands"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName =
    doesFileExist fileName >>= \exists ->
      if not exists
      then return ()
      else removeFile fileName

tmpSampleCommands = "./tmp/sample-commands.jsonl"
