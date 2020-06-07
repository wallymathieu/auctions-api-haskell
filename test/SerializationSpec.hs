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
      cmds `shouldBe` (Just [])
    it "can deserialize type" $
      let decoded = decode $ BS.pack "\"English|VAC0|VAC0|0\"" :: Maybe AuctionType in
        decoded `shouldBe` Just timedAscending
  describe "parse" $ do
    it "can parse amount" $
      let decoded = parseAmount "VAC0" in
        decoded `shouldBe` Just vac0
    it "can parse and write amount" $
      let decoded = parseAmount $ show vac0 in
        decoded `shouldBe` Just vac0
    it "can write auctiontype" $
      let decoded = parseAuctionType $ T.pack $ show timedAscending in
        decoded `shouldBe` Just timedAscending
    it "can read auctiontype" $
      let decoded = parseAuctionType $ T.pack $ "English|VAC0|VAC0|0" in
        decoded `shouldBe` Just timedAscending

  describe "write json" $ do
    it "can serialize add auction" $
      let encoded = BS.unpack $ encode addAuction in
        encoded `shouldBe` "{\"$type\":\"AddAuction\",\"at\":\"2016-01-01T08:28:00.607875Z\",\"auction\":{\"expiry\":\"2016-02-01T08:28:00.607875Z\",\"startsAt\":\"2016-01-01T08:28:00.607875Z\",\"user\":\"Sample_Seller\",\"currency\":\"SEK\",\"id\":1,\"title\":\"auction\",\"type\":\"Vickrey\"}}"
    it "can serialize place bid" $
      let encoded = BS.unpack $ encode bid in
        encoded `shouldBe` "{\"$type\":\"PlaceBid\",\"bid\":{\"amount\":\"SEK10\",\"at\":\"2016-01-01T08:28:00.607875000001Z\",\"auction\":1,\"user\":\"Buyer_1\",\"id\":\"baseless-leaf-olds-fade-sledsdebases\"},\"at\":\"2016-02-01T07:28:00.607875Z\"}"
