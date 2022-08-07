{-# LANGUAGE OverloadedStrings #-}
module ApiSpec where
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON ( FromValue(fromValue) )
import           Web.Spock (spockAsApp, spock)
import           Web.Spock.Config
import           Data.Aeson
import           Data.Vector ( singleton, fromList )
import           AuctionSite.Web.App

configuredApp = do
  state <- initAppState
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  spock spockCfg app

spec :: Spec
spec =
    with (spockAsApp configuredApp) $ do addAuctionSpec; addBidSpec
  where
    singletonArray = Array . singleton 
    array = Array . fromList
    seller1 = "eyJzdWIiOiJhMSIsICJuYW1lIjoiVGVzdCIsICJ1X3R5cCI6IjAifQo="
    buyer1 = "eyJzdWIiOiJhMiIsICJuYW1lIjoiQnV5ZXIiLCAidV90eXAiOiIwIn0K"
    firstAuctionReqJson = "{\"id\":1,\"startsAt\":\"2018-01-01T10:00:00.000Z\",\"endsAt\":\"2019-01-01T10:00:00.000Z\",\"title\":\"First auction\", \"currency\":\"VAC\" }"
    auctionJson = [ "currency" .= String "VAC", "expiry" .= String "2019-01-01T10:00:00Z", "id".= Number 1, "startsAt".= String "2018-01-01T10:00:00Z", "title".= String "First auction"]
    auctionWithBidJsonValue = object $ auctionJson ++ ["bids" .= array [ 
      object  ["amount" .= String "VAC11", "bidder" .= String "BuyerOrSeller|a2|Buyer"] ], "winner".=String "","winnerPrice".=String ""  ] 
    auctionWithoutBidJsonValue = object auctionJson
    auctionWithoutBidListJsonValue :: Value
    auctionWithoutBidListJsonValue = singletonArray auctionWithoutBidJsonValue
    auctionAddedJsonValue :: Value
    auctionAddedJsonValue = object [ "$type" .= String "AuctionAdded", "at" .= String "2018-08-04T00:00:00.000Z",
                                     "auction" .= object [
                                        "id" .= Number 1,
                                        "startsAt" .= String "2018-01-01T10:00:00.000Z",
                                        "title" .= String "First auction",
                                        "expiry" .= String "2019-01-01T10:00:00.000Z",
                                        "user" .= String "BuyerOrSeller|a1|Test",
                                        "type" .= String "English|VAC0|VAC0|0",
                                        "currency" .= String "VAC" ] ]
    bidAcceptedJsonValue :: Value
    bidAcceptedJsonValue = object [
        "$type" .= String "BidAccepted",
        "at" .= String "2018-01-01T10:00:00.000Z",
        "bid" .= object [
            "auction" .= Number 1,
            "user" .= String "BuyerOrSeller|a2|Buyer",
            "amount" .= String "VAC11",
            "at" .= String "2018-01-01T10:00:00.000Z" ] ]

    addAuctionOk = post "/auction" firstAuctionReqJson `shouldRespondWith` fromValue auctionAddedJsonValue
    addBidOk = post "/auction/1/bid" "{\"amount\":10}" `shouldRespondWith` fromValue bidAcceptedJsonValue

    addAuctionSpec = describe "add auction" $ do 
      it "possible to add auction" addAuctionOk
      it "not possible to same auction twice" $ do addAuctionOk; post "/auction" firstAuctionReqJson `shouldRespondWith` "Auction already exists" {matchStatus = 400}
      it "returns added auction" $ do addAuctionOk; get "/auction/1" `shouldRespondWith` fromValue auctionWithoutBidJsonValue
      it "returns added auctions" $ do addAuctionOk; get "/auctions" `shouldRespondWith` fromValue auctionWithoutBidListJsonValue
    addBidSpec = describe "add bids to auction" $ do 
      it "possible to add bid to auction" $ do addAuctionOk ; addBidOk
      it "possible to see the added bids" $ do addAuctionOk ; addBidOk ; get "/auction/1" `shouldRespondWith` fromValue auctionWithBidJsonValue
      it "not possible to add bid to non existant auction" $ post "/auction/2/bid" "{\"amount\":10}" `shouldRespondWith` "Auction not found" {matchStatus = 404}

