{-# LANGUAGE OverloadedStrings #-}
module ApiSpec where
import           AuctionSite.Web.App
import           AuctionSite.Domain.Commands (Event)

import           Web.Spock (spockAsApp, spock)
import           Web.Spock.Config
import           Data.Aeson
import           Data.Vector ( singleton, fromList )
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import           Network.HTTP.Types (methodGet, methodPost, Header, HeaderName)
import           Data.Time (UTCTime(..))
import qualified Data.Map as Map

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON ( FromValue(fromValue) )
import           Network.Wai.Test (SResponse)

xJwtPayload :: HeaderName
xJwtPayload = "x-jwt-payload"

getWithHeader :: ByteString -> [Header] -> WaiSession st SResponse
getWithHeader path headers = request methodGet path headers ""
postWithHeader :: ByteString  -> [Header] -> LB.ByteString -> WaiSession st SResponse
postWithHeader = request methodPost
getCurrentTime:: IO UTCTime
getCurrentTime = pure $ read "2018-08-04 00:00:00.000000 UTC"

onEvent:: Event-> IO ()
onEvent _ = pure ()

configuredApp = do
  state <- initAppState Map.empty
  spockCfg <- defaultSpockCfg () PCNoDatabase state

  spock spockCfg (app onEvent getCurrentTime)

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
      object  ["amount" .= Number 11, "bidder" .= String "BuyerOrSeller|a2|Buyer"] ], "winner".=Null,"winnerPrice".=Null ]
    auctionWithoutBidJsonValue = object $ auctionJson ++ ["bids" .= array [], "winner".=Null,"winnerPrice".=Null ]
    auctionWithoutBidListJsonValue :: Value
    auctionWithoutBidListJsonValue = singletonArray $ object auctionJson
    auctionAddedJsonValue :: Value
    auctionAddedJsonValue = object [ "$type" .= String "AuctionAdded", "at" .= String "2018-08-04T00:00:00Z",
                                     "auction" .= object [
                                        "id" .= Number 1,
                                        "startsAt" .= String "2018-01-01T10:00:00Z",
                                        "title" .= String "First auction",
                                        "expiry" .= String "2019-01-01T10:00:00Z",
                                        "user" .= String "BuyerOrSeller|a1|Test",
                                        "type" .= String "English|0|0|0",
                                        "currency" .= String "VAC" ] ]
    bidAcceptedJsonValue :: Value
    bidAcceptedJsonValue = object [
        "$type" .= String "BidAccepted",
        "at" .= String "2018-08-04T00:00:00Z",
        "bid" .= object [
            "auction" .= Number 1,
            "user" .= String "BuyerOrSeller|a2|Buyer",
            "amount" .= Number 11,
            "at" .= String "2018-08-04T00:00:00Z" ] ]
    addAuctionOk = postWithHeader "/auctions" [(xJwtPayload, seller1)] firstAuctionReqJson `shouldRespondWith` fromValue auctionAddedJsonValue
    addBidOk = postWithHeader "/auctions/1/bids" [(xJwtPayload, buyer1)] "{\"amount\":11}" `shouldRespondWith` fromValue bidAcceptedJsonValue

    addAuctionSpec = describe "add auction" $ do
      it "possible to add auction" addAuctionOk
      it "not possible to same auction twice" $ do addAuctionOk; postWithHeader "/auctions" [(xJwtPayload, seller1)] firstAuctionReqJson `shouldRespondWith` "\"AuctionAlreadyExists 1\"" {matchStatus = 400}
      it "returns added auction" $ do addAuctionOk; get "/auctions/1" `shouldRespondWith` fromValue auctionWithoutBidJsonValue
      it "returns added auctions" $ do addAuctionOk; get "/auctions" `shouldRespondWith` fromValue auctionWithoutBidListJsonValue
    addBidSpec = describe "add bids to auction" $ do
      it "possible to add bid to auction" $ do addAuctionOk ; addBidOk
      it "possible to see the added bids" $ do addAuctionOk ; addBidOk ; get "/auctions/1" `shouldRespondWith` fromValue auctionWithBidJsonValue
      it "not possible to add bid to non existant auction" $ postWithHeader "/auctions/2/bids" [(xJwtPayload, buyer1)] "{\"amount\":10}" `shouldRespondWith` "Auction not found" {matchStatus = 404}
