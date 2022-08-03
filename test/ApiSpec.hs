{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ApiSpec where
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Web.Spock (spockAsApp, spock)
import Web.Spock.Config
import GHC.Conc
import AuctionSite.Web.App

configuredApp = do
  state <- initAppState
  spockCfg <- defaultSpockCfg () PCNoDatabase state
  spock spockCfg app

firstAuctionJson = "{\"id\":1,\"startsAt\":\"2018-01-01T10:00:00.000Z\",\"endsAt\":\"2019-01-01T10:00:00.000Z\",\"title\":\"First auction\", \"currency\":\"VAC\" }"
spec :: Spec
spec =
    with (spockAsApp configuredApp) $
        do --describe "GET /" $
           -- do it "serves the home page" $ get "/" `shouldRespondWith` "\"Hello world\"" {matchStatus = 200}
           --
           describe "POST auction" $
               do it "possible to add auction" $
                      post "/auction" firstAuctionJson `shouldRespondWith` 200
                  it "possible to add bid to auction" $ do
                      post "/auction" firstAuctionJson `shouldRespondWith` 200
                      post "/auction/1/bid" "{\"amount\":10}" `shouldRespondWith` 200
                  it "not possible to add bid to non existant auction" $ do
                      post "/auction/2/bid" "{\"amount\":10}" `shouldRespondWith` "Auction not found" {matchStatus = 404}
                  it "returns auction" $ do
                      post "/auction" firstAuctionJson `shouldRespondWith` 200
                      get "/auction/1" `shouldRespondWith` auctionJson {matchStatus = 200}
                      get "/auctions" `shouldRespondWith` 200
                where auctionJson = [json|{currency: "VAC", expiry: "2019-01-01T10:00:00Z", id:1, startsAt: "2018-01-01T10:00:00Z", title: "First auction"}|]