module AuctionStateSpecs where
import qualified Domain.States as S
import SampleData
import Data.Time
import Test.Hspec

incrementSpec baseState =
  describe "increment time" $ do
    it "wont end just after start" $
      let state = S.inc (addUTCTime (toEnum 1) sampleStartsAt) baseState in
      S.hasEnded state `shouldBe` False

    it "wont end just before end" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleEndsAt) baseState in
      S.hasEnded state `shouldBe` False

    it "wont end just before start" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleStartsAt) baseState in
      S.hasEnded state `shouldBe` False

    it "will have ended just after end" $
      let state = S.inc (addUTCTime (toEnum 1) sampleEndsAt) baseState in
      S.hasEnded state `shouldBe` True

