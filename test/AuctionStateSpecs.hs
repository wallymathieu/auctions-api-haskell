module AuctionStateSpecs where
import qualified AuctionSite.Domain.States as S
import SampleData
import Data.Time
import Test.Hspec

incrementSpec baseState =
  describe "increment time" $ do
    it "can increment twice" $
      let s= S.inc sampleBidTime baseState
          s2= S.inc sampleBidTime s 
      in s `shouldBe` s2

    it "wont end just after start" $
      let state = S.inc (addUTCTime (toEnum 1) sampleStartsAt) baseState
      in S.hasEnded state `shouldBe` False

    it "wont end just before end" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleEndsAt) baseState
      in S.hasEnded state `shouldBe` False

    it "wont end just before start" $
      let state = S.inc (addUTCTime (toEnum (- 1)) sampleStartsAt) baseState
      in S.hasEnded state `shouldBe` False

    it "will have ended just after end" $
      let state = S.inc (addUTCTime (toEnum 1) sampleEndsAt) baseState
      in S.hasEnded state `shouldBe` True

