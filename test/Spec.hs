import qualified EnglishAuctionSpec as English
import qualified VickreyAuctionSpec as Vickrey
import qualified BlindAuctionStateSpec as Blind
import qualified ApiSerializationSpec
import qualified ApiSpec
import qualified SerializationSpec as Serialization
import           Test.Hspec

main :: IO ()
main = hspec $ do
  English.spec()
  Vickrey.spec()
  Blind.spec()
  ApiSerializationSpec.spec()
  ApiSpec.spec
  Serialization.spec()
