import qualified EnglishAuctionStateSpec as English
import qualified VickreyAuctionStateSpec as Vickrey
import qualified BlindAuctionStateSpec as Blind
import Test.Hspec

main :: IO ()
main = hspec $ do 
  English.spec()
  Vickrey.spec()
  Blind.spec()
