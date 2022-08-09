module AuctionSite.Aeson where
import Text.Read (readMaybe)
import qualified Data.Aeson.Types as ATyp
import qualified Data.Text as T
import Data.Aeson
import Text.Printf (printf)

toJsonOfShow :: Show a => a -> Value
toJsonOfShow a= String . T.pack $ show a
ofJsonReadFailed :: String -> Value -> ATyp.Parser a
ofJsonReadFailed fname invalid = ATyp.prependFailure (printf "parsing %s failed, " fname) (ATyp.typeMismatch "String" invalid)
ofJsonOfRead :: Read a => String -> Value -> ATyp.Parser a
ofJsonOfRead fname (String v) = case readMaybe (T.unpack v) of
                         Just r-> pure r
                         Nothing -> ofJsonReadFailed fname (String v)
ofJsonOfRead fname invalid    = ofJsonReadFailed fname invalid
