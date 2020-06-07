module Parser where
import           Data.Text

class Parser a where
    parse :: Text -> Maybe a
