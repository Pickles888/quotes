import Control.Monad.Catch (bracket_)
import Data.ByteString.Lazy.Char8 qualified as B
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status200, status404)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Handler.Warp (run)

port :: Int
port = 8080

main :: IO ()
main = run port app

app :: Application
app req respond =
  respond $ responseLBS status200 [] (B.pack (show req))
