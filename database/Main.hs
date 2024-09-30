import Database
import Network.HTTP.Types (status404)
import Network.Wai (Application, Request (pathInfo))
import Network.Wai.Handler.Warp (run)
import Server

port :: Int
port = 8080

main :: IO ()
main = run port app

app :: Application
app request respond = respond $ handle (serve (pathInfo request))
  where
    handle (Push quote) = handlePush quote
    handle (Pull dbpull) = handlePull dbpull
    handle (UrlError err) = makeResponse status404 err
