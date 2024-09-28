module Server (serve, wrapDBRequest) where

import Data.ByteString.Lazy as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, status404)
import Network.Wai (Response, responseLBS)

type Path = [T.Text]

type Node = T.Text

data DBResponse = Ok Node | Error Node DBError

data DBError = InvalidURL | NotFound

wrapDBRequest :: DBResponse -> IO Response
wrapDBRequest = undefined

serve :: Path -> DBResponse
serve [path]
  | matchNode "get" path = getAllDB
  | matchNode "getRand" path = getRandDB
  | matchNode "getNum" path = getDB
  | otherwise = serveNotFound [path]
serve (x : xs)
  | matchNode "add" x = addDB (Prelude.last xs)
  | matchNode "remove" x = removeDB (Prelude.last xs)
  | otherwise = serveNotFound (x : xs)

matchPath :: [String] -> [T.Text] -> Bool
matchPath xs path = (T.pack <$> xs) == path

matchNode :: String -> Node -> Bool
matchNode s path = T.pack s == path

getDB :: DBResponse
getDB = undefined

getRandDB :: DBResponse
getRandDB = undefined

getAllDB :: DBResponse
getAllDB = undefined

addDB :: Node -> DBResponse
addDB = undefined

removeDB :: Node -> DBResponse
removeDB = undefined

serveNotFound :: Path -> DBResponse
serveNotFound path =
  let showPath = T.intercalate (T.pack "/") path
   in Error showPath InvalidURL

makeResponse :: Status -> T.Text -> Response
makeResponse status text =
  responseLBS status [] (B.fromStrict (encodeUtf8 text))
