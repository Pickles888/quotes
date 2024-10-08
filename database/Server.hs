{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module Server
  ( serve,
    makeResponse,
    ServeType (Push, Pull, UrlError),
  )
where

import Data.ByteString.Lazy.Char8 as B
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Database (DBPull (GetAll, GetNum, GetRand), Quote)
import Network.HTTP.Types (Status, status200, status404)
import Network.Wai (Application, Response, responseLBS)

data ServeType = Push Quote | Pull DBPull | UrlError T.Text

type Path = [T.Text]

serve :: Path -> ServeType
serve path@(x : xs)
  | matchNode "get" x = handleGet path
  | matchNode "add" x = Push $ Quote (xs !! 0) (xs !! 1) (xs !! 2)
  | otherwise = serveNotFound path
  where
    handleGet (y : ys)
      | matchNode "random" x = Pull GetRand
      | matchNode "num" x = Pull GetNum
      | matchNode "all" x = Pull GetAll
      | otherwise = serveNotFound path

matchNode :: String -> T.Text -> Bool
matchNode s path = T.pack s == path

serveNotFound :: Path -> ServeType
serveNotFound path =
  let showPath = T.intercalate (T.pack "/") path
   in UrlError showPath

makeResponse :: Status -> T.Text -> Response
makeResponse status text =
  responseLBS status [] (B.fromStrict (encodeUtf8 text))

--- HANDLERS ---
handlePush :: Quote -> Connection -> Response
handlePush q c = add q c >> makeResponse status200 $ (T.pack "Added: " ++ show q)

handlePull :: DBPull -> Connection -> Response
handlePull a conn = do
  req <- case a of
    GetAll -> getAll conn
    GetRand -> getRand conn
    GetNum -> getNum conn
  makeResponse status200 $ encode req



----------------

