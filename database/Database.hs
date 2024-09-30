{-# LANGUAGE OverloadedStrings #-}

module Database
  ( create,
    handlePush,
    handlePull,
    DBPull (GetAll, GetRand, GetNum),
    Quote,
  )
where

import Data.Aeson
import Data.Text qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import GHC.Generics (Constructor (conName))
import Network.Wai (Response)
import Server (makeResponse)
import System.Random (Random (randomR), RandomGen, getStdRandom, randomRIO)

data Quote = Quote T.Text T.Text T.Text

data DBPull = GetAll | GetRand | GetNum

instance ToJSON Quote where
  toJSON (Quote source person quote) =
    object ["source" .= source, "person" .= person, "quote" .= quote]
  toEncoding (Quote source person quote) =
    pairs ("source" .= source <> "person" .= person <> "quote" .= quote)

instance FromJSON Quote where
  parseJSON = withObject "Quote" $ \v ->
    Quote
      <$> v .: "source"
      <*> v .: "person"
      <*> v .: "quote"

instance FromRow Quote where
  fromRow = Quote <$> field <*> field <*> field

instance ToRow Quote where
  toRow (Quote source person quote) = toRow (source, person, quote)

instance ToRow Int where
  toRow = toRow

--- HANDLERS ---
handlePush :: Quote -> Response
handlePush = undefined

handlePull :: DBPull -> Connection -> Response
handlePull a conn =
  let getReq GetAll = getAll
      getReq GetNum = getNum
      getReq GetRand = getRand
   in do
        req <- getReq a conn
        makeResponse status200 $ encode req

----------------

-- creates the connection and creates a table "quotes" if necessary
create :: IO Connection
create = do
  conn <- open "quotes.db"
  -- SOURCE: who gave the quote | PERSON: who was quoted | QUOTE: the quote
  execute_ conn "CREATE TABLE IF NOT EXISTS quotes (str SOURCE, str PERSON, str QUOTE)"
  return conn

-- adds to the database
add :: Quote -> Connection -> IO ()
add quote conn = execute conn "INSERT INTO quotes (str, str, str) VALUES (?, ?, ?)" quote

-- gets all items from quotes table
getAll :: Connection -> IO [Quote]
getAll conn = query_ conn "SELECT * FROM quotes" :: IO [Quote]

-- gets the number of entries in quotes table
getNum :: Connection -> IO Int
getNum conn = do
  quotes <- getAll conn
  return $ length quotes

-- gets a random item from quotes
getRand :: Connection -> IO Quote
getRand conn = do
  num <- getNum conn >>= (\x -> randomRIO (1, x))
  req <- query conn "SELECT * FROM quotes WHERE rowid = ?" num :: IO [Quote]
  return $ head req
