{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Servant.API
import           Servant.Server
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Control.Monad.Trans.Reader
import           GHC.Generics                   ( Generic )
import           GHC.TypeLits
import           Database.Beam
import           Database.Beam.Postgres
import           Data.Proxy
import           Data.Text                      ( pack )

import           Tables.KanaTable
import           Tables.KanjiTable

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

type API = SimpleAPI "kana" Kana String
    :<|> SimpleAPI "kanji" Kanji String

type SimpleAPI (name :: Symbol) a i = name :>
    ( Get '[JSON] [a]
    :<|> Capture "id" i :> Get '[JSON] a
    :<|> ReqBody '[JSON] a :> Post '[JSON] a
    )

main :: IO ()
main = do
  conn <- connectPostgreSQL "host='localhost' port='5432' dbname='kanjibenkyo'"
  putStrLn "Starting server on port 8000..."
  startServer conn 8000

startServer :: Connection -> Port -> IO ()
startServer conn port =
  run port $ simpleCors (serve (Proxy :: Proxy API) (server conn))

server :: Connection -> ServerT API Handler
server conn =
  hoistServer (Proxy :: Proxy API) (\r -> runReaderT r conn) handlers

handlers :: ServerT API (ReaderT Connection Handler)
handlers = kanaHandler :<|> kanjiHandler

simpleHandler
  :: (ReaderT Connection Handler [a])
  -> (i -> ReaderT Connection Handler a)
  -> (a -> ReaderT Connection Handler a)
  -> ServerT (SimpleAPI name a i) (ReaderT Connection Handler)
simpleHandler listAs getA postA = listAs :<|> getA :<|> postA

kanaHandler
  :: ServerT (SimpleAPI "kana" Kana String) (ReaderT Connection Handler)
kanaHandler = simpleHandler getAllKana findKana postKana

kanjiHandler
  :: ServerT (SimpleAPI "kanji" Kanji String) (ReaderT Connection Handler)
kanjiHandler = simpleHandler getAllKanji findKanji postKanji

getAllKana :: ReaderT Connection Handler [Kana]
getAllKana = do
  conn <- ask
  liftIO
    $ runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ all_ (dbKanaTable myDatabase)


findKana :: String -> ReaderT Connection Handler Kana
findKana hiragana = do
  conn <- ask
  liftIO
    $  runBeamPostgresDebug putStrLn conn
    $  fmap head
    $  runSelectReturningList
    $  select
    $  all_ (dbKanaTable myDatabase)
    |> filter_ (\kana -> _kanaHiragana kana ==. "ふ")
    |> limit_ 1

postKana :: Kana -> ReaderT Connection Handler Kana
postKana kana = do
  conn <- ask
  undefined
--   liftIO
--     $ runbeamPostgresDebug putStrLn conn do
--         runInsert $ insert (dbKanaTable myDatabase) $
--             insertValues [kana]


getAllKanji :: ReaderT Connection Handler [Kanji]
getAllKanji = do
  conn <- ask
  liftIO
    $ runBeamPostgresDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ all_ (dbKanjiTable myDatabase)


findKanji :: String -> ReaderT Connection Handler Kanji
findKanji kanjiChar = do
  conn <- ask
  liftIO
    $  runBeamPostgresDebug putStrLn conn
    $  fmap head
    $  runSelectReturningList
    $  select
    $  all_ (dbKanjiTable myDatabase)
    |> filter_ (\kanji -> _kanjiCharacter kanji ==. "見")
    |> limit_ 1

postKanji :: Kanji -> ReaderT Connection Handler Kanji
postKanji kanji = undefined

data MyDatabase f = MyDatabase
    { dbKanaTable :: f (TableEntity KanaTable)
    , dbKanjiTable :: f (TableEntity KanjiTable)
    } deriving stock (Generic)
      deriving anyclass (Database be)

myDatabase :: MyDatabase (DatabaseEntity be MyDatabase)
myDatabase = defaultDbSettings `withDbModification` dbModification
  { dbKanaTable  = modifyTable (\_ -> "kana") tableModification
  , dbKanjiTable = modifyTable (\_ -> "kanji") tableModification
  }
