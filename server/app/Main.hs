{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Control.Monad.Trans.Reader
import GHC.Generics (Generic)
import GHC.TypeLits
import Database.Beam
import Database.Beam.Postgres
import Data.Proxy

import Tables.KanaTable
import Tables.KanjiTable

type API = SimpleAPI "kana" (KanaTable Identity) Int
    :<|> SimpleAPI "kanji" (KanjiTable Identity) Int

type SimpleAPI (name :: Symbol) a i = name :>
    ( Get '[JSON] [a]
    :<|> Capture "id" i :> Get '[JSON] a
    )

main :: IO ()
main = do
    conn <- connectPostgreSQL "host='localhost' port='5432' dbname='kanjibenkyo'"
    startServer conn 8000

startServer :: Connection -> Port -> IO ()
startServer conn port =
    run port $ simpleCors (serve (Proxy :: Proxy API) (server conn))


server :: Connection -> ServerT API Handler
server conn = hoistServer
    (Proxy :: Proxy API)
    (\r -> runReaderT r conn)
    handlers

handlers :: ServerT API (ReaderT Connection Handler)
handlers = kanaHandler :<|> kanjiHandler

simpleHandler
    :: (ReaderT Connection Handler [a])
    -> (i -> ReaderT Connection Handler a)
    -> ServerT (SimpleAPI name a i) (ReaderT Connection Handler)
simpleHandler listAs getA =
    listAs :<|> getA

kanaHandler :: ServerT (SimpleAPI "kana" (KanaTable Identity) Int) (ReaderT Connection Handler)
kanaHandler = simpleHandler
    getAllKana
    undefined

kanjiHandler :: ServerT (SimpleAPI "kanji" (KanjiTable Identity) Int) (ReaderT Connection Handler)
kanjiHandler = simpleHandler
    getAllKanji
    undefined
    -- (\i -> pure (kanjiList !! i))

getAllKana :: ReaderT Connection Handler [KanaTable Identity]
getAllKana = do
    conn <- ask
    liftIO $ runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ all_ (dbKanaTable myDatabase)

getAllKanji :: ReaderT Connection Handler [KanjiTable Identity]
getAllKanji = do
    conn <- ask
    liftIO $ runBeamPostgresDebug putStrLn conn $ runSelectReturningList $ select $ all_ (dbKanjiTable myDatabase)

data MyDatabase f = MyDatabase
    { dbKanaTable :: f (TableEntity KanaTable)
    , dbKanjiTable :: f (TableEntity KanjiTable)
    } deriving stock (Generic) 
      deriving anyclass (Database be)

myDatabase :: MyDatabase (DatabaseEntity be MyDatabase)
myDatabase = defaultDbSettings `withDbModification`
    dbModification
        { dbKanaTable = modifyTable (\_ -> "kana") tableModification
        , dbKanjiTable = modifyTable (\_ -> "kanji") tableModification
        }
