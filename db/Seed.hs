{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding
import qualified Data.Text.IO       as TIO
import           GHC.Generics
import           System.IO.Unsafe

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

data Kana = Kana
    { _hiragana :: Text
    , _katakana :: Text
    , _romaji   :: Text
    }
    deriving (Show, Generic, ToJSON, FromJSON)

data Kanji = Kanji
    { kanji   :: Text
    , radical :: Text
    , strokes :: Text
    , grade   :: Text
    , meaning :: Text
    , onyomi  :: [Text]
    , kunyomi :: [Text]
    }
    deriving (Show, Generic, ToJSON, FromJSON)

main :: IO ()
main = do
  TIO.putStrLn "Seeding Kana..."
  kanaData <- BS.readFile "kana.csv"
  let kanaList = readKana kanaData
  -- kanaList <- return (readKana kanaData)
  -- sequence_ $ map printKana kanaList
  sequence_ $ map (\k -> TIO.putStr $ _hiragana k) kanaList

  TIO.putStrLn ""
  TIO.putStrLn "Seeding Kanji..."
  kanjiData <- BS.readFile "kanji.json"
  kanjiList <- readKanji kanjiData
  -- sequence_ $ map printKanji kanjiList
  sequence_ $ map (\k -> TIO.putStr $ kanji k ) kanjiList

readKana :: ByteString -> [Kana]
readKana kanaData =
  map kanaConstructor
    . map (take 3)
    . drop 1
    . map (T.splitOn ",")
    $ T.lines
    $ decodeUtf8 kanaData
 where
  kanaConstructor (hira : kata : roma : _) = Kana hira kata roma
  kanaConstructor _                        = Kana "" "" ""

printKana :: Kana -> IO ()
printKana kana = do
  TIO.putStrLn
    $  [_hiragana, _katakana, _romaji]
    |> map ($ kana)
    |> T.intercalate " | "

readKanji :: ByteString -> IO [Kanji]
readKanji kanjiData =
  --undefined
  (eitherDecodeStrict kanjiData :: Either String [Kanji])
    |> either
         (\msg -> do
           putStrLn msg
           return [Kanji "見" "見" "7" "1" "see" ["ケン"] ["み"]]
         )
         return

printKanji :: Kanji -> IO ()
printKanji kan =
  TIO.putStrLn $ [kanji, radical, meaning] |> map ($ kan) |> T.intercalate " | "
