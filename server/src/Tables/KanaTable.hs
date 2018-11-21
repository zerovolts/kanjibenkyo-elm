{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Tables.KanaTable (KanaTable) where

import Data.Aeson
import Data.Text (Text)
import Data.Vector
import Database.Beam
import GHC.Generics (Generic)

data KanaTable f = Kana
    { _kanaId :: Columnar f Int
    , _kanaHiragana :: Columnar f Text
    , _kanaKatakana :: Columnar f Text
    , _kanaRomaji :: Columnar f Text
    } deriving stock (Generic)
      deriving anyclass (Beamable)

instance ToJSON (KanaTable Identity)

instance Table KanaTable where
    data PrimaryKey KanaTable f = KanaId (Columnar f Int)
        deriving stock (Generic)
        deriving anyclass (Beamable)
    primaryKey :: KanaTable column -> PrimaryKey KanaTable column
    primaryKey x = KanaId (_kanaId x)