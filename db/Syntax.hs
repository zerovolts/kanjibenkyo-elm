{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE DataKinds                  #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE KindSignatures     #-}
-- {-# LANGUAGE PolyKinds #-}

tokenizeJapanese :: String -> [String]
tokenizeJapanese = chunksOf 3

-- class Renderable a where
--   render :: a -> Text

data Sentence a
  = VerbPhraseSentence (VerbPhrase a)
  | AdjectivePhraseSentence (AdjectivePhrase a)
  deriving (Show)

-- createSentence :: m a -> Sentence a
-- createSentence (VerbPhrase vp) = VerbPhraseSentence (VerbPhrase vp)
-- createSentence (AdjectivePhrase ap) =
--   AdjectivePhraseSentence (AdjectivePhrase ap)
-- -- createSentence _ = Nothing

data Quote a
  = QuotedVerbPhrase (VerbPhrase a)
  | QuotedNounPhrase (NounPhrase a)
  | QuotedAdjectivePhrase (AdjectivePhrase a)
  | QuotedAdverbPhrase (AdverbPhrase a)
  deriving (Show)

data VerbPhrase a
  = VerbPhrase (Verb a)
  | AdverbVerbPhrase (AdverbPhrase a) (VerbPhrase a)
  | NounVerbPhrase (NounPhrase a) (Particle a) (VerbPhrase a)
  deriving (Show)

data NounPhrase a
  = NounPhrase (Noun a)
  | VerbNounPhrase (VerbPhrase a) (NounPhrase a)
  | AdjectiveNounPhrase (AdjectivePhrase a) (NounPhrase a)
  deriving (Show)

data AdjectivePhrase a
  = AdjectivePhrase (Adjective a)
  | AdverbAdjectivePhrase (AdverbPhrase a) (AdjectivePhrase a)
  | NounAdjectivePhrase (NounPhrase a) (Particle a) (AdjectivePhrase a)
  deriving (Show)

data AdverbPhrase a
  = AdverbPhrase (Adverb a)
  | AdverbAdverbPhrase (AdverbPhrase a) (AdverbPhrase a)
  deriving (Show)


-- instance Renderable (Verb a) where
--   render (Verb verb) = verb


-- renderWord :: m a -> a
-- renderWord (Noun a) = a
-- renderWord  () = a

-- newtype Verb a = Verb { unVerb :: a }
--   deriving (Show)

-- newtype Noun a = Noun { unNoun :: a}
--   deriving (Show)

-- newtype Adjective a = Adjective { unAdjective :: a}
--   deriving (Show)

-- newtype Adverb a = Adverb { unAdverb :: a }
--   deriving (Show)

-- newtype Particle a = Particle { unParticle :: a }
--   deriving (Show)

data JWord a = JWord2 WordType a
  deriving (Show)

data WordType = Noun' | Verb' | Adjective' | Adverb' | Particle'
  deriving (Show)

type Noun a = JWord2 Noun' a
type Verb a = JWord2 Verb' a
type Adjective a = JWord2 Adjective' a
type Adverb a = JWord2 Adverb' a
type Particle a = JWord2 Particle' a

type Parser a = Text -> (a, Text)

parse :: Parser (Sentence Text)
parse = undefined

inputText :: Text
inputText = "赤い車はとても高い"

sentence :: Sentence Text
sentence =
  (VerbPhraseSentence
    (NounVerbPhrase
      (AdjectiveNounPhrase (AdjectivePhrase (JWord Adjective "赤い"))
                           (NounPhrase (JWord Noun "車"))
      )
      (JWord Particle "は")
      (AdverbVerbPhrase (AdverbPhrase (JWord Adverb "とても"))
                        (VerbPhrase (JWord Verb "高い"))
      )
    )
  )
