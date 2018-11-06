module Kanji exposing (Kanji, KanjiGrouping(..), KanjiView(..), default, isKanji)

import Char


type alias Kanji =
    { character : Char
    , strokes : Int
    , onyomi : List String
    , kunyomi : List String
    , meanings : List String
    , grade : Int
    , radical : Char
    , components : List Char
    }


type KanjiGrouping
    = NoGrouping
    | Grade
    | StrokeCount
    | Radical


type KanjiView
    = Node
    | Card



-- | Table


default : Kanji
default =
    { character = ' '
    , strokes = 7
    , onyomi = [ "ケン" ]
    , kunyomi = [ "み" ]
    , meanings = [ "see" ]
    , grade = 5
    , radical = '見'
    , components = []
    }


isKanji : Char -> Bool
isKanji char =
    let
        code =
            Char.toCode char
    in
    code >= 19968 && code <= 40943



-- getWords : Dict String Word -> Kanji -> List Word
