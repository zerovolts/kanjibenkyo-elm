module Kanji exposing (..)

import Char


type alias Kanji =
    { character : Char
    , onyomi : List String
    , kunyomi : List String
    , meanings : List String
    , jlpt : Int
    , radical : Char
    , components : List Char
    }


default : Kanji
default =
    { character = '見'
    , onyomi = [ "ケン" ]
    , kunyomi = [ "み" ]
    , meanings = [ "see" ]
    , jlpt = 5
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
