module Msg exposing (Msg(..))

import Http
import Kana exposing (Kana)
import Kanji exposing (Kanji)


type Msg
    = NoOp
    | AllKanaData (Result Http.Error (List Kana))
    | AllKanjiData (Result Http.Error (List Kanji))
