module Msg exposing (Msg(..))

import Http
import Kanji exposing (Kanji)


type Msg
    = NoOp
    | AllKanjiData (Result Http.Error (List Kanji))
