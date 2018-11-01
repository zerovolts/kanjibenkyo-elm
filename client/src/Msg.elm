module Msg exposing (Msg(..))

import Http
import Kana exposing (Category, Kana)
import Kanji exposing (Kanji)
import Url exposing (Url)


type Msg
    = NoOp
    | UrlChanged Url
    | AllKanaData (Result Http.Error (List Kana))
    | AllKanjiData (Result Http.Error (List Kanji))
    | ChangeKanaCategory Category
