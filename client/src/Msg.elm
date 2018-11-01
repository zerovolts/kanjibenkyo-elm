module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Http
import Kana exposing (Category, Kana)
import Kanji exposing (Kanji)
import Url exposing (Url)


type Msg
    = NoOp
    | LinkClicked UrlRequest
    | UrlChanged Url
    | AllKanaData (Result Http.Error (List Kana))
    | AllKanjiData (Result Http.Error (List Kanji))
    | ChangeKanaCategory Category
