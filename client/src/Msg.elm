module Msg exposing (Msg(..))

import Browser exposing (UrlRequest)
import Http
import Kana exposing (Category, Kana)
import Kanji exposing (Kanji, KanjiGrouping, KanjiView)
import Time exposing (Posix)
import Url exposing (Url)
import Word exposing (BaseWord, InflectedWord, Intent)


type Msg
    = NoOp
    | Tick Posix
    | LinkClicked UrlRequest
    | UrlChanged Url
    | AllKanaData (Result Http.Error (List Kana))
    | AllKanjiData (Result Http.Error (List Kanji))
    | ChangeKanaCategory Category
    | ChangeKanjiGrouping KanjiGrouping
    | ChangeKanjiFilter String
    | ChangeKanjiView KanjiView
    | ChangeCurrentWord BaseWord
    | InflectWord Intent
    | RemoveInflection
    | ClearInflections
    | Speak String
