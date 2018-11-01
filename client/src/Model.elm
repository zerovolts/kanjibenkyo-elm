module Model exposing (Model, init)

import Api exposing (getAllKana, getAllKanji)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Kana exposing (Category(..), Kana)
import Kanji exposing (Kanji)
import Msg exposing (Msg(..))
import Route exposing (Route)
import Url exposing (Url)
import Word exposing (Word)


type alias Model =
    { kana : Dict Char Kana
    , kanji : Dict Char Kanji
    , words : Dict String Word
    , url : Url
    , key : Key
    , route : Route
    , kanaFilter : Kana.Category
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { kana = Dict.empty -- Dict.singleton (.hiragana Kana.default) Kana.default
      , kanji = Dict.empty -- Dict.singleton (.character Kanji.default) Kanji.default
      , words = Dict.singleton (.word Word.default) Word.default
      , url = url
      , key = key
      , route = Route.toRoute url
      , kanaFilter = Hiragana
      }
    , Cmd.batch [ Api.getAllKanji, Api.getAllKana ]
    )
