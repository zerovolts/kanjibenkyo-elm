module Model exposing (Model, fetchRouteData, init)

import Api exposing (getAllKana, getAllKanjiIfNeeded)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Kana exposing (Category(..), Kana)
import Kanji exposing (Kanji, KanjiGrouping(..))
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Url exposing (Url)
import Word exposing (Word)


type alias Model =
    { kanaDict : Dict Char Kana
    , kanjiDict : Dict Char Kanji
    , wordsDict : Dict String Word
    , kana : List Char
    , kanji : List Char
    , url : Url
    , key : Key
    , route : Route
    , kanaFilter : Category
    , kanjiGrouping : KanjiGrouping
    , kanjiFilter : String
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { kanaDict = Dict.empty
            , kanjiDict = Dict.empty
            , wordsDict = Dict.singleton (.word Word.default) Word.default
            , kana = []
            , kanji = []
            , url = url
            , key = key
            , route = Route.toRoute url
            , kanaFilter = Hiragana
            , kanjiGrouping = Grade
            , kanjiFilter = ""
            }
    in
    ( model
    , fetchRouteData model (Route.toRoute url)
    )


fetchRouteData : Model -> Route -> Cmd Msg
fetchRouteData model route =
    case route of
        KanjiIndex ->
            getAllKanjiIfNeeded model.kanji

        KanaIndex ->
            getAllKana

        _ ->
            Cmd.none
