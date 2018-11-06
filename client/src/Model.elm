module Model exposing (Model, fetchRouteData, init)

import Api exposing (getAllKana, getAllKanjiIfNeeded)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Kana exposing (Category(..), Kana)
import Kanji exposing (Kanji, KanjiGrouping(..))
import Msg exposing (Msg(..))
import Page.Kanji.Index as KanjiIndex
import Route exposing (Route(..))
import Url exposing (Url)
import Word exposing (Word)


type alias Model =
    { kanaDict : Dict Char Kana
    , kanjiDict : Dict Char Kanji
    , wordsDict : Dict String Word
    , url : Url
    , key : Key
    , route : Route
    , kanjiFilter : String
    , kanaFilter : Category
    , kanjiGrouping : KanjiGrouping

    -- , page : PageData
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { kanaDict = Dict.empty
            , kanjiDict = Dict.empty
            , wordsDict = Dict.singleton (.word Word.default) Word.default
            , url = url
            , key = key
            , route = Route.toRoute url
            , kanjiFilter = ""
            , kanaFilter = Hiragana
            , kanjiGrouping = Grade

            -- , page = HomePage
            }
    in
    ( model
    , fetchRouteData model (Route.toRoute url)
    )


fetchRouteData : Model -> Route -> Cmd Msg
fetchRouteData model route =
    case route of
        KanjiIndex ->
            getAllKanjiIfNeeded (Dict.toList model.kanjiDict)

        KanaIndex ->
            getAllKana

        _ ->
            Cmd.none
