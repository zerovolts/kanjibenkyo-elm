module Model exposing (Cloud, Model, fetchRouteData, init)

import Api exposing (getAllKana, getAllKanjiIfNeeded)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Kana exposing (Category(..), Kana)
import Kanji exposing (Kanji, KanjiGrouping(..), KanjiView(..))
import Msg exposing (Msg(..))
import Page.Kanji.Index as KanjiIndex
import Route exposing (Route(..))
import Url exposing (Url)
import User exposing (User)
import Word exposing (BasicWord, InflectedWord)


type alias Model =
    { user : User
    , kanaDict : Dict Char Kana
    , kanjiDict : Dict Char Kanji
    , wordsDict : Dict String BasicWord
    , url : Url
    , key : Key
    , route : Route
    , kanaFilter : Category
    , kanjiFilter : String
    , kanjiGrouping : KanjiGrouping
    , kanjiView : KanjiView
    , clouds : List Cloud
    , currentWord : InflectedWord
    }


type alias Cloud =
    { x : Float
    , y : Float
    , deltaX : Float
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { user = User.default
            , kanaDict = Dict.empty
            , kanjiDict = Dict.empty
            , wordsDict = Dict.singleton (.root Word.default) Word.default
            , url = url
            , key = key
            , route = Route.toRoute url
            , kanaFilter = Hiragana
            , kanjiFilter = ""
            , kanjiGrouping = Grade
            , kanjiView = Node
            , clouds =
                [ { x = 0, y = 192, deltaX = 0.05 }
                , { x = 0, y = 4, deltaX = 0.15 }
                , { x = 0, y = 256, deltaX = -0.05 }
                , { x = 0, y = 48, deltaX = 0.2 }
                , { x = 0, y = 160, deltaX = -0.15 }
                ]
            , currentWord = Word.fromBasicWord Word.defaultGodan
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
