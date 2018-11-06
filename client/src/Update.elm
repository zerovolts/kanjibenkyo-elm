module Update exposing (kanaToDict, kanjiToDict, update)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Model exposing (Model, fetchRouteData)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllKanaData (Ok kanaList) ->
            ( { model | kanaDict = kanaToDict kanaList }, Cmd.none )

        AllKanaData (Err _) ->
            ( model, Cmd.none )

        AllKanjiData (Ok kanjiList) ->
            ( { model
                -- | kanji = List.map (\kanji -> kanji.character) kanjiList
                | kanjiDict = kanjiToDict kanjiList
              }
            , Cmd.none
            )

        AllKanjiData (Err _) ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                route =
                    Route.toRoute url
            in
            ( { model | url = url, route = route }
            , fetchRouteData model route
            )

        ChangeKanaCategory category ->
            ( { model | kanaFilter = category }, Cmd.none )

        ChangeKanjiGrouping grouping ->
            ( { model | kanjiGrouping = grouping }, Cmd.none )

        ChangeKanjiFilter text ->
            ( { model | kanjiFilter = text }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


kanaToDict : List Kana -> Dict Char Kana
kanaToDict allKana =
    List.foldl
        (\cur acc -> Dict.insert cur.hiragana cur acc)
        Dict.empty
        allKana


kanjiToDict : List Kanji -> Dict Char Kanji
kanjiToDict allKanji =
    List.foldl
        (\cur acc -> Dict.insert cur.character cur acc)
        Dict.empty
        allKanji
