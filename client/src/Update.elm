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
import Word exposing (pushIntent, removeIntent)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentWord =
            model.currentWord
    in
    case msg of
        Tick time ->
            ( { model
                | clouds =
                    List.map
                        (\cloud -> { cloud | x = cloud.x + cloud.deltaX })
                        model.clouds
              }
            , Cmd.none
            )

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

        ChangeKanjiView view ->
            ( { model | kanjiView = view }, Cmd.none )

        InflectWord intent ->
            ( { model | currentWord = pushIntent intent model.currentWord }, Cmd.none )

        RemoveInflection ->
            ( { model | currentWord = removeIntent model.currentWord }, Cmd.none )

        ClearInflections ->
            ( { model | currentWord = { currentWord | intents = [] } }, Cmd.none )

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
