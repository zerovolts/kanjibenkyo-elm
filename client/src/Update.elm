module Update exposing (kanaToDict, kanjiToDict, update)

import Dict exposing (Dict)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Model exposing (Model)
import Msg exposing (Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AllKanaData (Ok kanaList) ->
            ( { model | kana = kanaToDict kanaList }, Cmd.none )

        AllKanaData (Err _) ->
            ( model, Cmd.none )

        AllKanjiData (Ok kanjiList) ->
            ( { model | kanji = kanjiToDict kanjiList }, Cmd.none )

        AllKanjiData (Err _) ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        ChangeKanaCategory category ->
            ( { model | kanaFilter = category }, Cmd.none )

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
