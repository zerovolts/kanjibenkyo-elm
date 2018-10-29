module Main exposing (Model, init, main, update, view)

import Api exposing (getAllKana, getAllKanji)
import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html, div, h1, img, text)
import Http
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Msg exposing (Msg(..))
import Url exposing (Url)
import Word exposing (Word)


type alias Model =
    { kana : Dict Char Kana
    , kanji : Dict Char Kanji
    , words : Dict String Word
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { kana = Dict.empty -- Dict.singleton (.hiragana Kana.default) Kana.default
      , kanji = Dict.empty -- Dict.singleton (.character Kanji.default) Kanji.default
      , words = Dict.singleton (.word Word.default) Word.default
      }
    , Cmd.batch [ Api.getAllKanji, Api.getAllKana ]
    )


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

        NoOp ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "kanjibenkyō"
    , body =
        [ div []
            (List.map
                (\( char, kanji ) -> text <| String.fromChar kanji.character)
                (Dict.toList model.kanji)
            )
        , div []
            (List.map
                (\( char, kana ) -> text <| String.fromChar kana.hiragana)
                (Dict.toList model.kana)
            )
        ]
    }


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }
