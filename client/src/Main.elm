module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html, div, h1, img, text)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Url exposing (Url)
import Word exposing (Word)


type alias Model =
    { kana : Dict Char Kana
    , kanji : Dict Char Kanji
    , words : Dict String Word
    }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    ( { kana = Dict.singleton (.hiragana Kana.default) Kana.default
      , kanji = Dict.singleton (.character Kanji.default) Kanji.default
      , words = Dict.singleton (.word Word.default) Word.default
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "kanjibenkyo"
    , body =
        [ div []
            (List.map
                (\( char, kanji ) -> text <| String.fromChar kanji.character)
                (Dict.toList model.kanji)
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
