module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, div, h1, img, text)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Word exposing (Word)


type alias Model =
    { kana : Dict Char Kana
    , kanji : Dict Char Kanji
    , words : Dict String Word
    }


init : ( Model, Cmd Msg )
init =
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


view : Model -> Html Msg
view model =
    div []
        (List.map
            (\( char, kanji ) -> text <| String.fromChar kanji.character)
            (Dict.toList model.kanji)
        )


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
