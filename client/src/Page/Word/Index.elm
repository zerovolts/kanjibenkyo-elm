module Page.Word.Index exposing (view)

import Dict exposing (Dict)
import Element exposing (Element, column, el, row, text, wrappedRow)
import Element.Font as Font
import Msg exposing (Msg)
import Page.Basic exposing (BlockType(..), charBlock, hr)
import Word exposing (Word)


view : Dict String Word -> Element Msg
view words =
    let
        wordList =
            Dict.toList words
    in
    column
        [ Element.width Element.fill
        , Element.padding 64
        , Element.spacing 24
        ]
        [ row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text ("Words - " ++ (String.fromInt <| List.length wordList)))
            , Element.none
            ]
        , hr
        , wrappedRow
            [ Element.spacing 12
            ]
            (List.map
                (\( wordStr, _ ) -> charBlock WhiteBlack wordStr)
                wordList
            )
        ]
