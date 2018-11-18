module Page.Word.Index exposing (view)

import Dict exposing (Dict)
import Element as E exposing (Element, column, el, row, text, wrappedRow)
import Element.Font as Font
import Msg exposing (Msg(..))
import Page.Basic exposing (BlockType(..), button, charBlock, hr)
import Word exposing (BasicWord, InflectedWord, WordIntent(..))


view : Dict String BasicWord -> Element Msg
view words  =
    let
        wordList =
            Dict.toList words
    in
    column
        [ E.width E.fill
        , E.padding 64
        , E.spacing 24
        ]
        [ row
            [ E.width E.fill
            , E.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text ("Words - " ++ (String.fromInt <| List.length wordList)))
            , E.none
            ]
        , hr
        , wrappedRow
            [ E.spacing 12
            ]
            (List.map
                (\( root, _ ) -> charBlock WhiteBlack root)
                wordList
            )
        ]
