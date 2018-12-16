module Page.Kanji.Show exposing (view)

import Color
import Dict exposing (Dict)
import Element as E exposing (Element, column, el, px, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Kanji exposing (Kanji)
import Msg exposing (Msg)


view : Dict Char Kanji -> Char -> Element Msg
view kanjiDict kanjiChar =
    let
        kanji =
            Maybe.withDefault Kanji.default (Dict.get kanjiChar kanjiDict)
    in
    column
        [ E.width E.fill
        , E.padding 64
        ]
        [ row
            [ E.centerX
            , E.spacing 64
            ]
            [ el
                [ Border.rounded 5
                , Background.color Color.white
                , E.width (px 128)
                , E.height (px 128)
                , E.pointer
                , Font.size 80
                , Events.onClick (Msg.Speak <| String.fromChar kanjiChar)
                ]
                (el
                    [ E.centerX
                    , E.centerY
                    ]
                    (text (String.fromChar kanjiChar))
                )
            ]
        ]
