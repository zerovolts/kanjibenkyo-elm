module Page.Kanji.Card exposing (view)

import Color
import Element as E exposing (Color, Element, column, el, px, rgba255, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Kanji exposing (Kanji)
import Msg exposing (Msg)
import Page.Kanji.YomiTag as YomiTag


view : Kanji -> Element Msg
view kanji =
    row
        [ Background.color Color.white
        , Border.rounded 5
        , E.width (px (256 + 128 + 42))
        , E.height (px 128)
        ]
        [ el
            [ E.alignLeft
            , E.padding 24
            , Font.size 80
            ]
            (text (String.fromChar kanji.character))
        , column
            [ Background.color Color.backgroundDark
            , E.width E.fill
            , E.height E.fill
            , E.alignTop
            , E.spacing 4
            , E.padding 16
            , Border.roundEach { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 }
            ]
            [ tagListView Color.blue "kun'yomi" kanji.kunyomi
            , tagListView Color.red "on'yomi" kanji.onyomi
            ]
        ]


tagListView : Color -> String -> List String -> Element Msg
tagListView color label strings =
    wrappedRow
        [ E.spacing 4
        , E.width E.fill
        ]
        (YomiTag.view Color.background Color.textLight label
            :: List.map (YomiTag.view color Color.white) strings
        )
