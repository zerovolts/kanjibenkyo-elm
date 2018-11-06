module Page.Kanji.Card exposing (view)

import Color
import Element exposing (Color, Element, column, el, px, rgba255, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Kanji exposing (Kanji)
import Msg exposing (Msg)


view : Kanji -> Element Msg
view kanji =
    row
        [ Background.color Color.white
        , Element.padding (floor ((128 - 80) / 2))
        , Border.rounded 5
        , Element.width (px (256 + 128))
        , Element.height (px 128)
        , Element.spacing 16
        ]
        [ el
            [ Element.alignLeft
            , Font.size 80
            ]
            (text (String.fromChar kanji.character))
        , column
            [ -- [ Background.color Color.backgroundDark
              Element.alignTop
            , Element.spacing 4
            , Element.width Element.fill
            ]
            [ tagListView Color.blue "訓読み" kanji.kunyomi
            , tagListView Color.red "音読み" kanji.onyomi
            ]
        ]


tagListView : Color -> String -> List String -> Element Msg
tagListView color label strings =
    wrappedRow
        [ Element.spacing 4
        , Element.width Element.fill
        ]
        (tagView Color.backgroundDark Color.textLight label :: List.map (tagView color Color.white) strings)


tagView : Color -> Color -> String -> Element Msg
tagView background foreground str =
    let
        parts =
            String.split "-" str
    in
    el
        [ Background.color background
        , Font.color foreground
        , Font.size 14
        , Element.padding 4
        , Border.rounded 5
        ]
        (case parts of
            mainStr :: offStr :: _ ->
                row []
                    [ text mainStr, el [ Font.color (rgba255 255 255 255 0.5) ] (text offStr) ]

            _ ->
                text str
        )
