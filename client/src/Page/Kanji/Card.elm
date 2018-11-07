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
        , Border.rounded 5
        , Element.width (px (256 + 128 + 42))
        , Element.height (px 128)
        ]
        [ el
            [ Element.alignLeft
            , Element.padding 24
            , Font.size 80
            ]
            (text (String.fromChar kanji.character))
        , column
            [ Background.color Color.backgroundDark
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.alignTop
            , Element.spacing 4
            , Element.padding 16
            , Border.roundEach { topLeft = 0, topRight = 5, bottomLeft = 0, bottomRight = 5 }
            ]
            [ tagListView Color.blue "kun'yomi" kanji.kunyomi
            , tagListView Color.red "on'yomi" kanji.onyomi
            ]
        ]


tagListView : Color -> String -> List String -> Element Msg
tagListView color label strings =
    wrappedRow
        [ Element.spacing 4
        , Element.width Element.fill
        ]
        (tagView Color.background Color.textLight label :: List.map (tagView color Color.white) strings)


tagView : Color -> Color -> String -> Element Msg
tagView background foreground str =
    let
        parts =
            String.split "-" str

        isObsolete =
            Tuple.first (Maybe.withDefault ( ' ', "" ) (String.uncons str)) == '（'
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
                if isObsolete then
                    el [ Font.color (rgba255 255 255 255 0.5) ] (text (String.slice 1 -1 str))

                else
                    text str
        )
