module Page.Kanji.YomiTag exposing (view)

import Element as E exposing (Color, Element, el, rgba255, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Msg exposing (Msg)
import Page.Basic as Basic


view : Color -> Color -> String -> Element msg
view background foreground str =
    let
        parts =
            String.split "-" str

        isObsolete =
            Tuple.first (Maybe.withDefault ( ' ', "" ) (String.uncons str)) == '（'

        content =
            case parts of
                mainStr :: offStr :: _ ->
                    row []
                        [ text mainStr
                        , el [ Font.color (rgba255 255 255 255 0.5) ] (text offStr)
                        ]

                _ ->
                    if isObsolete then
                        el [ Font.color (rgba255 255 255 255 0.5) ] (text (String.slice 1 -1 str))

                    else
                        text str
    in
    Basic.tag background foreground content
