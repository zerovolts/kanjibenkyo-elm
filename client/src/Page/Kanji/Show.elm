module Page.Kanji.Show exposing (view)

import Color
import Dict exposing (Dict)
import Element as E exposing (Element, column, el, px, row, text, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Kanji exposing (Kanji)
import Msg exposing (Msg)
import Page.Basic as Basic
import Page.Kanji.YomiTag as YomiTag
import ViewHelpers exposing (defaultSpacing)


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
            [ column [ E.width (px 192), E.alignTop, E.spacing defaultSpacing ]
                [ el
                    [ Border.rounded 5
                    , Background.color Color.white
                    , E.width (px 128)
                    , E.height (px 128)
                    , E.pointer
                    , E.centerX
                    , E.alignTop
                    , Font.size 80
                    , Events.onClick (Msg.Speak <| String.fromChar kanjiChar)
                    ]
                    (el
                        [ E.centerX
                        , E.centerY
                        ]
                        (text (String.fromChar kanjiChar))
                    )
                , el [ E.centerX ]
                    (Basic.tagList
                        (List.map (text >> Basic.basicTag)
                            [ "Kanji"
                            , "Grade " ++ String.fromInt kanji.grade
                            , String.fromInt kanji.strokes ++ " strokes"
                            ]
                        )
                    )
                ]
            , column [ E.width E.fill, E.spacing defaultSpacing ]
                [ infoContainer "kun'yomi"
                    (Basic.tagList
                        (List.map (\k -> YomiTag.view Color.blue Color.white k) kanji.kunyomi)
                    )
                , infoContainer "on'yomi"
                    (Basic.tagList
                        (List.map (\k -> YomiTag.view Color.red Color.white k) kanji.onyomi)
                    )
                , infoContainer "meanings"
                    (text <| String.join ", " kanji.meanings)
                ]
            ]
        ]


infoContainer : String -> Element msg -> Element msg
infoContainer title content =
    let
        header : Element msg
        header =
            el
                [ E.paddingEach { top = 6, right = 8, bottom = 2, left = 8 }
                , Background.color Color.backgroundDark
                , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
                , Font.bold
                ]
                (text title)

        body : Element msg
        body =
            el
                [ Background.color Color.backgroundDark
                , E.padding 8
                , Border.roundEach { topLeft = 0, topRight = 5, bottomLeft = 5, bottomRight = 5 }
                , E.width (px 256)
                , Font.alignLeft
                ]
                content
    in
    column
        [ E.alignLeft, E.alignTop, E.width E.fill ]
        [ header, body ]
