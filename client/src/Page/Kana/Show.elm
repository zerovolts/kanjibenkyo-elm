module Page.Kana.Show exposing (view)

import Color
import Dict exposing (Dict)
import Element as E exposing (Element, column, el, px, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Kana exposing (Kana)
import Msg exposing (Msg)
import Page.Basic as Basic exposing (kanaBlock)
import ViewHelpers exposing (defaultSpacing)


view : Dict Char Kana -> Char -> Element Msg
view kanaDict kanaChar =
    let
        kana =
            Maybe.withDefault Kana.default (Dict.get kanaChar kanaDict)
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
                    , Font.size 80
                    , Events.onClick (Msg.Speak <| String.fromChar kanaChar)
                    ]
                    (el
                        [ E.centerX
                        , E.centerY
                        ]
                        (text (String.fromChar kanaChar))
                    )
                , el [ E.centerX ]
                    (Basic.tagList (List.map (text >> Basic.basicTag) [ "Kana" ]))
                ]
            , row
                [ E.spacing 12
                , E.alignTop
                ]
                (List.map kanaBlock
                    [ kana.hiragana |> String.fromChar
                    , kana.katakana |> String.fromChar
                    , kana.romaji
                    ]
                )
            ]
        ]
