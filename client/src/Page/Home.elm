module Page.Home exposing (view)

import Color
import Element exposing (Element, column, el, px, row)
import Element.Background as Background
import Element.Border as Border
import Msg exposing (Msg)


view : Element Msg
view =
    let
        columnTopPiece =
            el
                [ Background.color Color.orange
                , Element.height Element.fill
                , Element.width (px 76)
                ]
                Element.none

        columnLeg =
            column
                [ Element.height Element.fill
                , Element.width (px 76)
                ]
                [ el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Background.color Color.orange
                    ]
                    Element.none
                , el
                    [ Element.height Element.fill
                    , Element.width Element.fill
                    , Background.color Color.text
                    ]
                    Element.none
                ]
    in
    column
        [ Element.height Element.fill
        , Element.width Element.fill
        ]
        [ row
            [ Element.width Element.fill
            , Element.height (px 60)
            , Element.paddingXY 64 0
            , Element.spaceEvenly
            ]
            [ columnTopPiece, columnTopPiece, columnTopPiece ]
        , el
            [ Background.color Color.orange
            , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
            , Border.color Color.orangeDark
            , Element.width Element.fill
            , Element.height (px 52)
            ]
            Element.none
        , row
            [ Element.height Element.fill
            , Element.width Element.fill
            , Element.paddingXY 64 0
            , Element.spaceEvenly
            ]
            [ columnLeg, columnLeg ]
        ]
