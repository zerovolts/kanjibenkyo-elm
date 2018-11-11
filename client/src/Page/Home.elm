module Page.Home exposing (view)

import Color
import Element exposing (Element, column, el, px, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Model exposing (Cloud)
import Msg exposing (Msg)
import ViewHelpers exposing (unit)


view : List Cloud -> Element Msg
view clouds =
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
                    (column
                        [ Element.centerX, Element.centerY, Font.size 40, Element.spacing 32 ]
                        (List.map (\char -> el [] (text char)) [ "漢", "字", "勉", "強" ])
                    )
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
        , Background.gradient { angle = 0, steps = [ Color.background, Element.rgb255 202 234 245 ] }

        -- , Element.behindContent (row [] [ cloudView (unit 16) (unit 8), cloudView (unit 24) (unit 0), cloudView (unit 32) (unit 6) ])
        , Element.behindContent (row [ Element.spacing 192 ] (List.map (\cloud -> cloudView cloud.x cloud.y) clouds))
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
            [ columnLeg, mainSection, columnLeg ]
        ]


mainSection : Element Msg
mainSection =
    Element.none


cloudView : Float -> Float -> Element Msg
cloudView x y =
    column
        []
        [ el
            [ Background.color Color.white
            , Element.width (px 128)
            , Element.height (px 128)
            , Element.moveRight x
            , Element.moveDown y
            , Border.rounded 64
            , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 5, color = Element.rgba255 0 0 0 0.05 }
            ]
            (el
                [ Background.color Color.white
                , Element.width (px 256)
                , Element.height (px 128)
                , Element.moveLeft 64
                , Element.moveDown 48
                , Border.rounded 64
                , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 5, color = Element.rgba255 0 0 0 0.05 }
                ]
                (el
                    [ Background.color Color.white
                    , Element.width (px 128)
                    , Element.height (px 128)
                    , Element.moveRight 64
                    , Element.moveUp 48
                    , Border.rounded 64
                    ]
                    Element.none
                )
            )
        ]
