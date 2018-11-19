module Page.Home exposing (view)

import Color
import Element as E exposing (Element, column, el, px, row, text)
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
                , E.height E.fill
                , E.width (px 76)
                ]
                E.none

        columnLeg =
            column
                [ E.height E.fill
                , E.width (px 76)
                ]
                [ el
                    [ E.height E.fill
                    , E.width E.fill
                    , Background.color Color.orange
                    ]
                    (column
                        [ E.centerX, E.centerY, Font.size 40, E.spacing 32 ]
                        (List.map (\char -> el [] (text char)) [ "漢", "字", "勉", "強" ])
                    )
                , el
                    [ E.height E.fill
                    , E.width E.fill
                    , Background.color Color.text
                    ]
                    E.none
                ]
    in
    column
        [ E.height E.fill
        , E.width E.fill
        , Background.gradient { angle = 0, steps = [ Color.background, E.rgb255 202 234 245 ] }

        -- , E.behindContent (row [] [ cloudView (unit 16) (unit 8), cloudView (unit 24) (unit 0), cloudView (unit 32) (unit 6) ])
        , E.behindContent (row [ E.spacing 192 ] (List.map (\cloud -> cloudView cloud.x cloud.y) clouds))
        ]
        [ row
            [ E.width E.fill
            , E.height (px 60)
            , E.paddingXY 64 0
            , E.spaceEvenly
            ]
            [ columnTopPiece, columnTopPiece, columnTopPiece ]
        , el
            [ Background.color Color.orange
            , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
            , Border.color Color.orangeDark
            , E.width E.fill
            , E.height (px 52)
            ]
            E.none
        , row
            [ E.height E.fill
            , E.width E.fill
            , E.paddingXY 64 0
            , E.spaceEvenly
            ]
            [ columnLeg, mainSection, columnLeg ]
        ]


mainSection : Element Msg
mainSection =
    E.none


cloudView : Float -> Float -> Element Msg
cloudView x y =
    column
        []
        [ el
            [ Background.color Color.white
            , E.width (px 128)
            , E.height (px 128)
            , E.moveRight x
            , E.moveDown y
            , Border.rounded 64
            , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 5, color = E.rgba255 0 0 0 0.05 }
            ]
            (el
                [ Background.color Color.white
                , E.width (px 256)
                , E.height (px 128)
                , E.moveLeft 64
                , E.moveDown 48
                , Border.rounded 64
                , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 5, color = E.rgba255 0 0 0 0.05 }
                ]
                (el
                    [ Background.color Color.white
                    , E.width (px 128)
                    , E.height (px 128)
                    , E.moveRight 64
                    , E.moveUp 48
                    , Border.rounded 64
                    ]
                    E.none
                )
            )
        ]
