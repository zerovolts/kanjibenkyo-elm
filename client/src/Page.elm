module Page exposing (navBar, navLink, navLinks, titleText, view)

import Browser exposing (Document)
import Color
import Element as E
    exposing
        ( Element
        , column
        , el
        , link
        , px
        , row
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (node)
import Html.Attributes exposing (href, rel)
import Msg exposing (Msg)
import Url exposing (Url)
import User exposing (User)


view : Element Msg -> User -> String -> Document Msg
view content user currentPath =
    { title = "kanjibenkyō"
    , body =
        [ node "link"
            [ href "https://fonts.googleapis.com/css?family=Rounded+Mplus+1c"
            , rel "stylesheet"
            ]
            []
        , node "link"
            [ href "https://fonts.googleapis.com/css?family=Varela+Round"
            , rel "stylesheet"
            ]
            []
        , E.layout
            globalStyles
            (column
                [ E.height E.fill
                , E.width E.fill
                ]
                [ navBar
                    [ titleText
                    , row [ E.height E.fill, E.spacing 24 ] [ navLinks currentPath, userNav user ]
                    ]
                , el
                    [ E.height E.fill
                    , E.width E.fill
                    ]
                    content

                -- , el
                --     [ E.width E.fill
                --     , E.height (px 24)
                --     , Background.color Color.backgroundDark
                --     ]
                --     (el [ E.paddingXY 24 0, E.centerY ]
                --         (text "hello")
                --     )
                ]
            )
        ]
    }


globalStyles =
    [ Background.color Color.background
    , Font.family
        [ Font.typeface "Varela Round"
        , Font.typeface "Rounded Mplus 1c"
        , Font.sansSerif
        ]
    , Font.color Color.text
    , Font.size 16
    ]


navBar : List (Element Msg) -> Element Msg
navBar =
    row
        [ Background.color Color.orange
        , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
        , Border.color Color.orangeDark
        , E.height (px 52)
        , E.width E.fill
        , E.paddingXY 64 0
        , E.spaceEvenly
        ]


titleText : Element Msg
titleText =
    link
        [ E.height E.fill
        ]
        { url = "/"
        , label =
            row
                [ Font.size 26
                , E.pointer
                ]
                [ el [] (text "漢字勉強")
                , el [ Font.color Color.white ] (text "kanjibenkyō")
                ]
        }


navLinks : String -> Element Msg
navLinks currentPath =
    row
        [ Font.size 20
        , E.centerY
        , Font.color Color.white
        , E.height E.fill
        , E.pointer
        ]
        [ navLink "Kana" "/kana" currentPath, navLink "Kanji" "/kanji" currentPath, navLink "Words" "/words/inflector" currentPath ]


navLink : String -> String -> String -> Element Msg
navLink label url currentPath =
    link
        [ E.mouseOver [ Background.color Color.orangeDark ]
        , E.height E.fill
        , E.paddingXY 24 0
        , Background.color
            (if url == currentPath then
                Color.orangeDark

             else
                Color.orange
            )
        ]
        { url = url
        , label = el [ E.centerY ] (text label)
        }


userNav : User -> Element Msg
userNav user =
    el
        [ E.height E.fill
        , E.paddingXY 0 8
        ]
        (el
            [ Background.color Color.orangeDark
            , Border.rounded 5
            , E.paddingXY 12 0
            , E.width (px 160)
            , E.height E.fill
            , Font.color Color.white
            ]
            (el
                [ E.centerY
                ]
                (text user.name)
            )
        )
