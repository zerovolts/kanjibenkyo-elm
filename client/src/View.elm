module View exposing (view)

import Browser exposing (Document)
import Color
import Dict exposing (Dict)
import Element
    exposing
        ( Element
        , centerX
        , centerY
        , column
        , el
        , link
        , padding
        , px
        , row
        , spacing
        , text
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Html exposing (node)
import Html.Attributes exposing (href, rel)
import Kana exposing (Category(..))
import Kanji exposing (Kanji, KanjiGrouping(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Route exposing (Route(..))
import Url


view : Model -> Document Msg
view model =
    let
        currentPage =
            column
                [ Element.height Element.fill
                , Element.width Element.fill
                ]
                [ Lazy.lazy navBar
                    [ titleText
                    , navLinks
                    ]
                , case model.route of
                    Home ->
                        homePage

                    KanaIndex ->
                        kanaGridPage model.kanaDict model.kanaFilter

                    KanaShow kanaStr ->
                        let
                            ( kanaChar, _ ) =
                                Maybe.withDefault
                                    (Tuple.pair 'a' "")
                                    (String.uncons (Maybe.withDefault "" (Url.percentDecode kanaStr)))
                        in
                        kanaShowPage model.kanaDict kanaChar

                    KanjiIndex ->
                        kanjiGridPage model.kanjiDict model.kanjiGrouping model.kanjiFilter

                    WordIndex ->
                        wordGridPage Dict.empty

                    NotFound ->
                        el [] (text "Not Found!")
                ]
    in
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
        , Element.layout
            globalStyles
            currentPage
        ]
    }


homePage =
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


kanaShowPage kanaDict kanaStr =
    let
        kana =
            Maybe.withDefault Kana.default (Dict.get kanaStr kanaDict)
    in
    column
        []
        [ el [] (text <| String.fromChar kana.hiragana) ]


kanjiGridPage : Dict Char Kanji -> KanjiGrouping -> String -> Element Msg
kanjiGridPage kanjiDict kanjiGrouping kanjiFilter =
    let
        kanjiList =
            Dict.toList kanjiDict

        kanjiFiltered =
            List.filter
                (\( char, kanji ) ->
                    isKanjiFiltered kanji kanjiFilter
                )
                kanjiList

        kanjiGroups : List (Group Char)
        kanjiGroups =
            groupKanji kanjiFiltered kanjiGrouping
    in
    column
        [ Element.width Element.fill
        , Element.padding 64
        , Element.spacing 24
        ]
        [ row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text
                    ("Kanji - "
                        ++ (String.fromInt <| List.length <| kanjiFiltered)
                        ++ " / "
                        ++ (String.fromInt <| List.length <| kanjiList)
                    )
                )
            , row [ Element.spacing 64 ]
                [ Input.search [ Element.width (px 256) ]
                    { text = kanjiFilter
                    , placeholder = Just (Input.placeholder [] (text "Search all kanji"))
                    , label = Input.labelHidden "Kanji Search"
                    , onChange = \text -> ChangeKanjiFilter text
                    }
                , viewKanjiGrouping kanjiGrouping
                ]
            ]
        , column [ Element.width Element.fill, Element.spacing 32 ]
            (List.map
                (\group ->
                    column [ Element.width Element.fill, Element.spacing 16 ]
                        [ hr
                        , el [ Element.centerX, Font.size 18 ] (text group.title)
                        , wrappedRow
                            [ Element.spacing 12
                            ]
                            (List.map
                                (\char -> charBlock WhiteBlack <| String.fromChar char)
                                group.data
                            )
                        ]
                )
                kanjiGroups
            )
        ]


isKanjiFiltered : Kanji -> String -> Bool
isKanjiFiltered kanji kanjiFilter =
    let
        listMatch =
            List.any (\field -> String.contains kanjiFilter field)

        meaningsMatch =
            listMatch kanji.meanings

        onyomiMatch =
            listMatch kanji.onyomi

        kunyomiMatch =
            listMatch kanji.kunyomi
    in
    meaningsMatch || onyomiMatch || kunyomiMatch


wordGridPage words =
    let
        wordList =
            Dict.toList words
    in
    column
        [ Element.width Element.fill
        , Element.padding 64
        , Element.spacing 24
        ]
        [ row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text ("Words - " ++ (String.fromInt <| List.length wordList)))
            , Element.none
            ]
        , hr
        , wrappedRow
            [ Element.spacing 12
            ]
            (List.map
                (\( char, _ ) -> charBlock WhiteBlack <| String.fromChar char)
                wordList
            )
        ]


kanaGridPage kana kanaFilter =
    column
        [ Element.width Element.shrink
        , Element.centerX
        , Element.padding 64
        , Element.spacing 24
        ]
        [ row
            [ Element.width Element.fill
            , Element.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text
                    ("Kana - "
                        ++ (String.fromInt <|
                                List.length <|
                                    Dict.toList kana
                           )
                    )
                )
            , kanaFilterGroup kanaFilter
            ]
        , hr
        , el [ Element.centerX ]
            (kanaGrid kana kanaFilter)
        ]


kanaGridTemplate : List (List (Maybe Char))
kanaGridTemplate =
    [ [ Just 'あ', Just 'い', Just 'う', Just 'え', Just 'お' ]
    , [ Just 'か', Just 'き', Just 'く', Just 'け', Just 'こ' ]
    , [ Just 'さ', Just 'し', Just 'す', Just 'せ', Just 'そ' ]
    , [ Just 'た', Just 'ち', Just 'つ', Just 'て', Just 'と' ]
    , [ Just 'な', Just 'に', Just 'ぬ', Just 'ね', Just 'の' ]
    , [ Just 'は', Just 'ひ', Just 'ふ', Just 'へ', Just 'ほ' ]
    , [ Just 'ま', Just 'み', Just 'む', Just 'め', Just 'も' ]
    , [ Just 'や', Nothing, Just 'ゆ', Nothing, Just 'よ' ]
    , [ Just 'ら', Just 'り', Just 'る', Just 'れ', Just 'ろ' ]
    , [ Just 'わ', Just 'ゐ', Nothing, Just 'ゑ', Just 'を' ]
    , [ Just 'ん', Nothing, Nothing, Nothing, Nothing ]
    ]


kanaGrid kanaDict kanaFilter =
    let
        hydrateKana =
            \char ->
                case char of
                    Just c ->
                        let
                            kana =
                                Maybe.withDefault Kana.default (Dict.get c kanaDict)
                        in
                        charBlock WhiteBlack
                            (case kanaFilter of
                                Hiragana ->
                                    String.fromChar kana.hiragana

                                Katakana ->
                                    String.fromChar kana.katakana

                                Romaji ->
                                    kana.romaji
                            )

                    Nothing ->
                        charBlock WhiteBlack ""

        grid =
            List.reverse <|
                List.map
                    (\col ->
                        List.map hydrateKana col
                    )
                    kanaGridTemplate
    in
    column [ spacing 12 ]
        [ row [ spacing 12 ]
            (List.map (charBlock Faded)
                [ "nn", "w", "r", "y", "m", "h", "n", "t", "s", "k", "-" ]
            )
        , row
            [ spacing 12
            , Element.onRight
                (column
                    [ Element.moveRight 12
                    , Element.spacing 12
                    ]
                    (List.map (charBlock Faded)
                        [ "a", "i", "u", "e", "o" ]
                    )
                )
            ]
            (List.map
                (\col ->
                    column
                        [ Element.spacing 12
                        ]
                        col
                )
                grid
            )
        ]


hr =
    el
        [ Element.width Element.fill
        , Element.height (px 3)
        , Background.color Color.backgroundDark
        , Border.rounded 5
        ]
        Element.none


button =
    Input.button
        [ Background.color Color.orange
        , Font.color Color.white
        , Border.rounded 5
        , Element.paddingXY 12 8
        ]


radioButton selected =
    let
        optionalAttributes =
            if selected then
                [ Background.color Color.orangeDark
                ]

            else
                [ Background.color Color.orange
                , Element.moveUp 4
                ]
    in
    Input.button
        ([ Font.color Color.white
         , Element.paddingXY 16 10
         , Border.rounded 5
         ]
            ++ optionalAttributes
        )


kanaFilterGroup kanaFilter =
    row
        [ Element.spacing 1
        , Background.color Color.orangeDark
        , Border.rounded 5
        ]
        [ radioButton (kanaFilter == Hiragana)
            { onPress = Just (ChangeKanaCategory Hiragana)
            , label = text "Hiragana"
            }
        , radioButton (kanaFilter == Katakana)
            { onPress = Just (ChangeKanaCategory Katakana)
            , label = text "Katakana"
            }
        , radioButton (kanaFilter == Romaji)
            { onPress = Just (ChangeKanaCategory Romaji)
            , label = text "Rōmaji"
            }
        ]


viewKanjiGrouping kanjiGrouping =
    row
        [ Element.spacing 1
        , Background.color Color.orangeDark
        , Border.rounded 5
        ]
        [ radioButton (kanjiGrouping == Grade)
            { onPress = Just (ChangeKanjiGrouping Grade)
            , label = text "Grade"
            }
        , radioButton (kanjiGrouping == StrokeCount)
            { onPress = Just (ChangeKanjiGrouping StrokeCount)
            , label = text "Stroke"
            }
        , radioButton (kanjiGrouping == Radical)
            { onPress = Just (ChangeKanjiGrouping Radical)
            , label = text "Radical"
            }
        ]


type alias Group a =
    { title : String
    , data : List a
    }


groupKanji : List ( Char, Kanji ) -> KanjiGrouping -> List (Group Char)
groupKanji kanjiTuples kanjiGrouping =
    let
        sortingField =
            case kanjiGrouping of
                Grade ->
                    .grade
                        >> (\x ->
                                if x == -1 then
                                    100

                                else
                                    x
                           )

                StrokeCount ->
                    .strokes

                Radical ->
                    .radical >> Char.toCode

        foldingFn =
            \( char, kanji ) memo ->
                case Dict.get (sortingField kanji) memo of
                    Just list ->
                        Dict.insert (sortingField kanji) (char :: list) memo

                    Nothing ->
                        Dict.insert (sortingField kanji) [ char ] memo
    in
    kanjiTuples
        |> List.foldl foldingFn Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( key, group ) ->
                { title =
                    case kanjiGrouping of
                        Grade ->
                            gradeTitle group key

                        StrokeCount ->
                            strokesTitle group key

                        Radical ->
                            radicalTitle group key
                , data = List.reverse group
                }
            )


gradeTitle : List a -> Int -> String
gradeTitle group grade =
    let
        groupLengthStr =
            String.fromInt (List.length group)
    in
    if grade == 100 then
        "Secondary School (" ++ groupLengthStr ++ ")"

    else
        "Grade " ++ String.fromInt grade ++ " (" ++ groupLengthStr ++ ")"


strokesTitle : List a -> Int -> String
strokesTitle group strokes =
    let
        strokesStr =
            String.fromInt strokes

        groupLengthStr =
            String.fromInt (List.length group)
    in
    if strokes == 1 then
        strokesStr ++ "stroke (" ++ groupLengthStr ++ ")"

    else
        strokesStr ++ " strokes (" ++ groupLengthStr ++ ")"


radicalTitle : List a -> Int -> String
radicalTitle group radical =
    let
        radicalStr =
            String.fromChar (Char.fromCode radical)

        groupLengthStr =
            String.fromInt (List.length group)
    in
    radicalStr ++ " (" ++ groupLengthStr ++ ")"


navBar =
    row
        [ Background.color Color.orange
        , Border.widthEach { bottom = 4, left = 0, right = 0, top = 0 }
        , Border.color Color.orangeDark
        , Element.height (px 52)
        , Element.width Element.fill
        , Element.paddingXY 64 0
        , Element.spaceEvenly
        ]


titleText =
    link
        [ Element.height Element.fill
        ]
        { url = "/"
        , label =
            row
                [ Font.size 26
                , Element.pointer
                ]
                [ el [] (text "漢字勉強")
                , el [ Font.color Color.white ] (text "kanjibenkyō")
                ]
        }


navLinks =
    row
        [ Font.size 20
        , Element.centerY
        , Font.color Color.white
        , Element.height Element.fill
        , Element.pointer
        ]
        [ navLink "Kana" "/kana", navLink "Kanji" "/kanji", navLink "Words" "/words" ]


navLink label url =
    link
        [ Element.mouseOver [ Background.color Color.orangeDark ]
        , Element.height Element.fill
        , Element.paddingXY 24 0
        ]
        { url = url
        , label = el [ Element.centerY ] (text label)
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


type BlockType
    = WhiteBlack
    | Faded


charBlock blockType char =
    let
        ( backgroundColor, fontColor ) =
            case blockType of
                WhiteBlack ->
                    ( Color.white, Color.text )

                Faded ->
                    ( Color.backgroundDark, Color.white )
    in
    Keyed.el []
        ( char
        , link
            [ Background.color backgroundColor
            , Font.color fontColor
            , Element.width (px 48)
            , Element.height (px 48)
            , Border.rounded 5
            , Font.size 24
            , Element.pointer
            ]
            { url = "/kana/" ++ char
            , label =
                el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (text char)
            }
        )
