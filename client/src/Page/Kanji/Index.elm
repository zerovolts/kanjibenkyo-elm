module Page.Kanji.Index exposing (Model, view)

import Color
import Dict exposing (Dict)
import Element as E
    exposing
        ( Element
        , column
        , el
        , px
        , row
        , text
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy
import Kanji exposing (Kanji, KanjiGrouping(..), KanjiView(..))
import Msg exposing (Msg(..))
import Page.Basic exposing (BlockType(..), charBlock, hr, radioButton)
import Page.Kanji.Card as KanjiCard


type alias Model =
    { kanjiGrouping : KanjiGrouping
    , kanjiFilter : String
    }


view : Dict Char Kanji -> KanjiGrouping -> KanjiView -> String -> Element Msg
view kanjiDict kanjiGrouping kanjiView kanjiFilter =
    let
        kanjiList =
            Dict.toList kanjiDict

        kanjiFiltered =
            List.filter
                (\( char, kanji ) ->
                    isKanjiFiltered kanji kanjiFilter
                )
                kanjiList

        kanjiGroups : List (Group Kanji)
        kanjiGroups =
            groupKanji kanjiFiltered kanjiGrouping
    in
    column
        [ E.width E.fill
        , E.padding 64
        , E.spacing 24
        ]
        [ row
            [ E.width E.fill
            , E.spaceEvenly
            ]
            [ el [ Font.size 30 ]
                (text
                    ("Kanji - "
                        ++ (String.fromInt <| List.length <| kanjiFiltered)
                        ++ " / "
                        ++ (String.fromInt <| List.length <| kanjiList)
                    )
                )
            , row [ E.spacing 64 ]
                [ Input.search [ E.width (px 256), E.height (px 36), Border.width 0 ]
                    { text = kanjiFilter
                    , placeholder = Just (Input.placeholder [] (text "Search all kanji"))
                    , label = Input.labelHidden "Kanji Search"
                    , onChange = \text -> ChangeKanjiFilter text
                    }
                , viewKanjiView kanjiView
                , viewKanjiGrouping kanjiGrouping
                ]
            ]
        , case kanjiGrouping of
            NoGrouping ->
                column [ E.spacing 24, E.width E.fill ]
                    [ hr
                    , case kanjiView of
                        Node ->
                            kanjiNodeGridView (Tuple.second (List.unzip kanjiFiltered))

                        Card ->
                            kanjiCardGridView (Tuple.second (List.unzip kanjiFiltered))
                    ]

            _ ->
                column [ E.width E.fill, E.spacing 32 ]
                    (List.map
                        (\group ->
                            column [ E.width E.fill, E.spacing 16 ]
                                [ row [ E.width E.fill ]
                                    [ hr
                                    , el [ E.centerX, Font.size 18, E.paddingXY 24 0 ] (text group.title)
                                    , hr
                                    ]
                                , case kanjiView of
                                    Node ->
                                        kanjiNodeGridView group.data

                                    Card ->
                                        kanjiCardGridView group.data
                                ]
                        )
                        kanjiGroups
                    )
        ]


kanjiNodeGridView : List Kanji -> Element Msg
kanjiNodeGridView kanjiList =
    wrappedRow
        [ E.spacing 12
        ]
        (List.map
            (\kanji -> charBlock WhiteBlack <| String.fromChar kanji.character)
            kanjiList
        )


kanjiCardGridView : List Kanji -> Element Msg
kanjiCardGridView kanjiList =
    wrappedRow
        [ E.width E.fill
        , E.centerX
        , E.spacing 16
        , E.spaceEvenly
        ]
        (List.map
            (\kanji -> KanjiCard.view kanji)
            kanjiList
        )


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


viewKanjiView : KanjiView -> Element Msg
viewKanjiView kanjiView =
    row
        [ E.spacing 1
        , Background.color Color.orangeDark
        , Border.rounded 5
        ]
        [ radioButton (kanjiView == Node)
            { onPress = Just (ChangeKanjiView Node)
            , label = text "Node"
            }
        , radioButton (kanjiView == Card)
            { onPress = Just (ChangeKanjiView Card)
            , label = text "Card"
            }
        ]


viewKanjiGrouping : KanjiGrouping -> Element Msg
viewKanjiGrouping kanjiGrouping =
    row
        [ E.spacing 1
        , Background.color Color.orangeDark
        , Border.rounded 5
        ]
        [ radioButton (kanjiGrouping == NoGrouping)
            { onPress = Just (ChangeKanjiGrouping NoGrouping)
            , label = text "None"
            }
        , radioButton (kanjiGrouping == Grade)
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


groupKanji : List ( Char, Kanji ) -> KanjiGrouping -> List (Group Kanji)
groupKanji kanjiTuples kanjiGrouping =
    let
        sortingField =
            case kanjiGrouping of
                NoGrouping ->
                    always 0

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
                        Dict.insert (sortingField kanji) (kanji :: list) memo

                    Nothing ->
                        Dict.insert (sortingField kanji) [ kanji ] memo
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

                        _ ->
                            ""
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
