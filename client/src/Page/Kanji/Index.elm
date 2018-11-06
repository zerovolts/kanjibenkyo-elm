module Page.Kanji.Index exposing (Model, view)

import Color
import Dict exposing (Dict)
import Element
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
import Kanji exposing (Kanji, KanjiGrouping(..))
import Msg exposing (Msg(..))
import Page.Basic exposing (BlockType(..), charBlock, hr, radioButton)


type alias Model =
    { kanjiGrouping : KanjiGrouping
    , kanjiFilter : String
    }


view : Dict Char Kanji -> KanjiGrouping -> String -> Element Msg
view kanjiDict kanjiGrouping kanjiFilter =
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


viewKanjiGrouping : KanjiGrouping -> Element Msg
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
