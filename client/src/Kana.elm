module Kana exposing (Category(..), Kana, VowelCategory(..), changeVowel, default, isHiragana, isKana, isKatakana, kanaGroups, vowelCategoryToIndex)

import Char


type alias Kana =
    { hiragana : Char
    , katakana : Char
    , romaji : String
    }


default : Kana
default =
    { hiragana = 'あ'
    , katakana = 'ア'
    , romaji = "a"
    }


type Category
    = Hiragana
    | Katakana
    | Romaji


type VowelCategory
    = A
    | I
    | U
    | E
    | O


type ConsonantCategory
    = NoConsonant
    | K
    | S
    | T
    | N
    | H
    | M
    | Y
    | R
    | W


vowelCategoryToIndex : VowelCategory -> Int
vowelCategoryToIndex cat =
    case cat of
        A ->
            0

        I ->
            1

        U ->
            2

        E ->
            3

        O ->
            4


isKana : Char -> Bool
isKana char =
    isHiragana char || isKatakana char


isHiragana : Char -> Bool
isHiragana char =
    let
        code =
            Char.toCode char
    in
    code >= 12352 && code <= 12447


isKatakana : Char -> Bool
isKatakana char =
    let
        code =
            Char.toCode char
    in
    code >= 12448 && code <= 12543


kanaGroups : List String
kanaGroups =
    [ "あいうえお"
    , "かきくけこ"
    , "さしすせそ"
    , "たちつてと"
    , "なにぬねの"
    , "はひふへほ"
    , "まみむめも"
    , "らりるれろ"
    , "がぎぐげご"
    , "ざじずぜぞ"
    , "だぢづでど"
    , "ばびぶべぼ"
    , "ぱぴぷぺぽ"
    ]


changeVowel : Char -> VowelCategory -> Maybe Char
changeVowel kana cat =
    let
        index =
            vowelCategoryToIndex cat
    in
    kanaGroups
        |> List.filter (String.any (\c -> c == kana))
        |> List.head
        |> Maybe.andThen (\x -> Just (String.slice index (index + 1) x))
        |> Maybe.andThen String.uncons
        |> Maybe.andThen (\x -> Just (Tuple.first x))
