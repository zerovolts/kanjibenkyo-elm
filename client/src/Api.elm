module Api exposing
    ( decodeKanji
    , getAllKana
    , getAllKanji
    , getAllKanjiIfNeeded
    )

import Dict exposing (Dict)
import Http
import Json.Decode as D exposing (Decoder)
import Kana exposing (Kana)
import Kanji exposing (Kanji)
import Msg exposing (Msg(..))
import Route exposing (Route(..))


getAllKana : Cmd Msg
getAllKana =
    Http.send
        AllKanaData
        (Http.get "http://localhost:8000/kana" (D.list decodeKana))


decodeKana : Decoder Kana
decodeKana =
    D.map3 Kana
        (D.field "_kanaHiragana" D.string |> D.andThen strToChar)
        (D.field "_kanaKatakana" D.string |> D.andThen strToChar)
        (D.field "_kanaRomaji" D.string)


getAllKanjiIfNeeded : List a -> Cmd Msg
getAllKanjiIfNeeded kanji =
    if List.length kanji > 0 then
        Cmd.none

    else
        getAllKanji


getAllKanji : Cmd Msg
getAllKanji =
    Http.send
        AllKanjiData
        (Http.get "http://localhost:8000/kanji" (D.list decodeKanji))


decodeKanji : Decoder Kanji
decodeKanji =
    D.map8 Kanji
        (D.field "_kanjiCharacter" D.string |> D.andThen strToChar)
        (D.field "_kanjiStrokes" D.int)
        (D.field "_kanjiOnyomi" (D.list D.string))
        (D.field "_kanjiKunyomi" (D.list D.string))
        (D.field "_kanjiMeanings" (D.list D.string))
        (D.field "_kanjiGrade" D.int)
        (D.field "_kanjiRadical" D.string |> D.andThen strToChar)
        (D.field "_kanjiComponents" (D.list (D.string |> D.andThen strToChar)))


strToChar : String -> Decoder Char
strToChar str =
    str
        |> String.uncons
        |> Maybe.andThen (\tuple -> Just (Tuple.first tuple))
        |> Maybe.withDefault 'x'
        |> D.succeed
