module Api exposing
    ( decodeKanji
    , getAllKana
    , getAllKanji
    , getAllKanjiIfNeeded
    )

import Dict exposing (Dict)
import Http
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as DP
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
    D.succeed Kana
        |> DP.required "_kanaHiragana" char
        |> DP.required "_kanaKatakana" char
        |> DP.required "_kanaRomaji" D.string


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
    D.succeed Kanji
        |> DP.required "_kanjiCharacter" char
        |> DP.required "_kanjiStrokes" D.int
        |> DP.required "_kanjiOnyomi" (D.list D.string)
        |> DP.required "_kanjiKunyomi" (D.list D.string)
        |> DP.required "_kanjiMeanings" (D.list D.string)
        |> DP.required "_kanjiGrade" D.int
        |> DP.required "_kanjiRadical" char
        |> DP.required "_kanjiComponents" (D.list char)


char : Decoder Char
char =
    D.string |> D.andThen convertChar


convertChar : String -> Decoder Char
convertChar str =
    let
        convert =
            String.uncons >> Maybe.andThen (\tuple -> Just (Tuple.first tuple))
    in
    case convert str of
        Just c ->
            D.succeed c

        Nothing ->
            D.fail "Failed to decode char"
