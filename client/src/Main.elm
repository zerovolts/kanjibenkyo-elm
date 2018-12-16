module Main exposing (main)

import Browser exposing (Document)
import Browser.Events exposing (onAnimationFrame)
import Dict
import Element exposing (el, text)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Page
import Page.Home as Home
import Page.Kana.Index as KanaIndex
import Page.Kana.Show as KanaShow
import Page.Kanji.Index as KanjiIndex
import Page.Kanji.Show as KanjiShow
import Page.Word.Index as WordIndex
import Page.Word.Inflector as WordInflector
import Route exposing (Route(..))
import Update exposing (update)
import Url


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = Model.init
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


view : Model -> Document Msg
view model =
    let
        content =
            case model.route of
                Home ->
                    Home.view model.clouds

                KanaIndex ->
                    KanaIndex.view model.kanaDict model.kanaFilter

                KanaShow kanaStr ->
                    let
                        ( kanaChar, _ ) =
                            Maybe.withDefault
                                (Tuple.pair ' ' "")
                                (String.uncons (Maybe.withDefault "" (Url.percentDecode kanaStr)))
                    in
                    KanaShow.view model.kanaDict kanaChar

                KanjiIndex ->
                    KanjiIndex.view model.kanjiDict model.kanjiGrouping model.kanjiView model.kanjiFilter

                KanjiShow kanjiStr ->
                    let
                        ( kanjiChar, _ ) =
                            Maybe.withDefault
                                (Tuple.pair ' ' "")
                                (String.uncons (Maybe.withDefault "" (Url.percentDecode kanjiStr)))
                    in
                    KanjiShow.view model.kanjiDict kanjiChar

                WordIndex ->
                    WordIndex.view Dict.empty

                WordInflector ->
                    WordInflector.view model.kanaDict model.currentWord

                NotFound ->
                    el [] (text "Not Found!")
    in
    Page.view content model.user model.url.path


subscriptions model =
    -- onAnimationFrame Tick
    Sub.none
