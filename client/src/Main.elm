module Main exposing (main)

import Browser
import Model exposing (Model)
import Msg exposing (Msg(..))
import Update exposing (update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = Model.init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = always NoOp
        }
