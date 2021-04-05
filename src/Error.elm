module Error exposing (Error, viewHttpError)

import Element as UI
import Http
import Theme exposing (..)


type alias Error =
    Http.Error


viewHttpError maybeError =
    case maybeError of
        Nothing ->
            UI.text ""

        Just error ->
            UI.el [ UI.height <| UI.px 200, UI.width <| UI.px 800, smallFont ]
                (UI.text <| readHttpError error)


readHttpError : Http.Error -> String
readHttpError error =
    case error of
        Http.BadUrl str ->
            "Bad URL error:" ++ str

        Http.Timeout ->
            "Timeout error"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Bad staus error with code " ++ String.fromInt code

        Http.BadBody str ->
            "Bad body error: " ++ str
