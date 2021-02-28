module Error exposing (viewHttpError)

import Http


viewHttpError : Http.Error -> String
viewHttpError error =
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
