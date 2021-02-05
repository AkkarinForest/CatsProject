module HttpUtils exposing (get)

import Http exposing (Expect, Header, emptyBody, request)


get : { url : String, headers : List Header, expect : Expect msg } -> Cmd msg
get r =
    request
        { method = "GET"
        , headers = r.headers
        , url = r.url
        , body = emptyBody
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }
