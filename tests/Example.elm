module Example exposing (..)

import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Main
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "successfull decodes url" <|
        \_ ->
            """{
                "image":
                    {
                        "url":"cats.com"
                    },
                "name": "Ciri"
            }"""
                |> decodeString Main.catsDecoder
                |> Result.map .url
                |> Expect.equal
                    (Ok "cats.com")
