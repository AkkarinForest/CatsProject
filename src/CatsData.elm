module CatsData exposing (CatsData, fetchCats, viewCatsData)

import Element as UI
import Element.Input exposing (button)
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import RemoteData exposing (WebData)
import RemoteData.Http as Http


type alias CatsData =
    { url : String
    , name : String
    }


catsApiUrl : String
catsApiUrl =
    "https://api.thecatapi.com/v1/breeds?limit=10"


fetchCats : (WebData (List CatsData) -> msg) -> Cmd msg
fetchCats msg =
    Http.get
        catsApiUrl
        msg
        (Json.list catsDecoder)


catsDecoder : Json.Decoder CatsData
catsDecoder =
    Json.succeed CatsData
        |> Json.requiredAt [ "image", "url" ] Json.string
        |> Json.required "name" Json.string



-- ↑
-- ########  VIEW   ########
-- ↓


viewCatsData : List CatsData -> UI.Element msg
viewCatsData catsData =
    UI.row []
        [ UI.column []
            (List.map viewPhoto catsData)
        , UI.column [ UI.spacing 200 ]
            (List.map viewBreed catsData)
        ]


viewPhoto : CatsData -> UI.Element msg
viewPhoto photoData =
    UI.image [ UI.height <| UI.px 200 ]
        { src = photoData.url
        , description = "cat's photograph"
        }


viewBreed : CatsData -> UI.Element msg
viewBreed { name } =
    UI.text name
