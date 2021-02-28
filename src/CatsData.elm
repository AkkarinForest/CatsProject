module CatsData exposing (CatsData, fetchCats, viewCatsData)

import Element as UI
import Element.Background as Background
import Element.Input exposing (button)
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http


type alias CatsData =
    { url : String
    , name : String
    }


catsDecoder : Json.Decoder CatsData
catsDecoder =
    Json.succeed CatsData
        |> Json.requiredAt [ "image", "url" ] Json.string
        |> Json.required "name" Json.string


fetchCats : (RemoteData.WebData (List CatsData) -> msg) -> Cmd msg
fetchCats msg =
    Http.get
        "https://api.thecatapi.com/v1/breeds?limit=5"
        msg
        (Json.list catsDecoder)



-- ↑
-- ########  VIEW   ########
-- ↓


viewCatsData : msg -> List CatsData -> UI.Element msg
viewCatsData msg catsData =
    UI.column []
        [ button []
            { onPress = Just msg
            , label = UI.text "New Game"
            }
        , UI.row []
            [ UI.column []
                (List.map viewPhoto catsData)
            , UI.column []
                (List.map viewBreed catsData)
            ]
        ]


viewPhoto : CatsData -> UI.Element msg
viewPhoto photoData =
    UI.image [ UI.width <| UI.px 200 ]
        { src = photoData.url
        , description = "cat's photograph"
        }


viewBreed : CatsData -> UI.Element msg
viewBreed { name } =
    UI.text name
