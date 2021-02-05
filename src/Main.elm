module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import HttpUtils
import Json.Decode as Json
import Json.Decode.Pipeline as Json


apiKey : String
apiKey =
    ""



-- ↑
-- ########   MODEL  ########
-- ↓


type alias Model =
    { status : Status }


type Status
    = Loading
    | Loaded (List CatsData)
    | Errored String


type alias CatsData =
    { url : String
    , name : String
    }



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { status = Loading }


initialCmd : Cmd Msg
initialCmd =
    HttpUtils.get
        { url = "https://api.thecatapi.com/v1/breeds?limit=5"
        , headers = [ Http.header "api_key" apiKey ]
        , expect = Http.expectJson GotPhotos (Json.list catsDecoder)
        }


catsDecoder : Json.Decoder CatsData
catsDecoder =
    Json.succeed CatsData
        |> Json.requiredAt [ "image", "url" ] Json.string
        |> Json.required "name" Json.string



-- ↑
-- ########  UPDATE  ########
-- ↓


type Msg
    = ClickedReload
    | GotPhotos (Result Http.Error (List CatsData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedReload ->
            case model.status of
                Loaded (_ :: _) ->
                    ( model, Cmd.none )

                Loaded [] ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( { model | status = Errored ("Reload Error:" ++ errorMessage) }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                _ :: _ ->
                    ( { model | status = Loaded photos }
                    , Cmd.none
                    )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httpError) ->
            ( { model | status = Errored "Cats api error" }, Cmd.none )



-- ↑
-- ########   VIEW   ########
-- ↓


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos ->
                viewLoaded photos

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewLoaded : List CatsData -> List (Html Msg)
viewLoaded catsData =
    [ h1 [] [ text "Cats 😻" ]
    , button
        [ onClick ClickedReload ]
        [ text "New Game" ]
    , div [ id "thumbnails", class "med" ]
        (List.map viewCatsPhoto catsData)
    , div []
        (List.map viewCatsBreed catsData)
    ]


viewCatsPhoto : CatsData -> Html Msg
viewCatsPhoto catsData =
    img
        [ src catsData.url
        ]
        []


viewCatsBreed : CatsData -> Html Msg
viewCatsBreed catsData =
    text catsData.name



-- ↑
-- ########   MAIN   ########
-- ↓


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
