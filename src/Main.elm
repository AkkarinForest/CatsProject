module Main exposing (catsDecoder, main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import HttpUtils exposing (get)
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import RemoteData exposing (RemoteData(..), WebData)


apiKey : String
apiKey =
    "5e60a90e-2488-47d0-869e-f4cbbb6cbfc0"



-- ↑
-- ########   MODEL  ########
-- ↓


type alias Model =
    { catsData : WebData (List CatsData) }


type alias CatsData =
    { url : String
    , name : String
    }



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { catsData = Loading }


initialCmd : Cmd Msg
initialCmd =
    HttpUtils.get
        { url = "https://api.thecatapi.com/v1/breeds?limit=5"
        , headers = [ Http.header "api_key" apiKey ]
        , expect = Http.expectJson (RemoteData.fromResult >> GotPhotos) (Json.list catsDecoder)
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
    | GotPhotos (WebData (List CatsData))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedReload ->
            case model.catsData of
                NotAsked ->
                    ( model, Cmd.none )

                Success (_ :: _) ->
                    ( model, Cmd.none )

                Success [] ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Failure errorMessage ->
                    ( model, Cmd.none )

        -- ( { model | catsData = Failure ("Reload Error:" ++ errorMessage) }, Cmd.none )
        GotPhotos response ->
            ( { model | catsData = response }
            , Cmd.none
            )



-- case photos of
--     _ :: _ ->
--         ( { model | catsData = Loaded photos }
--         , Cmd.none
--         )
--
--     [] ->
--         ( { model | catsData = Failure "0 photos found" }, Cmd.none )
-- GotPhotos (Err httpError) ->
--     ( { model | catsData = Failure "Cats api error" }, Cmd.none )
-- ↑
-- ########   VIEW   ########
-- ↓


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.catsData of
            Success photos ->
                viewLoaded photos

            NotAsked ->
                []

            Loading ->
                []

            Failure errorMessage ->
                [ text "Error: " ]



-- [ text ("Error: " + errorMessage) ]


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
