module Main exposing (catsDecoder, main)

import Browser exposing (Document)
import Element as UI
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http



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
    { catsData = NotAsked }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        "https://api.thecatapi.com/v1/breeds?limit=5"
        GotPhotos
        (Json.list catsDecoder)


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


view : Model -> Document Msg
view model =
    { title = "Cat's Breeds"
    , body = [ body ]
    }


body =
    UI.layout [] <|
        UI.column []
            [ header, page ]


header : UI.Element msg
header =
    UI.text "Cats 😻"


page : UI.Element msg
page =
    UI.el [] UI.none


body2 model =
    div []
        [ h1 [] [ text "Cats 😻" ]
        , div [ class "content" ] <|
            case model.catsData of
                Success photos ->
                    viewLoaded photos

                NotAsked ->
                    []

                Loading ->
                    []

                Failure errorMessage ->
                    [ text "Error: " ]
        ]



-- [ text ("Error: " + errorMessage) ]


viewLoaded : List CatsData -> List (Html Msg)
viewLoaded catsData =
    [ button
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
    Browser.document
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
