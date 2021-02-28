module Main exposing (main)

import Browser exposing (Document)
import CatsData exposing (CatsData, fetchCats, viewCatsData)
import Element as UI
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import Http
import RemoteData exposing (RemoteData(..), WebData)



-- ↑
-- ########   MODEL  ########
-- ↓


type alias Model =
    { catsWebData : WebData (List CatsData) }



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { catsWebData = NotAsked }


initialCmd : Cmd Msg
initialCmd =
    fetchCats GotPhotos



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
            case model.catsWebData of
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

        GotPhotos response ->
            ( { model | catsWebData = response }
            , Cmd.none
            )



-- ↑
-- ########  VIEW   ########
-- ↓


view : Model -> Document Msg
view model =
    { title = "Cat's Breeds"
    , body = [ body model ]
    }


body : Model -> Html Msg
body model =
    UI.layout [] <| UI.column [] [ header, content model ]


blue =
    UI.rgb255 51 102 255


pink =
    UI.rgb255 255 102 255


header : UI.Element Msg
header =
    UI.el [ Background.color blue ] <| UI.text "Cats 😻"


content : Model -> UI.Element Msg
content model =
    viewCatsWebData model


viewCatsWebData : Model -> UI.Element Msg
viewCatsWebData model =
    UI.el [ Background.color pink ] <|
        case model.catsWebData of
            Success catsData ->
                viewCatsData ClickedReload catsData

            NotAsked ->
                UI.none

            Loading ->
                UI.none

            Failure errorMessage ->
                UI.text
                    (viewHttpError errorMessage)



-- viewHttpError : Http.Error -> String


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


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
