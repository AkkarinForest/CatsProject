module Main exposing (main)

import Browser exposing (Document)
import CatsData exposing (..)
import Element as UI
import Element.Background as Background
import Error exposing (viewHttpError)
import Html exposing (Html)
import List exposing (take)
import Random exposing (generate)
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData(..), WebData)
import Theme exposing (..)



-- ↑
-- ########   MODEL  ########
-- ↓


type alias Model =
    { catsData : List CatsData
    , nothing : Int
    }



-- ↑
-- ########  UPDATE  ########
-- ↓


type Msg
    = GotPhotos (WebData (List CatsData))
    | NewGame (List CatsData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotPhotos response ->
            case response of
                NotAsked ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Success [] ->
                    ( model, Cmd.none )

                Success catsData ->
                    ( model
                    , generate NewGame <| shuffle catsData
                    )

                Failure _ ->
                    ( model, Cmd.none )

        NewGame catsData ->
            ( { model | catsData = take 5 catsData }
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
    UI.layout [ Background.color black ] <| UI.column [] [ header, content model ]


header : UI.Element Msg
header =
    UI.el [ Background.color blue ] <| UI.text "Cats 😻"


content : Model -> UI.Element Msg
content model =
    UI.el [ Background.color pink ] <| viewCatsData model.catsData



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { catsData = [], nothing = 1 }


initialCmd : Cmd Msg
initialCmd =
    fetchCats GotPhotos


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
