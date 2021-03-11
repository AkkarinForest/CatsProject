module Main exposing (main)

import Browser exposing (Document)
import CatsData exposing (..)
import Element as UI
import Element.Background as Background
import Element.Font as Font
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
    UI.layout
        [ UI.width UI.fill, Background.color black, defaultFonts ]
        (UI.column
            [ UI.width UI.fill ]
            [ header, content model ]
        )


header : UI.Element Msg
header =
    UI.el
        [ UI.width UI.fill, UI.height (UI.px 100), Background.color blue ]
        (UI.el
            [ UI.centerX, UI.centerY, Font.size 30 ]
            (UI.text "Cats 😻")
        )


content : Model -> UI.Element Msg
content model =
    UI.column
        [ UI.centerX, Background.color pink, UI.width (UI.minimum 600 UI.shrink) ]
        [ viewCatsData model.catsData ]



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { catsData = []
    }


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
