module Main exposing (main)

import Browser exposing (Document)
import CatsData exposing (..)
import Element as UI
import Element.Background as Background
import Element.Font as Font
import Error exposing (viewHttpError)
import Html exposing (Html)
import Html5.DragDrop as DragDrop
import List exposing (map, take)
import Random exposing (generate)
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData(..), WebData)
import Theme exposing (..)



-- ↑
-- ########   MODEL  ########
-- ↓
-- ↑
-- ########  UPDATE  ########
-- ↓


type Msg
    = GotPhotos (WebData (List CatsData))
    | NewGame (List CatsData)
    | DragDropMsg (DragDrop.Msg DragId DropId)


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

        DragDropMsg msg_ ->
            let
                ( model_, result ) =
                    DragDrop.update msg_ model.dragDrop
            in
            ( { model | dragDrop = model_ }, Cmd.none )



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
        [ viewCatsData model.catsData, viewmock ]


viewmock =
    UI.row [ Background.color black ]
        [ UI.column [ Background.color blue ]
            [ UI.el (map UI.htmlAttribute (DragDrop.draggable DragDropMsg 2))
                (UI.text "dwa")
            , UI.el
                (map UI.htmlAttribute (DragDrop.draggable DragDropMsg 1))
                (UI.text "jeden")
            ]
        , UI.column [ Background.color white ] []
        ]



-- ↑
-- ########   INIT   ########
-- ↓


type alias DragId =
    Int


type alias DropId =
    Int


type alias Model =
    { catsData : List CatsData
    , dragDrop : DragDrop.Model DragId DropId
    }


initialModel : Model
initialModel =
    { catsData = []
    , dragDrop = DragDrop.init
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
