module Main exposing (main)

import Browser exposing (Document)
import Element as UI
import Element.Background as Background
import Element.Font as Font
import Error exposing (viewHttpError)
import Html exposing (Html)
import Html.Attributes exposing (draggable)
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import List exposing (take)
import Random exposing (generate)
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import Theme exposing (..)



-- ⇑
-- ########   MAIN   ########
-- ⇓
-- ↑
-- ~~~~~~~~   MODEL  ~~~~~~~~
-- ↓


type alias Model =
    { catsData : List CatsData
    , beingDragged : Maybe String
    , draggableItems : List String
    , items : DropableSpots
    }


type alias DropableSpots =
    { first : String, second : String }



-- ↑
-- ~~~~~~~~  UPDATE  ~~~~~~~~
-- ↓


type Msg
    = GotPhotos (WebData (List CatsData))
    | NewGame (List CatsData)
    | Drag String
    | DragEnd
    | DragOver
    | Drop DroppedInMsg


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
            ( { model | catsData = take 3 catsData }
            , Cmd.none
            )

        Drag item ->
            ( { model | beingDragged = Just item }
            , Cmd.none
            )

        DragEnd ->
            ( { model | beingDragged = Nothing }
            , Cmd.none
            )

        DragOver ->
            ( model, Cmd.none )

        Drop droppedInMsg ->
            case model.beingDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just item ->
                    let
                        newItems =
                            updateDropped droppedInMsg model.items item
                    in
                    ( { model
                        | beingDragged = Nothing
                        , items = newItems
                      }
                    , Cmd.none
                    )



-- ↑
-- ~~~~~~~~  VIEW    ~~~~~~~~
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
-- ~~~~~~~~   INIT   ~~~~~~~~
-- ↓


initialModel : Model
initialModel =
    { catsData = []
    , beingDragged = Nothing
    , draggableItems =
        List.range 1 5
            |> List.map Debug.toString
    , items = { first = "7", second = "6" }
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



-- ↑
-- ⇑
-- ######## DRAGING ########
-- ⇓
-- ↑
-- ~~~~~~~~  UPDATE  ~~~~~~~~
-- ↓


type DroppedInMsg
    = First
    | Second


updateDropped : DroppedInMsg -> DropableSpots -> String -> DropableSpots
updateDropped msg spots item =
    case msg of
        First ->
            { spots | first = item }

        Second ->
            { spots | second = item }



-- ↑
-- ~~~~~~~~  VIEW   ~~~~~~~~
-- ↓


viewDraggableItem : UI.Element Msg -> UI.Element Msg
viewDraggableItem item =
    UI.el
        [ UI.htmlAttribute (draggable "true")
        , onDragStart <| Drag "item"
        , onDragEnd DragEnd
        ]
        item


onDragStart : msg -> UI.Attribute msg
onDragStart msg =
    Decode.succeed msg
        |> Events.on "dragstart"
        |> UI.htmlAttribute


onDragEnd msg =
    Decode.succeed msg
        |> Events.on "dragend"
        |> UI.htmlAttribute


onDragOver msg =
    Decode.succeed ( msg, True )
        |> Events.preventDefaultOn "dragover"
        |> UI.htmlAttribute


onDrop msg =
    Decode.succeed ( msg, True )
        |> Events.preventDefaultOn "drop"
        |> UI.htmlAttribute



-- ⇑
-- ######## CATS DATA ########
-- ⇓
-- ↑
-- ~~~~~~~~  INIT   ~~~~~~~~
-- ↓


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
        (Decode.list catsDecoder)


catsDecoder : Decode.Decoder CatsData
catsDecoder =
    Decode.succeed CatsData
        |> Decode.requiredAt [ "image", "url" ] Decode.string
        |> Decode.required "name" Decode.string



-- ↑
-- ~~~~~~~~  VIEW   ~~~~~~~~
-- ↓


viewCatsData : List CatsData -> UI.Element Msg
viewCatsData catsData =
    UI.row []
        [ UI.column []
            (List.map viewPhoto catsData)
        , UI.column [ UI.spacing 200 ]
            (catsData
                |> List.map viewBreed
                |> List.map viewDraggableItem
            )
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
