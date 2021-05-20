-- ↑
-- ~~~~~~~~   Imports  ~~~~~~~~
-- ↓


module Main exposing (main)

import Browser exposing (Document)
import Element as UI
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Error exposing (Error, viewHttpError)
import Html exposing (Html)
import Html.Attributes exposing (draggable)
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import List exposing (head, take)
import Random exposing (generate)
import Random.List exposing (shuffle)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http
import Theme exposing (..)



-- ↑
-- ⇑
-- ########   MAIN   ########
-- ⇓
-- ↑
-- ~~~~~~~~   MODEL  ~~~~~~~~
-- ↓


type alias Model =
    { beingDragged : Maybe BreedName
    , catsGame : Game
    , error : Maybe Error
    }



-- ↑
-- ~~~~~~~~  UPDATE  ~~~~~~~~
-- ↓


type Msg
    = GotPhotos (WebData (List CatsData))
    | NewGame (List CatsData)
    | SubmitGame
    | Drag BreedName
    | DragEnd
    | DragOver
    | Drop Photo


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

                Failure error ->
                    ( { model | error = Just error }, Cmd.none )

        NewGame catsData ->
            ( { model
                | catsGame = take 4 catsData
              }
            , Cmd.none
            )

        SubmitGame ->
            ( model, Cmd.none )

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

        Drop newPosition ->
            case model.beingDragged of
                Nothing ->
                    ( model, Cmd.none )

                Just draggedItem ->
                    ( { model
                        | beingDragged = Nothing
                        , catsGame = updateDropped draggedItem newPosition model.catsGame
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
        [ viewHttpError model.error
        , viewCatsGame model.catsGame
        ]



-- ↑
-- ~~~~~~~~   INIT   ~~~~~~~~
-- ↓


initialModel : Model
initialModel =
    { beingDragged = Nothing
    , catsGame = []
    , error = Nothing
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


updateDropped : BreedName -> Photo -> Game -> Game
updateDropped draggedItem droppedOnPhoto catsGame =
    let
        maybeDraggedCat =
            List.filter (\cat -> cat.breedName == draggedItem) catsGame |> head

        maybeDroppedOnCat =
            List.filter (\cat -> cat.photo == droppedOnPhoto) catsGame |> head

        updateBreedName breedName photo cat =
            if cat.photo == photo then
                { cat | breedName = breedName }

            else
                cat
    in
    case ( maybeDraggedCat, maybeDroppedOnCat ) of
        ( Just draggedCat, Just droppedOnCat ) ->
            catsGame
                |> List.map (updateBreedName draggedItem droppedOnPhoto)
                |> List.map (updateBreedName droppedOnCat.breedName draggedCat.photo)

        ( _, _ ) ->
            catsGame



-- ↑
-- ~~~~~~~~   VIEW   ~~~~~~~~
-- ↓


viewDraggableItem : { a | breedName : BreedName } -> UI.Element Msg
viewDraggableItem item =
    UI.el
        [ UI.htmlAttribute (draggable "true")
        , onDragStart <| Drag item.breedName
        , onDragEnd DragEnd
        ]
        (viewBreed item.breedName)


viewDroppableArea : { a | photo : String } -> UI.Element Msg -> UI.Element Msg
viewDroppableArea { photo } elem =
    UI.el
        [ UI.height <| UI.px 80
        , UI.width <| UI.px 250
        , Background.color blue
        , onDragOver DragOver
        , onDrop <| Drop photo
        ]
        elem


onDragStart : msg -> UI.Attribute msg
onDragStart msg =
    Decode.succeed msg
        |> Events.on "dragstart"
        |> UI.htmlAttribute


onDragEnd : msg -> UI.Attribute msg
onDragEnd msg =
    Decode.succeed msg
        |> Events.on "dragend"
        |> UI.htmlAttribute


onDragOver : msg -> UI.Attribute msg
onDragOver msg =
    Decode.succeed ( msg, True )
        |> Events.preventDefaultOn "dragover"
        |> UI.htmlAttribute


onDrop : msg -> UI.Attribute msg
onDrop msg =
    Decode.succeed ( msg, True )
        |> Events.preventDefaultOn "drop"
        |> UI.htmlAttribute



-- ↑
-- ⇑
-- ######## CATS DATA ########
-- ⇓
-- ↑
-- ~~~~~~~~   MODEL  ~~~~~~~~
-- ↓


type alias CatsData =
    { photo : Photo
    , breedName : BreedName
    }


catsDecoder : Decode.Decoder CatsData
catsDecoder =
    Decode.succeed CatsData
        |> Decode.requiredAt [ "image", "url" ] Decode.string
        |> Decode.required "name" Decode.string



-- ↑
-- ↑
-- ~~~~~~~~   INIT   ~~~~~~~~
-- ↓


catsApiUrl : String
catsApiUrl =
    "https://api.thecatapi.com/v1/breeds?limit=10"


fetchCats : (WebData (List CatsData) -> msg) -> Cmd msg
fetchCats msg =
    Http.get
        catsApiUrl
        msg
        (Decode.list catsDecoder)



-- ↑
-- ⇑
-- ########   GAME   ########
-- ⇓
-- ↑
-- ~~~~~~~~   MODEL  ~~~~~~~~
-- ↓


type alias BreedName =
    String


type alias Photo =
    String


type alias Game =
    List
        { photo : Photo
        , breedName : BreedName
        }



-- ↑
-- ~~~~~~~~   VIEW   ~~~~~~~~
-- ↓


viewCatsGame : Game -> UI.Element Msg
viewCatsGame game =
    UI.column []
        [ UI.column []
            (List.map viewGameRow game)
        , button []
            { label = UI.text "Ready!"
            , onPress = Just SubmitGame
            }
        ]


viewGameRow cat =
    UI.row []
        [ viewPhoto cat.photo
        , cat |> viewDraggableItem |> viewDroppableArea cat
        ]


viewPhoto : Photo -> UI.Element msg
viewPhoto url =
    UI.image [ UI.height <| UI.px 200 ]
        { src = url
        , description = "cat's photograph"
        }


viewBreed : BreedName -> UI.Element msg
viewBreed breedName =
    UI.text breedName



-- ↑
-- ⇑
