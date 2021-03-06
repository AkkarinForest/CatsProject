module CatsData exposing (..)

import Element as UI
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import List exposing (head, map, map2, tail, take)
import RemoteData exposing (WebData)
import RemoteData.Http as Http
import Theme exposing (..)



-- ↑
-- ########  cats web data  ########
-- ↓


catsApiUrl : String
catsApiUrl =
    "https://api.thecatapi.com/v1/breeds?limit=10"


fetchCats : (WebData (List CatsData) -> msg) -> Cmd msg
fetchCats msg =
    Http.get
        catsApiUrl
        msg
        (Json.list catsDecoder)


catsDecoder : Json.Decoder CatsData
catsDecoder =
    Json.succeed CatsData
        |> Json.requiredAt [ "image", "url" ] Json.string
        |> Json.required "name" Json.string



-- ↑
-- ########  GAME   ########
-- ↓


type alias CatsData =
    { url : String
    , name : String
    }


type alias CatGameDatum =
    { url : String
    , breedName : String
    , breedPosition : Position
    }


type alias CatsGame =
    List CatGameDatum


type Position
    = One
    | Two
    | Three


positions : List Position
positions =
    [ One, Two, Three ]


startGame : List CatsData -> CatsGame
startGame catsData =
    let
        selectedCats : List CatsData
        selectedCats =
            take 3 catsData

        zipper : CatsData -> Position -> CatGameDatum
        zipper cat position =
            { url = cat.url, breedName = cat.name, breedPosition = position }
    in
    map2 zipper selectedCats positions



-- ↑
-- ########  dragging   ########
-- ↓


type alias DragId =
    Int


type alias DropId =
    Int


type alias DragDropModel =
    DragDrop.Model DragId DropId


type alias DragDropMsg =
    DragDrop.Msg DragId DropId



-- ↑
-- ########  GAME   ########
-- ↓


viewCatsData : (DragDropMsg -> msg) -> CatsGame -> UI.Element msg
viewCatsData msg catsGame =
    UI.row []
        [ UI.column []
            (List.map viewPhoto catsGame)
        , UI.column [ UI.spacing 200 ]
            (viewBreeds msg catsGame)
        ]


viewPhoto : CatGameDatum -> UI.Element msg
viewPhoto { url } =
    UI.image [ UI.height <| UI.px 200 ]
        { src = url
        , description = "cat's photograph"
        }


viewBreeds : (DragDropMsg -> msg) -> CatsGame -> List (UI.Element msg)
viewBreeds msg catsGame =
    List.map (viewBreedContainer msg) catsGame


viewBreedContainer : (DragDropMsg -> msg) -> CatGameDatum -> UI.Element msg
viewBreedContainer msg catGameDatum =
    UI.el []
        (viewBreed msg catGameDatum)


viewBreed : (DragDropMsg -> msg) -> CatGameDatum -> UI.Element msg
viewBreed msg { breedName, breedPosition } =
    UI.el
        (map UI.htmlAttribute (DragDrop.draggable msg 1))
        (UI.text breedName)


viewmock msg =
    UI.row [ Background.color black, UI.padding 10, UI.spacing 10, Border.color white, Border.width 2 ]
        [ UI.column [ Background.color blue, UI.padding 10 ]
            [ UI.el (map UI.htmlAttribute (DragDrop.draggable msg 2))
                (UI.text "dwa")
            , UI.el
                (map UI.htmlAttribute (DragDrop.draggable msg 1))
                (UI.text "jeden")
            ]
        , UI.column [ Background.color white ]
            [ UI.el
                (map UI.htmlAttribute (DragDrop.droppable msg 3))
                (UI.text "empty")
            ]
        ]
