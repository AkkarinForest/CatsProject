module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ dndLeft.subscriptions model.draggableLeft
        , dndRigth.subscriptions model.draggableRight
        ]


type Msg
    = DropToRight Item
    | DropToLeft Item
    | DnDMsgLeftColumn (DnD.Msg () Item)
    | DnDMsgRightColumn (DnD.Msg () Item)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( update_ msg model, Cmd.none )


addToLeft model item =
    { model
        | left = model.left ++ [ item ]
        , right = List.filter (\i -> i.id /= item.id) model.right
    }


addToRight model item =
    { model
        | right = model.right ++ [ item ]
        , left = List.filter (\i -> i.id /= item.id) model.left
    }


update_ : Msg -> Model -> Model
update_ msg model =
    case msg of
        DropToLeft item ->
            addToLeft model item

        DropToRight item ->
            addToRight model item

        DnDMsgLeftColumn msg_ ->
            { model | draggableLeft = DnD.update msg_ model.draggableLeft }

        DnDMsgRightColumn msg_ ->
            { model | draggableRight = DnD.update msg_ model.draggableRight }


view : Model -> Html Msg
view model =
    div []
        [ dndLeft.droppable ()
            []
            (List.map
                (\item -> dndRigth.draggable item [] [ text item.text ])
                model.left
            )
        , dndRigth.droppable ()
            []
            (List.map
                (\item -> dndLeft.draggable item [] [ text item.text ])
                model.right
            )
        , DnD.dragged
            model.draggableLeft
            (\i -> text i.text)
        , DnD.dragged
            model.draggableRight
            (\i -> text i.text)
        ]


dndLeft =
    DnD.init DnDMsgLeftColumn (always DropToLeft)


dndRigth =
    DnD.init DnDMsgRightColumn (always DropToRight)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ { id = 1, text = "hello" }, { id = 2, text = "world" } ]
        [ { id = 3, text = "elm" }, { id = 4, text = "is" }, { id = 5, text = "cool" } ]
        dndLeft.model
        dndRigth.model
    , Cmd.none
    )


type alias Model =
    { left : List Item
    , right : List Item
    , draggableLeft : DnD.Draggable () Item
    , draggableRight : DnD.Draggable () Item
    }


type alias Item =
    { id : Id
    , text : String
    }


type alias Id =
    Int
