module Main exposing (catsDecoder, main)

import Browser exposing (Document)
import Element as UI
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import Json.Decode as Json
import Json.Decode.Pipeline as Json
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Http



-- ↑
-- ########   MODEL  ########
-- ↓


type alias Model =
    { catsWebData : WebData (List CatsData) }


type alias CatsData =
    { url : String
    , name : String
    }



-- ↑
-- ########   INIT   ########
-- ↓


initialModel : Model
initialModel =
    { catsWebData = NotAsked }


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

        -- ( { model | catsWebData = Failure ("Reload Error:" ++ errorMessage) }, Cmd.none )
        GotPhotos response ->
            ( { model | catsWebData = response }
            , Cmd.none
            )



-- case photos of
--     _ :: _ ->
--         ( { model | catsWebData = Loaded photos }
--         , Cmd.none
--         )
--
--     [] ->
--         ( { model | catsWebData = Failure "0 photos found" }, Cmd.none )
-- GotPhotos (Err httpError) ->
--     ( { model | catsWebData = Failure "Cats api error" }, Cmd.none )
-- ↑
-- ########   VIEW   ########
-- ↓


view : Model -> Document Msg
view model =
    { title = "Cat's Breeds"
    , body = [ body model ]
    }



-- body : UI.Element msg


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
                viewCatsData catsData

            NotAsked ->
                UI.none

            Loading ->
                UI.none

            Failure errorMessage ->
                UI.text "Error: "



-- [ text ("Error: " + errorMessage) ]
-- viewCats : List CatsData -> List (Html Msg)


viewCatsData : List CatsData -> UI.Element Msg
viewCatsData catsData =
    UI.column []
        [ button []
            { onPress = Just ClickedReload
            , label = UI.text "New Game"
            }
        , UI.row []
            [ UI.column []
                (List.map viewPhoto catsData)
            , UI.column []
                (List.map viewBreed catsData)
            ]
        ]


viewPhoto : CatsData -> UI.Element msg
viewPhoto photoData =
    UI.image [ UI.width <| UI.px 200 ]
        { src = photoData.url
        , description = "cat's photograph"
        }


viewBreed : CatsData -> UI.Element msg
viewBreed { name } =
    UI.text name



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
