module Main exposing (..)

import Html exposing (Html, div, table, tbody, td, text, th, thead, tr, program)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)
import RemoteData exposing (WebData)


-- MODEL


type alias Task =
    { taskId : Int
    , description : String
    }


type alias Model =
    WebData (List Task)


init : ( Model, Cmd Msg )
init =
    ( RemoteData.Loading, fetchTasks )


fetchTasks : Cmd Msg
fetchTasks =
    Http.get fetchTasksUrl tasksDecoder
        |> RemoteData.sendRequest
        |> Cmd.map OnFetchTasks


fetchTasksUrl : String
fetchTasksUrl =
    "http://localhost:3000/api/tasks"


tasksDecoder : Decode.Decoder (List Task)
tasksDecoder =
    Decode.list taskDecoder


taskDecoder : Decode.Decoder Task
taskDecoder =
    decode Task
        |> required "taskId" Decode.int
        |> required "description" Decode.string



-- MESSAGES


type Msg
    = OnFetchTasks (WebData (List Task))



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        RemoteData.NotAsked ->
            text "Loading"

        RemoteData.Loading ->
            text "Loading..."

        RemoteData.Success tasks ->
            list tasks

        RemoteData.Failure err ->
            text (toString err)


list : List Task -> Html Msg
list tasks =
    table []
        [ thead []
            [ th [] [ text "Id" ]
            , th [] [ text "Description" ]
            ]
        , tbody [] (List.map taskRow tasks)
        ]


taskRow : Task -> Html Msg
taskRow task =
    tr []
        [ td [] [ text (toString task.taskId) ]
        , td [] [ text task.description ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnFetchTasks response ->
            ( response, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
