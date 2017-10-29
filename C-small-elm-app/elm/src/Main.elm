module Main exposing (..)

import Html exposing (br, button, div, form, input, label, text, Html, table, tr, td, th, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (send, expectStringResponse, Request, Body, request, header)
import Generated.RestApi exposing (..)
import Date.Extra exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, subscriptions = subscriptions, view = view, update = update }


type alias Model =
    { statusMsg : String
    , persons : List Person
    , currentPerson : Maybe Person
    , logs : List LogEntry
    }


init : ( Model, Cmd Msg )
init =
    ( model, Http.send PersonsResponse getPersons )


model : Model
model =
    Model "" [] Nothing []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PersonRequest String
    | PersonResponse (Result Http.Error Person)
    | PersonsRequest
    | PersonsResponse (Result Http.Error (List Person))
    | LogEntriesRequest String
    | LogEntriesResponse (Result Http.Error (List LogEntry))
    | NullMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonRequest name ->
            ( model, Http.send PersonResponse (getPersonByName name) )

        PersonResponse (Ok result) ->
            ( { model | currentPerson = Just result, logs = [] }, Cmd.none )

        PersonResponse (Err err) ->
            ( { model | statusMsg = "Server not responding." }, Cmd.none )

        PersonsRequest ->
            ( model, Http.send PersonsResponse getPersons )

        PersonsResponse (Ok result) ->
            ( { model | persons = result }, Cmd.none )

        PersonsResponse (Err err) ->
            let
                _ =
                    Debug.log "hello?" "anyone home"
            in
                ( { model | statusMsg = "Server not responding." }, Cmd.none )

        LogEntriesRequest author ->
            ( model, Http.send LogEntriesResponse (getLogsByAuthor author) )

        LogEntriesResponse (Ok result) ->
            ( { model | logs = result }, Cmd.none )

        LogEntriesResponse (Err err) ->
            ( { model | statusMsg = "Server not responding." }, Cmd.none )

        NullMsg ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        personsTable =
            if model.persons /= [] then
                [ table []
                    ([ tr [] [ th [] [ text "Name" ] ] ]
                        ++ List.map personToRow model.persons
                    )
                ]
            else
                []

        personInfo =
            personInfoBox model.currentPerson

        logsTable =
            if model.logs /= [] then
                [ table []
                    ([ tr []
                        [ th [ align ("left") ] [ text "Date" ]
                        , th [ align ("left") ] [ text "Log" ]
                        ]
                     ]
                        ++ List.map logToRow model.logs
                    )
                ]
            else
                []
    in
        div []
            ([ strong [] [ text model.statusMsg ] ]
                ++ personsTable
                ++ personInfo
                ++ logsTable
            )


personToRow : Person -> Html Msg
personToRow person =
    tr []
        [ td [] [ text person.personName ]
        , td [] [ button [ onClick (PersonRequest person.personName) ] [ text "Info" ] ]
        ]


personInfoBox : Maybe Person -> List (Html Msg)
personInfoBox mperson =
    case mperson of
        Nothing ->
            []

        Just person ->
            [ div []
                [ table []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , td [] [ text person.personName ]
                        ]
                    , tr []
                        [ th [] [ text "Email" ]
                        , td [] [ text person.personEmail ]
                        ]
                    , tr []
                        [ th [] [ text "DOB" ]
                        , td [] [ text (toIsoString person.personDob) ]
                        ]
                    ]
                , button [ onClick (LogEntriesRequest person.personName) ]
                    [ text "Get logs" ]
                ]
            ]


logToRow : LogEntry -> Html Msg
logToRow log =
    tr []
        [ td [] [ text (toIsoString log.logDayWritten) ]
        , td [] [ text log.logContent ]
        ]
