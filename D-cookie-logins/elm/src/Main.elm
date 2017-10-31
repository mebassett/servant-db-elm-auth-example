module Main exposing (..)

import CsrfCookie
import Html exposing (br, button, div, form, input, label, text, Html, table, tr, td, th, strong)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (send, expectStringResponse, Request, Body, request, header, toTask)
import Task exposing (..)
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
    , loginCreds : Login
    , login : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( model, sendWithCsrfToken PersonResponse getPerson )


model : Model
model =
    Model "" [] Nothing [] (Login "" "") False


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = PersonRequest
    | PersonResponse (Result Http.Error Person)
    | PersonsRequest
    | PersonsResponse (Result Http.Error (List Person))
    | LogEntriesRequest
    | LogEntriesResponse (Result Http.Error (List LogEntry))
    | LoginUsernameSet String
    | LoginPasswordSet String
    | LoginRequest Login
    | LoginResponse (Result Http.Error NoContent)
    | LogoutRequest
    | LogoutResponse (Result Http.Error NoContent)
    | NullMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PersonRequest ->
            ( model, sendWithCsrfToken PersonResponse getPerson )

        PersonResponse (Ok result) ->
            ( { model | currentPerson = Just result,
                        login = True,
                        logs = [] }
            , Http.send PersonsResponse getPersons )

        PersonResponse (Err err) ->
            ( { model | statusMsg = "Server did not like the request. Try logging in?" }
            , Http.send PersonsResponse getPersons
            )

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

        LogEntriesRequest ->
            ( model, sendWithCsrfToken LogEntriesResponse getLogs )

        LogEntriesResponse (Ok result) ->
            ( { model | logs = result }, Cmd.none )

        LogEntriesResponse (Err err) ->
            ( { model | statusMsg = "Server not responding." }, Cmd.none )

        LoginUsernameSet newName ->
            let
                newCred =
                    Login newName model.loginCreds.password
            in
                ( { model | loginCreds = newCred }, Cmd.none )

        LoginPasswordSet newPass ->
            let
                newCred =
                    Login model.loginCreds.username newPass
            in
                ( { model | loginCreds = newCred }, Cmd.none )

        LoginRequest login ->
            ( model, Http.send LoginResponse (postLogin login) )

        LoginResponse (Ok result) ->
            let
                _ =
                    Debug.log "Login OK" result
            in
                ( { model | login = True }, sendWithCsrfToken PersonResponse getPerson )

        LoginResponse (Err err) ->
            let
                _ =
                    Debug.log "Bad Login" err
            in
                ( model, Cmd.none )

        LogoutRequest ->
            ( model, Http.send LogoutResponse postLogout )

        LogoutResponse (Ok result) ->
            ({ model | login = False, currentPerson = Nothing }, Cmd.none)

        LogoutResponse (Err err) ->
            let _ =
                Debug.log "Bad logout" err
            in
                ( model, Cmd.none )

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

        loginBox =
            case model.currentPerson of
                Nothing ->
                    [ Html.form []
                        [ label [] [ text "Name" ]
                        , br [] []
                        , input [ type_ "text", placeholder "John", onInput LoginUsernameSet ] []
                        , label [] [ text "password" ]
                        , br [] []
                        , input [ type_ "password", placeholder "supersecure", onInput LoginPasswordSet ] []
                        , br [] []
                        , button [ type_ "button", onClick (LoginRequest model.loginCreds) ] [ text "Login" ]
                        ]
                    ]

                Just person ->
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

        personInfoButton =
            if model.login then
                [ button [ onClick LogoutRequest ] [ text "Logout" ] ]
            else
                loginBox
    in
        div []
            ([ strong [] [ text model.statusMsg ] ]
                ++ personsTable
                ++ personInfoButton
                ++ personInfo
                ++ logsTable
            )


personToRow : Person -> Html Msg
personToRow person =
    tr []
        [ td [] [ text person.personName ]
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
                , button [ onClick (LogEntriesRequest) ]
                    [ text "Get logs" ]
                ]
            ]


logToRow : LogEntry -> Html Msg
logToRow log =
    tr []
        [ td [] [ text (toIsoString log.logDayWritten) ]
        , td [] [ text log.logContent ]
        ]


sendWithCsrfToken :
    (Result Http.Error a -> msg)
    -> (String -> Http.Request a)
    -> Cmd msg
sendWithCsrfToken handler reqFunc =
    CsrfCookie.csrfCookie ()
        |> Task.map Just
        |> Task.onError (always (Task.succeed Nothing))
        |> Task.andThen
            (\mcsrf ->
                toTask
                    (reqFunc
                        (case mcsrf of
                            Nothing ->
                                ""

                            Just csrf ->
                                csrf
                        )
                    )
            )
        |> Task.attempt handler
