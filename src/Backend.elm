module Backend exposing (..)

import Dict exposing (Dict)
import Env
import Html
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { userCredentials = Dict.empty
      , userSessions = Dict.empty
      , adminSession = Nothing
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GetLoginInfo ->
            case Dict.get sessionId model.userSessions of
                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId YouAreNotLoggedIn )

                Just user ->
                    ( model
                    , Lamdera.sendToFrontend clientId (YouAreLoggedIn user)
                    )

        SignupRequest { username, password } ->
            if Dict.member username model.userCredentials then
                ( model
                , Lamdera.sendToFrontend
                    clientId
                    (UsernameAlreadyExists { username = username })
                )

            else
                ( { model
                    | userCredentials =
                        Dict.insert username password model.userCredentials
                    , userSessions =
                        Dict.insert sessionId username model.userSessions
                  }
                , Lamdera.sendToFrontend clientId (YouAreLoggedIn username)
                )

        LoginRequest { username, password } ->
            let
                badCredentials =
                    ( model
                    , Lamdera.sendToFrontend
                        clientId
                        (BadCredentials { username = username })
                    )
            in
            case Dict.get username model.userCredentials of
                Nothing ->
                    badCredentials

                Just correctPassword ->
                    if password == correctPassword then
                        ( { model
                            | userSessions =
                                Dict.insert sessionId username model.userSessions
                          }
                        , Lamdera.sendToFrontend clientId (YouAreLoggedIn username)
                        )

                    else
                        badCredentials

        LogoutRequest ->
            ( { model | userSessions = Dict.remove sessionId model.userSessions }
            , Lamdera.sendToFrontend clientId YouAreNotLoggedIn
            )

        AdminLoginRequest { password } ->
            if password == Env.adminPassword then
                ( { model | adminSession = Just sessionId }
                , Dict.keys model.userCredentials
                    |> AdminLoggedIn
                    |> Lamdera.sendToFrontend clientId
                )

            else
                ( model
                , Lamdera.sendToFrontend
                    clientId
                    (BadCredentials { username = "" })
                )

        AdminLogoutRequest ->
            { model | adminSession = Nothing }
                |> updateFromFrontend sessionId clientId GetLoginInfo

        DeleteUserRequest user ->
            case model.adminSession of
                Nothing ->
                    ( model, Cmd.none )

                Just adminSession ->
                    if adminSession == sessionId then
                        let
                            newModel =
                                { model
                                    | userCredentials =
                                        Dict.remove user model.userCredentials
                                    , userSessions =
                                        Dict.filter
                                            (\_ user_val -> user /= user_val)
                                            model.userSessions
                                }
                        in
                        ( newModel
                        , Dict.keys newModel.userCredentials
                            |> AdminLoggedIn
                            |> Lamdera.sendToFrontend clientId
                        )

                    else
                        ( model, Cmd.none )
