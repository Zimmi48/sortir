module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Lamdera
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, parse, s)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , state = Starting (parse urlParser url)
      }
    , Lamdera.sendToBackend GetLoginInfo
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                External url ->
                    ( model, Nav.load url )

        UrlChanged url ->
            routeChanged (parse urlParser url) model

        SignupButton ->
            ( model, Nav.pushUrl model.key "/signup" )

        LoginButton ->
            ( model, Nav.pushUrl model.key "/login" )

        LogoutButton ->
            case model.state of
                LoggedIn _ ->
                    ( model, Lamdera.sendToBackend LogoutRequest )

                AdminDashboard _ ->
                    ( { model | state = Starting Nothing }
                    , Cmd.batch
                        [ Nav.pushUrl model.key "/"
                        , Lamdera.sendToBackend AdminLogoutRequest
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        SendButton ->
            case model.state of
                NotLoggedIn (Signup { username, password, passwordAgain }) ->
                    if password == passwordAgain then
                        ( { model | state = Starting (Just SignupRoute) }
                        , Lamdera.sendToBackend
                            (SignupRequest
                                { username = username, password = password }
                            )
                        )

                    else
                        ( model, Cmd.none )

                NotLoggedIn (Login loginModel) ->
                    ( { model | state = Starting (Just LoginRoute) }
                    , Lamdera.sendToBackend
                        (LoginRequest
                            { username = loginModel.username
                            , password = loginModel.password
                            }
                        )
                    )

                AdminLogin { password } ->
                    ( model
                    , Lamdera.sendToBackend
                        (AdminLoginRequest { password = password })
                    )

                _ ->
                    ( model, Cmd.none )

        UsernameInput value ->
            case model.state of
                NotLoggedIn (Signup signupModel) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Signup
                                    { signupModel
                                        | username = value
                                        , alreadyExistingUsername = Nothing
                                    }
                                )
                      }
                    , Cmd.none
                    )

                NotLoggedIn (Login loginModel) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Login
                                    { loginModel
                                        | username = value
                                        , badCredentials = False
                                    }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    -- Not supposed to receive these messages in the other cases
                    ( model, Cmd.none )

        PasswordInput value ->
            case model.state of
                NotLoggedIn (Signup signupModel) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Signup { signupModel | password = value })
                      }
                    , Cmd.none
                    )

                NotLoggedIn (Login loginModel) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Login
                                    { loginModel
                                        | password = value
                                        , badCredentials = False
                                    }
                                )
                      }
                    , Cmd.none
                    )

                AdminLogin _ ->
                    ( { model
                        | state =
                            AdminLogin
                                { password = value, badCredentials = False }
                      }
                    , Cmd.none
                    )

                _ ->
                    -- Not supposed to receive these messages in the other cases
                    ( model, Cmd.none )

        PasswordAgainInput value ->
            case model.state of
                NotLoggedIn (Signup signupModel) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Signup { signupModel | passwordAgain = value })
                      }
                    , Cmd.none
                    )

                _ ->
                    -- Not supposed to receive these messages in the other cases
                    ( model, Cmd.none )

        DeleteUser user ->
            ( model, Lamdera.sendToBackend (DeleteUserRequest user) )


urlParser : Parser (Route -> a) a
urlParser =
    Parser.oneOf
        [ Parser.map HomeRoute Parser.top
        , Parser.map LoginRoute (s "login")
        , Parser.map SignupRoute (s "signup")
        , Parser.map AdminRoute (s "admin")
        ]


routeChanged : Maybe Route -> Model -> ( Model, Cmd FrontendMsg )
routeChanged maybeRoute model =
    let
        unknown =
            ( model, Nav.pushUrl model.key "/" )

        backToInit route =
            ( { model | state = Starting route }
            , Lamdera.sendToBackend GetLoginInfo
            )
    in
    case ( maybeRoute, model.state ) of
        ( Just AdminRoute, state ) ->
            case state of
                AdminLogin _ ->
                    ( model, Cmd.none )

                AdminDashboard _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | state =
                            AdminLogin { password = "", badCredentials = False }
                      }
                    , Cmd.none
                    )

        ( route, AdminLogin _ ) ->
            backToInit route

        ( route, AdminDashboard _ ) ->
            backToInit route

        ( Nothing, _ ) ->
            unknown

        ( Just route, Starting _ ) ->
            ( { model | state = Starting (Just route) }, Cmd.none )

        ( Just route, NotLoggedIn _ ) ->
            case route of
                HomeRoute ->
                    ( { model | state = NotLoggedIn Home }, Cmd.none )

                SignupRoute ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Signup
                                    { username = ""
                                    , password = ""
                                    , passwordAgain = ""
                                    , alreadyExistingUsername = Nothing
                                    }
                                )
                      }
                    , Cmd.none
                    )

                LoginRoute ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Login
                                    { username = ""
                                    , password = ""
                                    , badCredentials = False
                                    }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( Just route, LoggedIn _ ) ->
            case route of
                HomeRoute ->
                    ( model, Cmd.none )

                _ ->
                    unknown


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        YouAreNotLoggedIn ->
            let
                returnModel =
                    { model | state = NotLoggedIn Home }
            in
            case model.state of
                Starting route ->
                    routeChanged route returnModel

                _ ->
                    ( returnModel, Cmd.none )

        YouAreLoggedIn loggedInModel ->
            let
                returnModel =
                    { model | state = LoggedIn loggedInModel }
            in
            case model.state of
                Starting route ->
                    routeChanged route returnModel

                _ ->
                    ( returnModel, Cmd.none )

        UsernameAlreadyExists { username } ->
            case model.state of
                Starting (Just SignupRoute) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Signup
                                    { username = username
                                    , password = ""
                                    , passwordAgain = ""
                                    , alreadyExistingUsername =
                                        Just username
                                    }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        BadCredentials { username } ->
            case model.state of
                Starting (Just LoginRoute) ->
                    ( { model
                        | state =
                            NotLoggedIn
                                (Login
                                    { username = username
                                    , password = ""
                                    , badCredentials = True
                                    }
                                )
                      }
                    , Cmd.none
                    )

                AdminLogin _ ->
                    ( { model
                        | state =
                            AdminLogin
                                { password = "", badCredentials = True }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AdminLoggedIn users ->
            ( { model | state = AdminDashboard { users = users } }, Cmd.none )


view model =
    case model.state of
        Starting _ ->
            viewStarting

        LoggedIn loggedInModel ->
            viewLoggedIn loggedInModel

        NotLoggedIn Home ->
            viewHome

        NotLoggedIn (Login loginModel) ->
            viewLogin loginModel

        NotLoggedIn (Signup signupModel) ->
            viewSignup signupModel

        AdminLogin adminLoginModel ->
            viewAdminLogin adminLoginModel

        AdminDashboard adminModel ->
            viewAdminDashboard adminModel


viewStarting =
    { title = "Welcome to Sortir"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Starting" ]
            ]
        ]
    }


viewLoggedIn model =
    { title = "Sortir"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ "Hello " ++ model.username ++ ". This is your dashboard" |> Html.text ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "20px"
                ]
                [ Html.button [ onClick LogoutButton ] [ Html.text "Log out" ] ]
            ]
        ]
    }


viewHome =
    { title = "Welcome to Sortir"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Please log in or sign up" ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "20px"
                ]
                [ Html.button [ onClick LoginButton ] [ Html.text "Log in" ] ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "10px"
                ]
                [ Html.button [ onClick SignupButton ] [ Html.text "Sign up" ] ]
            ]
        ]
    }


viewLogin model =
    { title = "Welcome to Sortir | Login"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Please log in" ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "20px"
                ]
                [ viewInput "text" "Username" model.username UsernameInput
                , viewInput "password" "Password" model.password PasswordInput
                , viewBadCredentials model
                ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "10px"
                ]
                [ Html.button [ onClick SendButton ] [ Html.text "Log in" ] ]
            ]
        ]
    }


viewSignup model =
    { title = "Welcome to Sortir | Sign up"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Please sign up" ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "20px"
                ]
                [ viewInput "text" "Username" model.username UsernameInput
                , viewInput "password" "Password" model.password PasswordInput
                , viewInput "password" "Retype password" model.passwordAgain PasswordAgainInput
                , viewValidation model
                , viewUsernameAlreadyExists model
                ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "10px"
                ]
                [ Html.button [ onClick SendButton ] [ Html.text "Sign up" ] ]
            ]
        ]
    }


viewAdminLogin model =
    { title = "Sortir | Admin login page"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "Please log in" ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "20px"
                ]
                [ viewInput "password" "Password" model.password PasswordInput
                , viewBadCredentials model
                ]
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "10px"
                ]
                [ Html.button [ onClick SendButton ] [ Html.text "Log in" ] ]
            ]
        ]
    }


viewAdminDashboard model =
    { title = "Sortir | Admin dashboard"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            ([ Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text "This is the list of user accounts:" ]
             ]
                ++ List.map viewUser model.users
                ++ [ Html.div
                        [ Attr.style "font-family" "sans-serif"
                        , Attr.style "padding-top" "20px"
                        ]
                        [ Html.button [ onClick LogoutButton ] [ Html.text "Log out" ] ]
                   ]
            )
        ]
    }


viewInput t p v toMsg =
    Html.div []
        [ Html.input
            [ Attr.type_ t
            , Attr.placeholder p
            , Attr.value v
            , onInput toMsg
            ]
            []
        ]


viewValidation : { a | password : String, passwordAgain : String } -> Html msg
viewValidation model =
    if model.password == "" || model.passwordAgain == "" then
        Html.div [] []

    else if model.password == model.passwordAgain then
        Html.div [ Attr.style "color" "green" ] [ Html.text "Passwords match!" ]

    else
        Html.div [ Attr.style "color" "red" ] [ Html.text "Passwords do not match!" ]


viewBadCredentials model =
    if model.badCredentials then
        Html.div [ Attr.style "color" "red" ] [ Html.text "Bad username or password!" ]

    else
        Html.div [] []


viewUsernameAlreadyExists model =
    case model.alreadyExistingUsername of
        Just username ->
            Html.div
                [ Attr.style "color" "red" ]
                [ "Username " ++ username ++ " already exists!" |> Html.text ]

        Nothing ->
            Html.div [] []


viewUser user =
    Html.div
        [ Attr.style "font-family" "sans-serif"
        , Attr.style "padding-top" "20px"
        ]
        [ Html.text user
        , Html.span
            [ Attr.style "padding-left" "15px" ]
            [ Html.button
                [ Attr.style "font-style"
                    "italic"
                , onClick
                    (DeleteUser user)
                ]
                [ Html.text "Delete" ]
            ]
        ]
