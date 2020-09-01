module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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
        [ "Starting"
            |> Element.text
            |> Element.el [ Element.centerX, Element.centerY ]
            |> List.singleton
            |> Element.row [ Element.width Element.fill ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewLoggedIn model =
    { title = "Sortir"
    , body =
        [ topBar model
            |> Element.layout [ Element.padding 20 ]
        ]
    }


topBar model =
    Element.row
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 7
        ]
        [ "Sortir dashboard" |> Element.text
        , "Logged in as "
            ++ model.username
            ++ "."
            |> Element.text
            |> Element.el [ Element.alignRight ]
        , Input.button linkStyle
            { onPress = Just LogoutButton
            , label = Element.text "Log out"
            }
        ]


viewHome =
    { title = "Welcome to Sortir"
    , body =
        [ [ Element.text "Please "
          , Element.link linkStyle
                { url = "/login", label = Element.text "log in" }
          , Element.text " or "
          , Element.link linkStyle
                { url = "/signup", label = Element.text "sign up" }
          , Element.text "."
          ]
            |> Element.paragraph []
            |> Element.el [ Element.centerX, Element.centerY ]
            |> List.singleton
            |> Element.row [ Element.width Element.fill ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewLogin model =
    { title = "Welcome to Sortir | Login"
    , body =
        [ [ Element.text "Please log in"
          , Input.username []
                { onChange = UsernameInput
                , text = model.username
                , placeholder = Nothing
                , label = "Username:" |> Element.text |> Input.labelLeft []
                }
          , Input.currentPassword []
                { onChange = PasswordInput
                , text = model.password
                , placeholder = Nothing
                , label = "Password:" |> Element.text |> Input.labelLeft []
                , show = False
                }
          , viewBadCredentials model
          , Input.button buttonStyle
                { onPress = Just SendButton
                , label = Element.text "Log in"
                }
          ]
            |> Element.column [ Element.centerX, Element.spacing 10 ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewSignup model =
    { title = "Welcome to Sortir | Sign up"
    , body =
        [ [ Element.text "Please sign up"
          , Input.username []
                { onChange = UsernameInput
                , text = model.username
                , placeholder = Nothing
                , label = "Username:" |> Element.text |> Input.labelLeft []
                }
          , Input.newPassword []
                { onChange = PasswordInput
                , text = model.password
                , placeholder = Nothing
                , label = "Password:" |> Element.text |> Input.labelLeft []
                , show = False
                }
          , Input.newPassword []
                { onChange = PasswordAgainInput
                , text = model.passwordAgain
                , placeholder = Nothing
                , label = "Retype password:" |> Element.text |> Input.labelLeft []
                , show = False
                }
          , viewValidation model
          , viewUsernameAlreadyExists model
          , Input.button buttonStyle
                { onPress = Just SendButton
                , label = Element.text "Log in"
                }
          ]
            |> Element.column [ Element.centerX, Element.spacing 10 ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewAdminLogin model =
    { title = "Sortir | Admin login page"
    , body =
        [ [ Element.text "Please log in"
          , Input.currentPassword []
                { onChange = PasswordInput
                , text = model.password
                , placeholder = Nothing
                , label = "Password:" |> Element.text |> Input.labelLeft []
                , show = False
                }
          , viewBadCredentials model
          , Input.button buttonStyle
                { onPress = Just SendButton
                , label = Element.text "Log in"
                }
          ]
            |> Element.column [ Element.centerX, Element.spacing 10 ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewAdminDashboard model =
    { title = "Sortir | Admin dashboard"
    , body =
        [ [ Element.row
                [ Element.width Element.fill
                , Element.padding 10
                , Element.spacing 7
                ]
                [ "Admin dashboard" |> Element.text
                , Input.button (linkStyle ++ [ Element.alignRight ])
                    { onPress = Just LogoutButton
                    , label = Element.text "Log out"
                    }
                ]
          , "This is the list of user accounts:"
                |> Element.text
                |> Element.el [ Element.centerX ]
          ]
            ++ List.map viewUser model.users
            |> Element.column
                [ Element.width Element.fill
                , Element.padding 20
                , Element.spacing 15
                ]
            |> Element.layout [ Element.padding 20 ]
        ]
    }


viewValidation model =
    if model.password == "" || model.passwordAgain == "" then
        Element.none

    else if model.password == model.passwordAgain then
        "Passwords match!"
            |> Element.text
            |> Element.el [ Font.color (Element.rgb255 0 128 0) ]

    else
        "Passwords do not match!"
            |> Element.text
            |> Element.el [ Font.color (Element.rgb255 255 0 0) ]


viewBadCredentials model =
    if model.badCredentials then
        "Bad username or password!"
            |> Element.text
            |> Element.el [ Font.color (Element.rgb255 255 0 0) ]

    else
        Element.none


viewUsernameAlreadyExists model =
    case model.alreadyExistingUsername of
        Just username ->
            "Username "
                ++ username
                ++ " already exists!"
                |> Element.text
                |> Element.el [ Font.color (Element.rgb255 255 0 0) ]

        Nothing ->
            Element.none


viewUser user =
    Element.row [ Element.centerX, Element.spacing 20 ]
        [ Element.text user
        , Input.button linkStyle { onPress = Just (DeleteUser user), label = Element.text "Delete" }
        ]


linkStyle =
    [ Font.color (Element.rgb255 178 34 34)
    , Font.underline
    ]


buttonStyle =
    [ Background.color (Element.rgb255 220 220 220)
    , Border.rounded 7
    , Border.width 1
    , Border.color (Element.rgb255 128 128 128)
    , Element.paddingXY 30 7
    ]
