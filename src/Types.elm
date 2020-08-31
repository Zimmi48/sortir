module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (SessionId)
import Url exposing (Url)


type NotLoggedInPage
    = Home
    | Signup
        { username : String
        , password : String
        , passwordAgain : String
        , alreadyExistingUsername : Maybe String
        }
    | Login
        { username : String
        , password : String
        , badCredentials : Bool
        }


type Route
    = HomeRoute
    | LoginRoute
    | SignupRoute


type LocalState
    = Starting (Maybe Route)
    | LoggedIn
    | NotLoggedIn NotLoggedInPage


type alias FrontendModel =
    { key : Key
    , state : LocalState
    }


type alias BackendModel =
    { userCredentials : Dict String String
    , userSessions : Dict SessionId String
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | LoginButton
    | SignupButton
    | LogoutButton
    | SendButton
    | UsernameInput String
    | PasswordInput String
    | PasswordAgainInput String


type ToBackend
    = GetLoginInfo
    | SignupRequest { username : String, password : String }
    | LoginRequest { username : String, password : String }
    | LogoutRequest


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = YouAreNotLoggedIn
    | YouAreLoggedIn String
    | UsernameAlreadyExists { username : String }
    | BadCredentials { username : String }
