module Types exposing (..)

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Http
import Lamdera exposing (ClientId, SessionId)
import Result exposing (Result)
import Time
import Url exposing (Url)


type RemoteData a
    = NotAsked
    | Loading
    | Failure Http.Error
    | Success a


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
    | AdminRoute


type LocalState
    = Starting (Maybe Route)
    | LoggedIn { username : String }
    | NotLoggedIn NotLoggedInPage
    | AdminLogin { password : String, badCredentials : Bool }
    | AdminDashboard { users : List String }


type alias Movie =
    { title : String
    , directors : Maybe String
    , actors : Maybe String
    , releaseYear : Maybe Int
    , runtime : Maybe Int
    , posterLink : Maybe String
    }


type alias Theater =
    { name : String
    , address : String
    , postalCode : String
    }


type alias FrontendModel =
    { key : Key
    , state : LocalState
    }


type alias BackendModel =
    { userCredentials : Dict String String
    , userSessions : Dict SessionId String
    , adminSession : Maybe ClientId
    , movies : Dict Int Movie
    , theaters : Dict String Theater
    , showtimes : List ( Time.Posix, List { movie : Int, theater : String } )
    , now : Time.Posix
    , lastAllocineResponse : Array (Array (RemoteData String))
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | LogoutButton
    | SendButton
    | UsernameInput String
    | PasswordInput String
    | PasswordAgainInput String
    | AdminAction AdminRequest


type ToBackend
    = GetLoginInfo
    | SignupRequest { username : String, password : String }
    | LoginRequest { username : String, password : String }
    | LogoutRequest
    | AdminLoginRequest { password : String }
    | AdminLogoutRequest
    | AdminRequest AdminRequest


type AdminRequest
    = DeleteUserRequest String
    | CallAllocineApi
    | Decode


type BackendMsg
    = SetTime Time.Posix
    | AllocineResponse { queryNum : Int, page : Int } (Result Http.Error String)


type ToFrontend
    = YouAreNotLoggedIn
    | YouAreLoggedIn { username : String }
    | UsernameAlreadyExists { username : String }
    | BadCredentials { username : String }
    | AdminLoggedIn (List String)
    | Log String
