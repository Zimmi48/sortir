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


type RemoteData a b
    = NotAsked
    | Loading
    | Failure b
    | Success a


resultToRemoteData result =
    case result of
        Ok ok ->
            Success ok

        Err err ->
            Failure err


type alias WebData a =
    RemoteData a Http.Error


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


type LoggedInPage
    = Dashboard (RemoteData (List Showtime) String)
    | MoviePage Int (RemoteData Movie String)
    | SearchPage SearchForm (RemoteData (List Showtime) String)


type Route
    = HomeRoute
    | LoginRoute
    | SignupRoute
    | AdminRoute
    | MovieRoute Int
    | SearchRoute


type LocalState
    = Starting (Maybe Route)
    | LoggedIn { username : String, page : LoggedInPage }
    | NotLoggedIn NotLoggedInPage
    | AdminLogin { password : String, badCredentials : Bool }
    | AdminDashboard { backendLog : List String, users : List String }


type alias SearchForm =
    { releaseYearMin : Maybe Int
    , releaseYearMax : Maybe Int
    }


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


type alias Showtime =
    { time : Time.Posix
    , movieCode : Int
    , movie : Movie
    , theaterCode : String
    , theater : Theater
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
    , lastAllocineResponse : Array (Array (WebData String))
    }


type MinMax
    = Min
    | Max


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | LogoutButton
    | SendButton
    | UsernameInput String
    | PasswordInput String
    | PasswordAgainInput String
    | ReleaseYearInput MinMax String
    | AdminAction AdminRequest


type ToBackend
    = GetLoginInfo
    | SignupRequest { username : String, password : String }
    | LoginRequest { username : String, password : String }
    | LogoutRequest
    | AdminLoginRequest { password : String }
    | AdminLogoutRequest
    | AdminRequest AdminRequest
    | MovieRequest Int
    | NextShowtimesRequest SearchCriteria


type alias SearchCriteria =
    SearchForm


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
    | MovieResponse Int (Result String Movie)
    | NextShowtimesResponse (List Showtime)
