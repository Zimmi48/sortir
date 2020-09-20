module Backend exposing (..)

import Array
import Array.Extra as Array
import DateFormat
import Debug
import Dict exposing (Dict)
import Env
import Html
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, decodeValue)
import Lamdera exposing (ClientId, SessionId)
import List.Extra as List
import ListDate exposing (listToDate)
import Parser exposing ((|.), (|=), Parser)
import Result.Extra as Result
import SHA1
import Task
import Time
import TimeZone
import Tuple
import Types exposing (..)
import Url.Builder exposing (QueryParameter)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Time.every (60 * 1000) SetTime
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { userCredentials = Dict.empty
      , userSessions = Dict.empty
      , adminSession = Nothing
      , movies = Dict.empty
      , theaters = Dict.empty
      , showtimes = []
      , now = Time.millisToPosix 0
      , lastAllocineResponse =
            Array.repeat
                (Array.length Env.allocineQueries)
                (Array.fromList [ NotAsked ])
      }
    , Task.perform SetTime Time.now
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        SetTime now ->
            ( { model
                | now = now
                , showtimes =
                    List.dropWhile
                        (\( time, _ ) ->
                            Time.posixToMillis time < Time.posixToMillis now
                        )
                        model.showtimes
              }
            , Cmd.none
            )

        AllocineResponse { queryNum, page } (Ok response) ->
            if page == 1 then
                case getPageNumber response of
                    Ok 1 ->
                        -- When there is a single page we request the next query
                        ( { model
                            | lastAllocineResponse =
                                model.lastAllocineResponse
                                    |> Array.set queryNum
                                        (Array.fromList [ Success response ])
                                    |> Array.set (queryNum + 1)
                                        (Array.fromList [ Loading ])
                          }
                        , queryAllocine
                            { queryNum = queryNum + 1, page = 1 }
                            model.now
                        )

                    Ok pageNumber ->
                        -- When there are multiple pages, we request the next one
                        ( { model
                            | lastAllocineResponse =
                                model.lastAllocineResponse
                                    |> Array.set queryNum
                                        (Array.initialize pageNumber
                                            (\i ->
                                                if i == 0 then
                                                    Success response

                                                else if i == 1 then
                                                    Loading

                                                else
                                                    NotAsked
                                            )
                                        )
                          }
                        , queryAllocine
                            { queryNum = queryNum, page = 2 }
                            model.now
                        )

                    Err error ->
                        ( { model
                            | lastAllocineResponse =
                                Array.update queryNum
                                    (\pages ->
                                        Array.set (page - 1)
                                            (error
                                                |> Decode.errorToString
                                                |> Http.BadBody
                                                |> Failure
                                            )
                                            pages
                                    )
                                    model.lastAllocineResponse
                          }
                        , Cmd.none
                        )

            else
                -- We are not requesting the first page
                -- so we already know the number of pages
                case Array.get queryNum model.lastAllocineResponse of
                    Just pages ->
                        if page == Array.length pages then
                            -- We have no more pages to get
                            ( { model
                                | lastAllocineResponse =
                                    model.lastAllocineResponse
                                        |> Array.set queryNum
                                            (Array.set (page - 1)
                                                (Success response)
                                                pages
                                            )
                                        |> Array.set (queryNum + 1)
                                            (Array.fromList [ Loading ])
                              }
                            , queryAllocine
                                { queryNum = queryNum + 1, page = 1 }
                                model.now
                            )

                        else
                            ( { model
                                | lastAllocineResponse =
                                    Array.set queryNum
                                        (pages
                                            |> Array.set (page - 1)
                                                (Success response)
                                            |> Array.set page Loading
                                        )
                                        model.lastAllocineResponse
                              }
                            , queryAllocine
                                { queryNum = queryNum, page = page + 1 }
                                model.now
                            )

                    Nothing ->
                        --should never happen
                        ( model
                        , "Corrupted lastAllocineResponse: "
                            ++ Debug.toString model.lastAllocineResponse
                            |> sendLog model
                        )

        AllocineResponse { queryNum, page } (Err error) ->
            ( { model
                | lastAllocineResponse =
                    Array.update queryNum
                        (\pages -> Array.set (page - 1) (Failure error) pages)
                        model.lastAllocineResponse
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        GetLoginInfo ->
            case Dict.get sessionId model.userSessions of
                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId YouAreNotLoggedIn )

                Just user ->
                    ( model
                    , Lamdera.sendToFrontend
                        clientId
                        (YouAreLoggedIn { username = user })
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
                , Lamdera.sendToFrontend
                    clientId
                    (YouAreLoggedIn { username = username })
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
                        , Lamdera.sendToFrontend
                            clientId
                            (YouAreLoggedIn { username = username })
                        )

                    else
                        badCredentials

        LogoutRequest ->
            ( { model | userSessions = Dict.remove sessionId model.userSessions }
            , Lamdera.sendToFrontend clientId YouAreNotLoggedIn
            )

        AdminLoginRequest { password } ->
            if password == Env.adminPassword then
                ( { model | adminSession = Just clientId }
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

        AdminRequest request ->
            case model.adminSession of
                Nothing ->
                    ( model, Cmd.none )

                Just adminSession ->
                    if adminSession == clientId then
                        adminRequestUpdate clientId request model

                    else
                        ( model, Cmd.none )

        MovieRequest code ->
            case Dict.get code model.movies of
                Just movie ->
                    ( model
                    , movie
                        |> Result.Ok
                        |> MovieResponse code
                        |> Lamdera.sendToFrontend clientId
                    )

                Nothing ->
                    ( model
                    , Result.Err "Not found"
                        |> MovieResponse code
                        |> Lamdera.sendToFrontend clientId
                    )

        NextShowtimesRequest searchCriteria ->
            ( model
            , getFirstShowtimes searchCriteria model
                |> NextShowtimesResponse
                |> Lamdera.sendToFrontend clientId
            )


adminRequestUpdate clientId msg model =
    case msg of
        DeleteUserRequest user ->
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

        CallAllocineApi ->
            ( { model
                | lastAllocineResponse =
                    Array.set 0
                        (Array.fromList [ Loading ])
                        model.lastAllocineResponse
              }
            , queryAllocine { queryNum = 0, page = 1 } model.now
            )

        Decode ->
            let
                results =
                    model.lastAllocineResponse
                        |> Array.toList
                        |> List.map Array.toList
                        |> List.concat
            in
            case
                Result.map3
                    (\theaters movies showtimes ->
                        { model
                            | theaters =
                                Dict.union
                                    (Dict.fromList theaters)
                                    model.theaters
                            , movies =
                                Dict.union
                                    (Dict.fromList movies)
                                    model.movies
                            , showtimes = showtimes
                        }
                    )
                    (decodeTheaters results)
                    (decodeMovies results)
                    (decodeShowtimes results)
            of
                Ok newModel ->
                    ( newModel, Cmd.none )

                Err error ->
                    ( model
                    , "Error while decoding: "
                        ++ error
                        |> Log
                        |> Lamdera.sendToFrontend clientId
                    )


sendLog model log =
    case model.adminSession of
        Nothing ->
            Cmd.none

        Just clientId ->
            Log log |> Lamdera.sendToFrontend clientId


buildAllocineUrl : String -> List QueryParameter -> Time.Posix -> String
buildAllocineUrl method params now =
    let
        sed =
            DateFormat.format "yyyyMMdd" Time.utc now

        completeParams =
            [ Url.Builder.string "partner" Env.allocinePartner
            , Url.Builder.string "sed" sed
            ]
                ++ params

        sig =
            method
                ++ (completeParams |> Url.Builder.toQuery |> String.dropLeft 1)
                ++ Env.allocineKey
                |> SHA1.fromString
                |> SHA1.toBase64
    in
    completeParams
        ++ [ Url.Builder.string "sig" sig ]
        |> Url.Builder.crossOrigin
            "http://api.allocine.fr"
            [ "rest", "v3", method ]


showtimeListUrl : { zip : String, radius : Int } -> Int -> Time.Posix -> String
showtimeListUrl { zip, radius } page =
    buildAllocineUrl "showtimelist"
        [ Url.Builder.string "zip" zip
        , Url.Builder.int "radius" radius
        , Url.Builder.int "page" page
        ]


queryAllocine params now =
    case Array.get params.queryNum Env.allocineQueries of
        Nothing ->
            Cmd.none

        Just query ->
            Http.get
                { url = showtimeListUrl query params.page now
                , expect = Http.expectString (AllocineResponse params)
                }


getPageNumber json =
    json
        |> decodeString countDecoder
        |> Result.map
            (\{ count, totalResults } ->
                (toFloat totalResults / toFloat count) |> ceiling |> max 1
            )


countDecoder =
    Decode.field "feed"
        (Decode.map2 (\x y -> { count = x, totalResults = y })
            (Decode.field "count" Decode.int)
            (Decode.field "totalResults" Decode.int)
        )


decodeTheaters : List (WebData String) -> Result String (List ( String, Theater ))
decodeTheaters results =
    results
        |> Result.combineMap
            (\result ->
                case result of
                    Success json ->
                        json
                            |> decodeString theatersDecoder
                            |> Result.mapError Decode.errorToString

                    _ ->
                        Result.Err "Not a success"
            )
        |> Result.map List.concat


theatersDecoder : Decoder (List ( String, Theater ))
theatersDecoder =
    Decode.at [ "feed", "theaterShowtimes" ]
        (Decode.list
            (Decode.at [ "place", "theater" ]
                (Decode.map2 Tuple.pair
                    (Decode.field "code" Decode.string)
                    theaterDecoder
                )
            )
        )


theaterDecoder : Decoder Theater
theaterDecoder =
    Decode.map3 Theater
        (Decode.field "name" Decode.string)
        (Decode.field "address" Decode.string)
        (Decode.field "postalCode" Decode.string)


decodeMovies : List (WebData String) -> Result String (List ( Int, Movie ))
decodeMovies results =
    results
        |> Result.combineMap
            (\result ->
                case result of
                    Success json ->
                        json
                            |> decodeString moviesDecoder
                            |> Result.mapError Decode.errorToString

                    _ ->
                        Result.Err "Not a success"
            )
        |> Result.map (List.concat >> List.concat >> List.uniqueBy Tuple.first)
        |> Result.andThen
            (Result.combineMap
                (Result.combineMapSecond (decodeValue movieDecoder))
                >> Result.mapError Decode.errorToString
            )


moviesDecoder : Decoder (List (List ( Int, Decode.Value )))
moviesDecoder =
    Decode.at [ "feed", "theaterShowtimes" ]
        (Decode.list
            (Decode.field "movieShowtimes"
                (Decode.list
                    (Decode.at [ "onShow", "movie" ]
                        (Decode.map2 Tuple.pair
                            (Decode.field "code" Decode.int)
                            Decode.value
                        )
                    )
                )
            )
        )


movieDecoder : Decoder Movie
movieDecoder =
    Decode.map6 Movie
        (Decode.field "title" Decode.string)
        (Decode.maybe (Decode.at [ "castingShort", "directors" ] Decode.string))
        (Decode.maybe (Decode.at [ "castingShort", "actors" ] Decode.string))
        (Decode.maybe (Decode.at [ "release", "releaseDate" ] Decode.string)
            |> Decode.map (Maybe.andThen (String.left 4 >> String.toInt))
        )
        (Decode.maybe (Decode.field "runtime" Decode.int))
        (Decode.maybe (Decode.at [ "poster", "href" ] Decode.string))


decodeShowtimes :
    List (WebData String)
    ->
        Result String
            (List
                ( Time.Posix
                , List
                    { movie : Int
                    , theater : String
                    }
                )
            )
decodeShowtimes results =
    results
        |> Result.combineMap
            (\result ->
                case result of
                    Success json ->
                        json
                            |> decodeString showtimesDecoder
                            |> Result.mapError Decode.errorToString

                    _ ->
                        Result.Err "Not a success"
            )
        |> Result.map List.concat
        |> Result.map (List.sortBy (Tuple.first >> Time.posixToMillis))
        |> Result.map gather


gather : List ( a, b ) -> List ( a, List b )
gather =
    List.foldr
        (\( time1, show ) acc ->
            case acc of
                [] ->
                    [ ( time1, [ show ] ) ]

                ( time2, shows ) :: queue ->
                    if time1 == time2 then
                        ( time1, show :: shows ) :: queue

                    else
                        ( time1, [ show ] ) :: acc
        )
        []


getFirstShowtimes : SearchCriteria -> Model -> List Showtime
getFirstShowtimes criteria model =
    let
        aux number showtimes =
            case ( number, showtimes ) of
                ( 0, _ ) ->
                    []

                ( _, [] ) ->
                    []

                ( _, ( _, [] ) :: tail ) ->
                    aux number tail

                ( _, ( time, show :: showtail ) :: tail ) ->
                    case
                        Maybe.map2
                            (\movie theater ->
                                { time = time
                                , movie = movie
                                , theater = theater
                                , movieCode = show.movie
                                , theaterCode = show.theater
                                }
                            )
                            (Dict.get show.movie model.movies)
                            (Dict.get show.theater model.theaters)
                    of
                        Just showtime ->
                            if matchesCriteria criteria showtime then
                                showtime
                                    :: aux (number - 1)
                                        (( time, showtail ) :: tail)

                            else
                                aux number (( time, showtail ) :: tail)

                        Nothing ->
                            aux number (( time, showtail ) :: tail)
    in
    aux 10 model.showtimes


matchesCriteria : SearchCriteria -> Showtime -> Bool
matchesCriteria criteria { time, movie, theater } =
    let
        releaseYearMin =
            case ( movie.releaseYear, criteria.releaseYearMin ) of
                ( Just releaseYear, Just min ) ->
                    releaseYear >= min

                _ ->
                    True

        releaseYearMax =
            case ( movie.releaseYear, criteria.releaseYearMax ) of
                ( Just releaseYear, Just max ) ->
                    releaseYear <= max

                _ ->
                    True
    in
    releaseYearMin && releaseYearMax


showtimesDecoder : Decoder (List ( Time.Posix, { movie : Int, theater : String } ))
showtimesDecoder =
    (Decode.at [ "place", "theater", "code" ] Decode.string
        |> Decode.andThen
            (\theater ->
                (Decode.at [ "onShow", "movie", "code" ] Decode.int
                    |> Decode.andThen
                        (\movie ->
                            (Decode.field "d" Decode.string
                                |> Decode.andThen
                                    (\date ->
                                        (Decode.field "$" Decode.string
                                            |> Decode.andThen
                                                (\time ->
                                                    case toPosix date time of
                                                        Ok posix ->
                                                            Decode.succeed
                                                                ( posix
                                                                , { movie = movie
                                                                  , theater = theater
                                                                  }
                                                                )

                                                        Err error ->
                                                            Decode.fail error
                                                )
                                        )
                                            |> Decode.list
                                            |> Decode.field "t"
                                    )
                            )
                                |> Decode.list
                                |> Decode.map List.concat
                                |> Decode.field "scr"
                        )
                )
                    |> Decode.list
                    |> Decode.map List.concat
                    |> Decode.field "movieShowtimes"
            )
    )
        |> Decode.list
        |> Decode.map List.concat
        |> Decode.at [ "feed", "theaterShowtimes" ]


toPosix : String -> String -> Result String Time.Posix
toPosix date time =
    let
        twoDigits =
            Parser.oneOf
                [ Parser.succeed Basics.identity
                    |. Parser.symbol "0"
                    |= Parser.int
                , Parser.int
                ]

        dateParser =
            Parser.succeed (\y m d -> { year = y, month = m, day = d })
                |= Parser.int
                |. Parser.symbol "-"
                |= twoDigits
                |. Parser.symbol "-"
                |= twoDigits
                |. Parser.end

        timeParser =
            Parser.succeed (\h m -> { hour = h, minutes = m })
                |= twoDigits
                |. Parser.symbol ":"
                |= twoDigits
                |. Parser.end
    in
    Result.combineBoth
        ( Parser.run dateParser date, Parser.run timeParser time )
        |> Result.mapError Parser.deadEndsToString
        |> Result.andThen
            (\( { year, month, day }, { hour, minutes } ) ->
                listToDate (TimeZone.europe__paris ())
                    [ year, month, day, hour, minutes ]
            )
