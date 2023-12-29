module Backend exposing (..)

import Common exposing (defaultSettings, points)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Sha256 exposing (sha256)
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel



--noinspection ElmUnusedSymbol


app : { init : ( Model, Cmd BackendMsg ), update : BackendMsg -> Model -> ( Model, Cmd BackendMsg ), updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg ), subscriptions : Model -> Sub BackendMsg }
app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Time.every hour TimeTick
        }


hour : number
hour =
    1000 * 60 * 60


month : number
month =
    hour * 24 * 30


init : ( Model, Cmd BackendMsg )
init =
    ( { currentSaltIndex = 0, sessions = Dict.empty, users = Dict.empty, currentTime = Time.millisToPosix 0 }
    , Time.now |> Task.perform TimeTick
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        TimeTick time ->
            let
                expiredSessions =
                    model.sessions
                        |> Dict.filter
                            (\_ { created } -> Time.posixToMillis created + month < Time.posixToMillis time)
            in
            ( { model
                | sessions = Dict.diff model.sessions expiredSessions
                , currentTime = time
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        maybeSession =
            Dict.get sessionId model.sessions

        maybeUser =
            maybeSession
                |> Maybe.map .username
                |> Maybe.andThen (\username -> Dict.get username model.users)

        throwOut =
            Lamdera.sendToFrontend clientId KickOut
    in
    case msg of
        UpdateSettings settings ->
            case maybeUser of
                Nothing ->
                    ( model, throwOut )

                Just { username } ->
                    let
                        updateUser user =
                            { user | settings = settings }
                    in
                    ( { model | users = Dict.update username (Maybe.map updateUser) model.users }, Cmd.none )

        GetSettings ->
            case maybeUser of
                Just user ->
                    ( model, Lamdera.sendToFrontend clientId <| GotSettings user.settings )

                Nothing ->
                    ( model, throwOut )

        InsertUser username password ->
            let
                uLength =
                    String.length username

                validUsername =
                    String.all Char.isAlphaNum username
                        && (uLength >= 3)
                        && (uLength <= 32)

                pwLength =
                    String.length password

                validPassword =
                    -- https://www.ibm.com/docs/en/baw/19.x?topic=security-characters-that-are-valid-user-ids-passwords
                    String.all (\c -> Char.isAlphaNum c || List.member c (String.toList "!()-.?[]_`~;:@#$%^&*+= ")) password
                        && (pwLength >= 8)
                        && (pwLength <= 64)

                salt =
                    String.fromInt model.currentSaltIndex

                hash =
                    sha256 (salt ++ password)

                user : User
                user =
                    { username = username
                    , passwordHash = hash
                    , passwordSalt = salt
                    , settings = defaultSettings
                    , pastDictations = []
                    }

                allUsernames =
                    allUsers model |> List.map .username

                usernameTaken =
                    List.member username allUsernames

                session =
                    { username = username, created = model.currentTime }
            in
            if validUsername && validPassword && not usernameTaken then
                ( { model
                    | sessions = Dict.insert sessionId session model.sessions
                    , currentSaltIndex = model.currentSaltIndex + 1
                    , users = Dict.insert username user model.users
                  }
                , Lamdera.sendToFrontend clientId LoginSuccessful
                )

            else
                ( model, Lamdera.sendToFrontend clientId RegisterFailed )

        InsertSession username password ->
            case Dict.get username model.users of
                Just user ->
                    let
                        hash =
                            sha256 (user.passwordSalt ++ password)
                    in
                    if hash == user.passwordHash then
                        let
                            session =
                                { username = username, created = model.currentTime }

                            sessions =
                                Dict.insert sessionId session model.sessions
                        in
                        ( { model | sessions = sessions }, Lamdera.sendToFrontend clientId LoginSuccessful )

                    else
                        ( model, Lamdera.sendToFrontend clientId LoginFailed )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId LoginFailed )

        GetSession ->
            if maybeSession == Nothing then
                ( model, Lamdera.sendToFrontend clientId KickOut )

            else
                ( model, Lamdera.sendToFrontend clientId LoginSuccessful )

        RemoveSession ->
            case maybeSession of
                Just _ ->
                    ( { model
                        | sessions = Dict.remove sessionId model.sessions
                      }
                    , throwOut
                    )

                Nothing ->
                    ( model, throwOut )

        ConsStatistic pastDictation ->
            case maybeSession of
                Just { username } ->
                    let
                        updateUser user =
                            { user | pastDictations = pastDictation :: user.pastDictations }
                    in
                    ( { model | users = Dict.update username (Maybe.map updateUser) model.users }, Cmd.none )

                Nothing ->
                    ( model, throwOut )

        GetStatistic ->
            case maybeUser of
                Just user ->
                    ( model, Lamdera.sendToFrontend clientId <| UpdateStatistic user.pastDictations )

                Nothing ->
                    ( model, throwOut )

        GetAppStatistic ->
            let
                userCount =
                    List.length <| allUsers <| model

                pastDictationCount =
                    model
                        |> allUsers
                        |> List.concatMap .pastDictations
                        |> List.length

                statistic : AppStatistic
                statistic =
                    { userCount = userCount
                    , pastDictationCount = pastDictationCount
                    }
            in
            ( model, Lamdera.sendToFrontend clientId <| UpdateAppStatistic statistic )

        GetAllPoints lesson ->
            let
                allPoints : List Int
                allPoints =
                    model
                        |> allUsers
                        |> List.concatMap .pastDictations
                        |> List.filter (\p -> p.lesson == lesson)
                        |> List.map points
            in
            ( model, Lamdera.sendToFrontend clientId <| UpdateAllPoints allPoints )


allUsers : { a | users : Dict Username User } -> List User
allUsers { users } =
    Dict.values users



{-
   needsLogin msg =
       case msg of
           UpdateSettings _ ->
               True

           GetSettings ->
               True

           InsertUser _ _ ->
               False

           InsertSession _ _ ->
               False

           GetSession ->
               True

           RemoveSession ->
               True
-}
