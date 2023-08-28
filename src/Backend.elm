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
    ( { currentSaltIndex = 0, activeSessions = Dict.empty, passiveUsers = Dict.empty, currentTime = Time.millisToPosix 0 }
    , Time.now |> Task.perform TimeTick
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        TimeTick time ->
            let
                expiredSessions =
                    model.activeSessions
                        |> Dict.filter
                            (\_ { created } -> Time.posixToMillis created + month < Time.posixToMillis time)

                expiredUsers =
                    expiredSessions |> Dict.values |> List.map (\{ user } -> ( user.username, user )) |> Dict.fromList
            in
            ( { model
                | activeSessions = Dict.diff model.activeSessions expiredSessions
                , passiveUsers = Dict.union model.passiveUsers expiredUsers
                , currentTime = time
              }
            , Cmd.none
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        maybeSession =
            Dict.get sessionId model.activeSessions

        throwOut =
            Lamdera.sendToFrontend clientId KickOut
    in
    case msg of
        UpdateSettings settings ->
            let
                updateUser user =
                    { user | settings = settings }

                updateSession session =
                    { session | user = updateUser session.user }
            in
            ( { model | activeSessions = Dict.update sessionId (Maybe.map updateSession) model.activeSessions }, Cmd.none )

        GetSettings ->
            case maybeSession of
                Just session ->
                    ( model, Lamdera.sendToFrontend clientId <| GotSettings session.user.settings )

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
                    String.all (\c -> Char.isAlphaNum c || List.member c (String.toList "!()-.?[]_`~;:@#$%^&*+=")) password
                        && (pwLength >= 10)
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
                    { user = user, created = model.currentTime }
            in
            if validUsername && validPassword && not usernameTaken then
                ( { model | activeSessions = Dict.insert sessionId session model.activeSessions, currentSaltIndex = model.currentSaltIndex + 1 }
                , Lamdera.sendToFrontend clientId LoginSuccessful
                )

            else
                ( model, Lamdera.sendToFrontend clientId RegisterFailed )

        InsertSession username password ->
            case Dict.get username model.passiveUsers of
                Just user ->
                    let
                        hash =
                            sha256 (user.passwordSalt ++ password)
                    in
                    if hash == user.passwordHash then
                        let
                            session =
                                { user = user, created = model.currentTime }

                            passiveUsers =
                                Dict.remove username model.passiveUsers

                            activeSessions =
                                Dict.insert sessionId session model.activeSessions
                        in
                        ( { model | passiveUsers = passiveUsers, activeSessions = activeSessions }, Lamdera.sendToFrontend clientId LoginSuccessful )

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
                Just session ->
                    ( { model
                        | activeSessions = Dict.remove sessionId model.activeSessions
                        , passiveUsers = Dict.insert session.user.username session.user model.passiveUsers
                      }
                    , throwOut
                    )

                Nothing ->
                    ( model, throwOut )

        ConsStatistic pastDictation ->
            let
                updateUser user =
                    { user | pastDictations = pastDictation :: user.pastDictations }

                updateSession session =
                    { session | user = updateUser session.user }
            in
            ( { model | activeSessions = Dict.update sessionId (Maybe.map updateSession) model.activeSessions }, Cmd.none )

        GetStatistic ->
            case maybeSession of
                Just session ->
                    ( model, Lamdera.sendToFrontend clientId <| UpdateStatistic session.user.pastDictations )

                Nothing ->
                    ( model, throwOut )

        GetUserCount ->
            let
                count =
                    List.length <| allUsers <| model
            in
            ( model, Lamdera.sendToFrontend clientId <| UpdateUserCount count )

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


allUsers : { a | passiveUsers : Dict Username User, activeSessions : Dict SessionId Session } -> List User
allUsers { passiveUsers, activeSessions } =
    Dict.values passiveUsers ++ List.map .user (Dict.values activeSessions)



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
