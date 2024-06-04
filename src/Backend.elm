module Backend exposing (..)

import Common exposing (bucketStatistic, bucketStatisticDaily, defaultSettings, globalDictationCurveInterval, layoutNames, points)
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
    ( { currentSaltIndex = 0, sessions = Dict.empty, users = Dict.empty, currentTime = Time.millisToPosix 0, lessons = Dict.empty }
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
                    , pastDictationStats = []
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
                        ( lessons, pastStat ) =
                            createDictationStat model.lessons pastDictation

                        updateUser user =
                            { user | pastDictationStats = pastStat :: user.pastDictationStats }
                    in
                    ( { model | users = Dict.update username (Maybe.map updateUser) model.users, lessons = lessons }, Cmd.none )

                Nothing ->
                    ( model, throwOut )

        GetStatistic ->
            case maybeUser of
                Just user ->
                    ( model
                    , user.pastDictationStats
                        |> List.filterMap (resolveDictation model.lessons)
                        |> UpdateStatistic
                        |> Lamdera.sendToFrontend clientId
                    )

                Nothing ->
                    ( model, throwOut )

        GetAppStatistic ->
            let
                userCount =
                    List.length <| allUsers <| model

                pastDictations =
                    model
                        |> allUsers
                        |> List.concatMap .pastDictationStats

                pastDictationCount =
                    pastDictations |> List.length

                pastDictationCurve =
                    pastDictations
                        |> bucketStatistic globalDictationCurveInterval
                        |> Dict.toList
                        |> List.map
                            (\( k, v ) ->
                                ( Time.millisToPosix (k * globalDictationCurveInterval), List.length v )
                            )

                statistic : AppStatistic
                statistic =
                    { userCount = userCount
                    , pastDictationCount = pastDictationCount
                    , pastDictationCurve = pastDictationCurve
                    }
            in
            ( model, Lamdera.sendToFrontend clientId <| UpdateAppStatistic statistic )

        GetAllPoints lesson ->
            let
                allPoints : List Int
                allPoints =
                    model
                        |> allUsers
                        |> List.concatMap .pastDictationStats
                        |> List.filterMap (resolveDictation model.lessons)
                        |> List.filter (\p -> p.lesson == lesson)
                        |> List.map points
            in
            ( model, Lamdera.sendToFrontend clientId <| UpdateAllPoints allPoints )


resolveDictation : Dict Hash Lesson -> PastDictationStat -> Maybe PastDictation
resolveDictation lessons { duration, errors, finished, lessonKey } =
    lessons
        |> Dict.get lessonKey
        |> Maybe.map
            (\lesson ->
                { duration = duration, errors = errors, finished = finished, lesson = lesson }
            )


createDictationStat : Dict Hash Lesson -> PastDictation -> ( Dict Hash Lesson, PastDictationStat )
createDictationStat lessons_ { lesson, duration, errors, finished } =
    let
        ( lessonKey, lessons ) =
            findOrInsertLesson lessons_ lesson

        pastDictationStat : PastDictationStat
        pastDictationStat =
            { duration = duration, errors = errors, finished = finished, lessonKey = lessonKey }
    in
    ( lessons, pastDictationStat )


allUsers : { a | users : Dict Username User } -> List User
allUsers { users } =
    Dict.values users


findOrInsertLesson : Dict Hash Lesson -> Lesson -> ( Hash, Dict Hash Lesson )
findOrInsertLesson dict lesson =
    let
        hash =
            hashLesson lesson

        updatedDict =
            dict |> Dict.insert hash lesson
    in
    ( hash, updatedDict )


hashLesson { title, content, layout } =
    ("lesson"
        ++ title
        ++ content
        ++ (layout |> Maybe.map layoutNames |> Maybe.withDefault "")
    )
        |> sha256
