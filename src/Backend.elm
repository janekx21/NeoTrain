module Backend exposing (..)

import Dict
import Lamdera exposing (ClientId, SessionId)
import Sha256 exposing (sha256)
import Types exposing (..)


type alias Model =
    BackendModel



--noinspection ElmUnusedSymbol


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { currentSaltIndex = 0, activeSessions = Dict.empty, passiveUsers = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        maybeSession =
            Dict.get sessionId model.activeSessions

        throwOut =
            Lamdera.sendToFrontend clientId ThrowOut
    in
    case msg of
        UpdateSettings settings ->
            let
                mapSession { user } =
                    { user = { user | settings = settings } }
            in
            ( { model | activeSessions = Dict.update sessionId (Maybe.map mapSession) model.activeSessions }, Cmd.none )

        GetSettings ->
            case maybeSession of
                Just session ->
                    ( model, Lamdera.sendToFrontend clientId <| GotSettings session.user.settings )

                Nothing ->
                    ( model, throwOut )

        Register username password ->
            let
                salt =
                    String.fromInt model.currentSaltIndex

                hash =
                    sha256 (salt ++ password)

                user : User
                user =
                    { username = username, passwordHash = hash, passwordSalt = salt, settings = defaultSettings }
            in
            ( { model | passiveUsers = Dict.insert username user model.passiveUsers, currentSaltIndex = model.currentSaltIndex + 1 }
            , Lamdera.sendToFrontend clientId LoginSuccessful
            )

        Login username password ->
            case Dict.get username model.passiveUsers of
                Just user ->
                    let
                        hash =
                            sha256 (user.passwordSalt ++ password)
                    in
                    if hash == user.passwordHash then
                        let
                            passiveUsers =
                                Dict.remove username model.passiveUsers

                            activeSessions =
                                Dict.insert sessionId { user = user } model.activeSessions
                        in
                        ( { model | passiveUsers = passiveUsers, activeSessions = activeSessions }, Lamdera.sendToFrontend clientId LoginSuccessful )

                    else
                        ( model, Lamdera.sendToFrontend clientId LoginFailed )

                Nothing ->
                    ( model, Lamdera.sendToFrontend clientId LoginFailed )

        SessionCheck ->
            if maybeSession == Nothing then
                ( model, Lamdera.sendToFrontend clientId LoginFailed )

            else
                ( model, Lamdera.sendToFrontend clientId LoginSuccessful )

        BackendLogout ->
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


needsLogin msg =
    case msg of
        UpdateSettings _ ->
            True

        GetSettings ->
            True

        Register _ _ ->
            False

        Login _ _ ->
            False

        SessionCheck ->
            True

        BackendLogout ->
            True
