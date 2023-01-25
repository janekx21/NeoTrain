module Evergreen.Migrate.V2 exposing (..)

import Dict
import Evergreen.V1.Types as Old
import Evergreen.V2.Types as New
import Lamdera.Migrations exposing (..)
import Task
import Time


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        defaultSettings =
            { blockOnError = New.CorrectLetter, fontSize = 32, paddingLeft = 20, paddingRight = 20, layout = New.Neo }

        new : New.FrontendModel
        new =
            { key = old.key
            , items = old.items |> List.map (\b -> New.Lesson b.title b.content)
            , page = New.LoginAndRegisterPage { username = "", password = "", failed = New.NotAsked, visibility = False }
            , settings = defaultSettings
            , statistic = []
            }
    in
    ModelMigrated ( new, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    let
        migrateLayout : Old.Layout -> New.Layout
        migrateLayout layout =
            case layout of
                Old.Neo ->
                    New.Neo

                Old.NeoQwertz ->
                    New.NeoQwertz

                Old.Bone ->
                    New.Bone

                Old.AdNW ->
                    New.AdNW

                Old.KOY ->
                    New.KOY

                Old.NeoQwerty ->
                    New.NeoQwerty

                Old.Vou ->
                    New.Vou

                Old.Mine ->
                    New.Mine

        migrateSettings : Old.Settings -> New.Settings
        migrateSettings settings =
            { blockOnError =
                case settings.blockOnError of
                    Old.OneBackspace ->
                        New.OneBackspace

                    Old.CorrectLetter ->
                        New.CorrectLetter
            , fontSize = settings.fontSize
            , paddingLeft = settings.paddingLeft
            , paddingRight = settings.paddingRight
            , layout = migrateLayout settings.layout
            }

        migrateUser : Old.User -> New.User
        migrateUser user =
            { username = user.username
            , passwordHash = user.passwordHash
            , passwordSalt = user.passwordSalt
            , settings = migrateSettings user.settings
            , pastDictations = []
            }

        -- session expires
        migrateSession : Old.Session -> New.Session
        migrateSession session =
            { user = migrateUser session.user, created = Time.millisToPosix 0 }

        new : New.BackendModel
        new =
            { currentTime = Time.millisToPosix 0 -- gets set in Cmd
            , passiveUsers = old.passiveUsers |> Dict.map (always migrateUser)
            , activeSessions = old.activeSessions |> Dict.map (always migrateSession)
            , currentSaltIndex = old.currentSaltIndex
            }
    in
    ModelMigrated ( new, Time.now |> Task.perform New.TimeTick )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgOldValueIgnored


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgOldValueIgnored


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgOldValueIgnored


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
