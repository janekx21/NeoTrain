module Evergreen.Migrate.V7 exposing (..)

import Dict
import Evergreen.V6.Types as Old
import Evergreen.V7.Types as New
import Lamdera.Migrations exposing (..)


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


migrateTheme : Old.ThemeName -> New.ThemeName
migrateTheme theme =
    case theme of
        Old.WheatField ->
            New.WheatField

        Old.ElectricFields ->
            New.ElectricFields

        Old.CandyLand ->
            New.CandyLand


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
    , theme = New.Theme (migrateTheme settings.theme.name) settings.theme.dark
    }


defaultAuth : New.AuthModel
defaultAuth =
    { username = "", failed = New.NotAsked, password = "", passwordVisibility = False }


migrateStatistic : Old.PastDictation -> New.PastDictation
migrateStatistic old =
    { errors = old.errors
    , lesson = old.lesson
    , duration = old.duration
    , finished = old.finished
    }


migrateFrontend : Old.FrontendModel -> New.FrontendModel
migrateFrontend old =
    { key = old.key
    , page = New.AuthPage defaultAuth
    , usersCount2 = 1
    , settings = migrateSettings old.settings
    , statistic = List.map migrateStatistic old.statistic
    , authorised = old.authorised
    }


migrateUser : Old.User -> New.User
migrateUser user =
    { username = user.username
    , passwordHash = user.passwordHash
    , passwordSalt = user.passwordSalt
    , settings = migrateSettings user.settings
    , pastDictations = user.pastDictations
    }


migrateSession : Old.Session -> New.Session
migrateSession session =
    { user = migrateUser session.user, created = session.created }


migrateBackend : Old.BackendModel -> New.BackendModel
migrateBackend o =
    { currentTime = o.currentTime
    , passiveUsers = o.passiveUsers |> Dict.map (always migrateUser)
    , activeSessions = o.activeSessions |> Dict.map (always migrateSession)
    , currentSaltIndex = o.currentSaltIndex
    }


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated ( migrateFrontend old, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated ( migrateBackend old, Cmd.none )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgOldValueIgnored


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgOldValueIgnored


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgOldValueIgnored
