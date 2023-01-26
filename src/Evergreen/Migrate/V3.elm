module Evergreen.Migrate.V3 exposing (..)

import Dict
import Evergreen.V2.Types as Old
import Evergreen.V3.Types as New
import Lamdera.Migrations exposing (..)


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
    , theme = New.ElectricFields
    }


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        model : New.FrontendModel
        model =
            { settings = migrateSettings old.settings
            , page = New.AuthPage { username = "", failed = New.NotAsked, password = "", passwordVisibility = False }
            , authorised = False
            , key = old.key
            , statistic = old.statistic
            }
    in
    ModelMigrated ( model, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    let
        migrateUser : Old.User -> New.User
        migrateUser user =
            { username = user.username
            , passwordHash = user.passwordHash
            , passwordSalt = user.passwordSalt
            , settings = migrateSettings user.settings
            , pastDictations = user.pastDictations
            }

        -- session expires
        migrateSession : Old.Session -> New.Session
        migrateSession session =
            { user = migrateUser session.user, created = session.created }

        model : Old.BackendModel -> New.BackendModel
        model o =
            { currentTime = o.currentTime
            , passiveUsers = o.passiveUsers |> Dict.map (always migrateUser)
            , activeSessions = o.activeSessions |> Dict.map (always migrateSession)
            , currentSaltIndex = o.currentSaltIndex
            }
    in
    ModelMigrated ( model old, Cmd.none )


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
