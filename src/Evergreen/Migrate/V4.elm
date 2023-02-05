module Evergreen.Migrate.V4 exposing (..)

import Common exposing (defaultAuth)
import Evergreen.V3.Types as Old
import Evergreen.V4.Types as New
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


defaultAuth : New.AuthModel
defaultAuth =
    { username = "", failed = New.NotAsked, password = "", passwordVisibility = False }


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        new : New.FrontendModel
        new =
            { key = old.key
            , page = New.AuthPage defaultAuth
            , settings = migrateSettings old.settings
            , statistic = old.statistic
            , authorised = old.authorised
            , userCount = 1
            }
    in
    ModelMigrated ( new, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


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
