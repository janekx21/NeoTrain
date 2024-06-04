module Evergreen.Migrate.V28 exposing (..)

{-| This migration file was automatically generated by the lamdera compiler.

It includes:

  - A migration for each of the 6 Lamdera core types that has changed
  - A function named `migrate_ModuleName_TypeName` for each changed/custom type

Expect to see:

  - `Unimplementеd` values as placeholders wherever I was unable to figure out a clear migration path for you
  - `@NOTICE` comments for things you should know about, i.e. new custom type constructors that won't get any
    value mappings from the old type by default

You can edit this file however you wish! It won't be generated again.

See <https://dashboard.lamdera.app/docs/evergreen> for more info.

-}

import Dict exposing (Dict)
import Evergreen.V27.Translation
import Evergreen.V27.Types
import Evergreen.V28.Translation
import Evergreen.V28.Types
import Lamdera.Migrations exposing (..)
import Sha256 exposing (sha256)


frontendModel : Evergreen.V27.Types.FrontendModel -> ModelMigration Evergreen.V28.Types.FrontendModel Evergreen.V28.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V27.Types.BackendModel -> ModelMigration Evergreen.V28.Types.BackendModel Evergreen.V28.Types.BackendMsg
backendModel old =
    ModelMigrated ( migrate_Types_BackendModel old, Cmd.none )


frontendMsg : Evergreen.V27.Types.FrontendMsg -> MsgMigration Evergreen.V28.Types.FrontendMsg Evergreen.V28.Types.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Evergreen.V27.Types.ToBackend -> MsgMigration Evergreen.V28.Types.ToBackend Evergreen.V28.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V27.Types.BackendMsg -> MsgMigration Evergreen.V28.Types.BackendMsg Evergreen.V28.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V27.Types.ToFrontend -> MsgMigration Evergreen.V28.Types.ToFrontend Evergreen.V28.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Types_BackendModel : Evergreen.V27.Types.BackendModel -> Evergreen.V28.Types.BackendModel
migrate_Types_BackendModel old =
    let
        allLessons : List Evergreen.V28.Types.Lesson
        allLessons =
            old.users |> Dict.values |> List.concatMap (\u -> u.pastDictations) |> List.map (\p -> p.lesson) |> List.map (\l -> { title = l.title, content = l.content, layout = Maybe.map migrate_Types_Layout l.layout })

        lessons =
            List.foldl (\acc dict -> findOrInsertLesson dict acc |> Tuple.second) Dict.empty allLessons
    in
    { currentSaltIndex = old.currentSaltIndex
    , users = old.users |> Dict.map (\k -> migrate_Types_User)
    , sessions = old.sessions
    , currentTime = old.currentTime
    , lessons = lessons
    }


findOrInsertLesson : Dict Evergreen.V28.Types.Hash Evergreen.V28.Types.Lesson -> Evergreen.V28.Types.Lesson -> ( Evergreen.V28.Types.Hash, Dict Evergreen.V28.Types.Hash Evergreen.V28.Types.Lesson )
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


layoutNames layout =
    case layout of
        Evergreen.V28.Types.Neo ->
            "Neo"

        Evergreen.V28.Types.Bone ->
            "Bone"

        Evergreen.V28.Types.NeoQwertz ->
            "NeoQwertz"

        Evergreen.V28.Types.AdNW ->
            "AdNW"

        Evergreen.V28.Types.KOY ->
            "KOY"

        Evergreen.V28.Types.NeoQwerty ->
            "NeoQwerty"

        Evergreen.V28.Types.Vou ->
            "Vou"

        Evergreen.V28.Types.Mine ->
            "Mine"

        Evergreen.V28.Types.Noted ->
            "Noted"


migrate_Translation_Language : Evergreen.V27.Translation.Language -> Evergreen.V28.Translation.Language
migrate_Translation_Language old =
    case old of
        Evergreen.V27.Translation.German ->
            Evergreen.V28.Translation.German

        Evergreen.V27.Translation.English ->
            Evergreen.V28.Translation.English


migrate_Types_Layout : Evergreen.V27.Types.Layout -> Evergreen.V28.Types.Layout
migrate_Types_Layout old =
    case old of
        Evergreen.V27.Types.Neo ->
            Evergreen.V28.Types.Neo

        Evergreen.V27.Types.NeoQwertz ->
            Evergreen.V28.Types.NeoQwertz

        Evergreen.V27.Types.Bone ->
            Evergreen.V28.Types.Bone

        Evergreen.V27.Types.AdNW ->
            Evergreen.V28.Types.AdNW

        Evergreen.V27.Types.KOY ->
            Evergreen.V28.Types.KOY

        Evergreen.V27.Types.NeoQwerty ->
            Evergreen.V28.Types.NeoQwerty

        Evergreen.V27.Types.Vou ->
            Evergreen.V28.Types.Vou

        Evergreen.V27.Types.Mine ->
            Evergreen.V28.Types.Mine

        Evergreen.V27.Types.Noted ->
            Evergreen.V28.Types.Noted


migrate_Types_MonoFont : Evergreen.V27.Types.MonoFont -> Evergreen.V28.Types.MonoFont
migrate_Types_MonoFont old =
    case old of
        Evergreen.V27.Types.PTMono ->
            Evergreen.V28.Types.PTMono

        Evergreen.V27.Types.RobotoMono ->
            Evergreen.V28.Types.RobotoMono

        Evergreen.V27.Types.UbuntuSansMono ->
            Evergreen.V28.Types.UbuntuSansMono

        Evergreen.V27.Types.JetBrainsMono ->
            Evergreen.V28.Types.JetBrainsMono

        Evergreen.V27.Types.IbmPlexMono ->
            Evergreen.V28.Types.IbmPlexMono


migrate_Types_Settings : Evergreen.V27.Types.Settings -> Evergreen.V28.Types.Settings
migrate_Types_Settings old =
    { blockOnError = old.blockOnError |> migrate_Types_WaitingFor
    , fontSize = old.fontSize
    , paddingLeft = old.paddingLeft
    , paddingRight = old.paddingRight
    , layout = old.layout |> migrate_Types_Layout
    , theme = old.theme |> migrate_Types_Theme
    , language = old.language |> migrate_Translation_Language
    }


migrate_Types_Theme : Evergreen.V27.Types.Theme -> Evergreen.V28.Types.Theme
migrate_Types_Theme old =
    { name = old.name |> migrate_Types_ThemeName
    , dark = old.dark
    , rounding = old.rounding
    , borderWidth = old.borderWidth
    , monoFont = old.monoFont |> migrate_Types_MonoFont
    }


migrate_Types_ThemeName : Evergreen.V27.Types.ThemeName -> Evergreen.V28.Types.ThemeName
migrate_Types_ThemeName old =
    case old of
        Evergreen.V27.Types.WheatField ->
            Evergreen.V28.Types.WheatField

        Evergreen.V27.Types.ElectricFields ->
            Evergreen.V28.Types.ElectricFields

        Evergreen.V27.Types.CandyLand ->
            Evergreen.V28.Types.CandyLand

        Evergreen.V27.Types.NeoClassic ->
            Evergreen.V28.Types.NeoClassic

        Evergreen.V27.Types.Dracula ->
            Evergreen.V28.Types.Dracula

        Evergreen.V27.Types.Contrast ->
            Evergreen.V28.Types.Contrast


migrate_Types_User : Evergreen.V27.Types.User -> Evergreen.V28.Types.User
migrate_Types_User old =
    let
        migrateP : Evergreen.V27.Types.PastDictation -> Evergreen.V28.Types.PastDictationStat
        migrateP p =
            { duration = p.duration
            , errors = p.errors
            , finished = p.finished
            , lessonKey = hashLesson { title = p.lesson.title, content = p.lesson.content, layout = Maybe.map migrate_Types_Layout p.lesson.layout }
            }

        pastDictationStats : List Evergreen.V28.Types.PastDictationStat
        pastDictationStats =
            old.pastDictations
                |> List.map
                    migrateP
    in
    { username = old.username
    , passwordHash = old.passwordHash
    , passwordSalt = old.passwordSalt
    , settings = old.settings |> migrate_Types_Settings
    , pastDictationStats = pastDictationStats
    }


migrate_Types_WaitingFor : Evergreen.V27.Types.WaitingFor -> Evergreen.V28.Types.WaitingFor
migrate_Types_WaitingFor old =
    case old of
        Evergreen.V27.Types.OneBackspace ->
            Evergreen.V28.Types.OneBackspace

        Evergreen.V27.Types.CorrectLetter ->
            Evergreen.V28.Types.CorrectLetter
