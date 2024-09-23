module Evergreen.Migrate.V25 exposing (..)

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

import Evergreen.V24.Translation
import Evergreen.V24.Types
import Evergreen.V25.Translation
import Evergreen.V25.Types
import Lamdera.Migrations exposing (..)
import List
import Maybe


frontendModel : Evergreen.V24.Types.FrontendModel -> ModelMigration Evergreen.V25.Types.FrontendModel Evergreen.V25.Types.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Evergreen.V24.Types.BackendModel -> ModelMigration Evergreen.V25.Types.BackendModel Evergreen.V25.Types.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Evergreen.V24.Types.FrontendMsg -> MsgMigration Evergreen.V25.Types.FrontendMsg Evergreen.V25.Types.FrontendMsg
frontendMsg old =
    MsgMigrated ( migrate_Types_FrontendMsg old, Cmd.none )


toBackend : Evergreen.V24.Types.ToBackend -> MsgMigration Evergreen.V25.Types.ToBackend Evergreen.V25.Types.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Evergreen.V24.Types.BackendMsg -> MsgMigration Evergreen.V25.Types.BackendMsg Evergreen.V25.Types.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Evergreen.V24.Types.ToFrontend -> MsgMigration Evergreen.V25.Types.ToFrontend Evergreen.V25.Types.FrontendMsg
toFrontend old =
    MsgUnchanged


migrate_Translation_Language : Evergreen.V24.Translation.Language -> Evergreen.V25.Translation.Language
migrate_Translation_Language old =
    case old of
        Evergreen.V24.Translation.German ->
            Evergreen.V25.Translation.German

        Evergreen.V24.Translation.English ->
            Evergreen.V25.Translation.English


migrate_Types_AuthModel : Evergreen.V24.Types.AuthModel -> Evergreen.V25.Types.AuthModel
migrate_Types_AuthModel old =
    { username = old.username
    , password = old.password
    , passwordVisibility = old.passwordVisibility
    , failed = old.failed |> migrate_Types_LoginFail
    }


migrate_Types_AuthMsg : Evergreen.V24.Types.AuthMsg -> Evergreen.V25.Types.AuthMsg
migrate_Types_AuthMsg old =
    case old of
        Evergreen.V24.Types.SetUsername p0 ->
            Evergreen.V25.Types.SetUsername p0

        Evergreen.V24.Types.SetPassword p0 ->
            Evergreen.V25.Types.SetPassword p0

        Evergreen.V24.Types.SetVisibility p0 ->
            Evergreen.V25.Types.SetVisibility p0

        Evergreen.V24.Types.TryLogin p0 p1 ->
            Evergreen.V25.Types.TryLogin p0 p1

        Evergreen.V24.Types.TryRegister p0 p1 ->
            Evergreen.V25.Types.TryRegister p0 p1

        Evergreen.V24.Types.ToInfo ->
            Evergreen.V25.Types.ToInfo

        Evergreen.V24.Types.WithoutLogin ->
            Evergreen.V25.Types.WithoutLogin

        Evergreen.V24.Types.ToggleTranslation ->
            Evergreen.V25.Types.ToggleTranslation


migrate_Types_Bucket : Evergreen.V24.Types.Bucket -> Evergreen.V25.Types.Bucket
migrate_Types_Bucket old =
    old |> List.map migrate_Types_PastDictation


migrate_Types_Dictation : Evergreen.V24.Types.Dictation -> Evergreen.V25.Types.Dictation
migrate_Types_Dictation old =
    old


migrate_Types_FrontendMsg : Evergreen.V24.Types.FrontendMsg -> Evergreen.V25.Types.FrontendMsg
migrate_Types_FrontendMsg old =
    case old of
        Evergreen.V24.Types.UrlClicked p0 ->
            Evergreen.V25.Types.UrlClicked p0

        Evergreen.V24.Types.UrlChanged p0 ->
            Evergreen.V25.Types.UrlChanged p0

        Evergreen.V24.Types.NoOpFrontendMsg ->
            Evergreen.V25.Types.NoOpFrontendMsg

        Evergreen.V24.Types.PageMsg p0 ->
            Evergreen.V25.Types.PageMsg (p0 |> migrate_Types_PageMsg)

        Evergreen.V24.Types.Back ->
            Evergreen.V25.Types.Back

        Evergreen.V24.Types.SetSettings p0 ->
            Evergreen.V25.Types.SetSettings (p0 |> migrate_Types_Settings)

        Evergreen.V24.Types.OnHover p0 ->
            Evergreen.V25.Types.OnHover (p0 |> migrate_Types_Hover)

        Evergreen.V24.Types.Logout ->
            Evergreen.V25.Types.Logout

        Evergreen.V24.Types.FinishedDictation p0 p1 p2 p3 ->
            Evergreen.V25.Types.FinishedDictation p0 (p1 |> migrate_Types_Lesson) p2 p3

        Evergreen.V24.Types.ChangePage p0 ->
            Evergreen.V25.Types.ChangePage (p0 |> migrate_Types_Page)


migrate_Types_Hover : Evergreen.V24.Types.Hover -> Evergreen.V25.Types.Hover
migrate_Types_Hover old =
    old |> migrate_Types_Bucket


migrate_Types_KeyboardKey : Evergreen.V24.Types.KeyboardKey -> Evergreen.V25.Types.KeyboardKey
migrate_Types_KeyboardKey old =
    case old of
        Evergreen.V24.Types.Character p0 ->
            Evergreen.V25.Types.Character p0

        Evergreen.V24.Types.Control p0 ->
            Evergreen.V25.Types.Control p0


migrate_Types_Layout : Evergreen.V24.Types.Layout -> Evergreen.V25.Types.Layout
migrate_Types_Layout old =
    case old of
        Evergreen.V24.Types.Neo ->
            Evergreen.V25.Types.Neo

        Evergreen.V24.Types.NeoQwertz ->
            Evergreen.V25.Types.NeoQwertz

        Evergreen.V24.Types.Bone ->
            Evergreen.V25.Types.Bone

        Evergreen.V24.Types.AdNW ->
            Evergreen.V25.Types.AdNW

        Evergreen.V24.Types.KOY ->
            Evergreen.V25.Types.KOY

        Evergreen.V24.Types.NeoQwerty ->
            Evergreen.V25.Types.NeoQwerty

        Evergreen.V24.Types.Vou ->
            Evergreen.V25.Types.Vou

        Evergreen.V24.Types.Mine ->
            Evergreen.V25.Types.Mine

        Evergreen.V24.Types.Noted ->
            Evergreen.V25.Types.Noted


migrate_Types_Lesson : Evergreen.V24.Types.Lesson -> Evergreen.V25.Types.Lesson
migrate_Types_Lesson old =
    { layout = old.layout |> Maybe.map migrate_Types_Layout
    , title = old.title
    , content = old.content
    }


migrate_Types_LoginFail : Evergreen.V24.Types.LoginFail -> Evergreen.V25.Types.LoginFail
migrate_Types_LoginFail old =
    case old of
        Evergreen.V24.Types.NotAsked ->
            Evergreen.V25.Types.NotAsked

        Evergreen.V24.Types.WrongUsernameOrPassword ->
            Evergreen.V25.Types.WrongUsernameOrPassword

        Evergreen.V24.Types.UsernameOrPasswordInvalid ->
            Evergreen.V25.Types.UsernameOrPasswordInvalid


migrate_Types_Menu : Evergreen.V24.Types.Menu -> Evergreen.V25.Types.Menu
migrate_Types_Menu old =
    { current = old.current |> Maybe.map migrate_Types_Lesson
    }


migrate_Types_Mods : Evergreen.V24.Types.Mods -> Evergreen.V25.Types.Mods
migrate_Types_Mods old =
    old


migrate_Types_Page : Evergreen.V24.Types.Page -> Evergreen.V25.Types.Page
migrate_Types_Page old =
    case old of
        Evergreen.V24.Types.MenuPage p0 ->
            Evergreen.V25.Types.MenuPage (p0 |> migrate_Types_Menu)

        Evergreen.V24.Types.TypingPage p0 ->
            Evergreen.V25.Types.TypingPage (p0 |> migrate_Types_TypingModel)

        Evergreen.V24.Types.TypingStatisticPage p0 ->
            Evergreen.V25.Types.TypingStatisticPage (p0 |> migrate_Types_TypingStatisticModel)

        Evergreen.V24.Types.SettingsPage p0 ->
            Evergreen.V25.Types.SettingsPage p0

        Evergreen.V24.Types.StatisticPage p0 ->
            Evergreen.V25.Types.StatisticPage (p0 |> migrate_Types_Hover)

        Evergreen.V24.Types.AuthPage p0 ->
            Evergreen.V25.Types.AuthPage (p0 |> migrate_Types_AuthModel)

        Evergreen.V24.Types.InfoPage ->
            Evergreen.V25.Types.InfoPage


migrate_Types_PageMsg : Evergreen.V24.Types.PageMsg -> Evergreen.V25.Types.PageMsg
migrate_Types_PageMsg old =
    case old of
        Evergreen.V24.Types.TypingMsg p0 ->
            Evergreen.V25.Types.TypingMsg (p0 |> migrate_Types_TypingMsg)

        Evergreen.V24.Types.AuthMsg p0 ->
            Evergreen.V25.Types.AuthMsg (p0 |> migrate_Types_AuthMsg)

        Evergreen.V24.Types.SettingsMsg p0 ->
            Evergreen.V25.Types.SettingsMsg (p0 |> migrate_Types_SettingsMsg)


migrate_Types_PastDictation : Evergreen.V24.Types.PastDictation -> Evergreen.V25.Types.PastDictation
migrate_Types_PastDictation old =
    { errors = old.errors
    , lesson = old.lesson |> migrate_Types_Lesson
    , duration = old.duration
    , finished = old.finished
    }


migrate_Types_Settings : Evergreen.V24.Types.Settings -> Evergreen.V25.Types.Settings
migrate_Types_Settings old =
    { blockOnError = old.blockOnError |> migrate_Types_WaitingFor
    , fontSize = old.fontSize
    , paddingLeft = old.paddingLeft
    , paddingRight = old.paddingRight
    , layout = old.layout |> migrate_Types_Layout
    , theme = old.theme |> migrate_Types_Theme
    , language = old.language |> migrate_Translation_Language
    }


migrate_Types_SettingsMsg : Evergreen.V24.Types.SettingsMsg -> Evergreen.V25.Types.SettingsMsg
migrate_Types_SettingsMsg old =
    case old of
        Evergreen.V24.Types.SetLayer p0 ->
            Evergreen.V25.Types.SetLayer p0


migrate_Types_Theme : Evergreen.V24.Types.Theme -> Evergreen.V25.Types.Theme
migrate_Types_Theme old =
    { name = old.name |> migrate_Types_ThemeName
    , dark = old.dark
    , rounding = old.rounding
    , borderWidth = old.borderWidth
    }


migrate_Types_ThemeName : Evergreen.V24.Types.ThemeName -> Evergreen.V25.Types.ThemeName
migrate_Types_ThemeName old =
    case old of
        Evergreen.V24.Types.WheatField ->
            Evergreen.V25.Types.WheatField

        Evergreen.V24.Types.ElectricFields ->
            Evergreen.V25.Types.ElectricFields

        Evergreen.V24.Types.CandyLand ->
            Evergreen.V25.Types.CandyLand

        Evergreen.V24.Types.NeoClassic ->
            Evergreen.V25.Types.NeoClassic

        Evergreen.V24.Types.Dracula ->
            Evergreen.V25.Types.Dracula


migrate_Types_TypingModel : Evergreen.V24.Types.TypingModel -> Evergreen.V25.Types.TypingModel
migrate_Types_TypingModel old =
    { dictation = old.dictation |> migrate_Types_Dictation
    , madeError = old.madeError
    , errors = old.errors
    , mods = old.mods |> migrate_Types_Mods
    , lesson = old.lesson |> migrate_Types_Lesson
    , duration = old.duration
    , paused = old.paused
    , showKeyboard = old.showKeyboard
    , textOffset = old.textOffset
    , textSpeed = old.textSpeed
    }


migrate_Types_TypingMsg : Evergreen.V24.Types.TypingMsg -> Evergreen.V25.Types.TypingMsg
migrate_Types_TypingMsg old =
    case old of
        Evergreen.V24.Types.KeyDown p0 ->
            Evergreen.V25.Types.KeyDown (p0 |> migrate_Types_KeyboardKey)

        Evergreen.V24.Types.KeyUp p0 ->
            Evergreen.V25.Types.KeyUp (p0 |> migrate_Types_KeyboardKey)

        Evergreen.V24.Types.KeyDownBatch p0 ->
            Evergreen.V25.Types.KeyDownBatch p0

        Evergreen.V24.Types.TickTypingTime ->
            Evergreen.V25.Types.TickTypingTime

        Evergreen.V24.Types.AnimationFrameDelta p0 ->
            Evergreen.V25.Types.AnimationFrameDelta p0

        Evergreen.V24.Types.Pause ->
            Evergreen.V25.Types.Pause

        Evergreen.V24.Types.Play ->
            Evergreen.V25.Types.Play

        Evergreen.V24.Types.ToggleKeyboard ->
            Evergreen.V25.Types.ToggleKeyboard

        Evergreen.V24.Types.Exit ->
            Evergreen.V25.Types.Exit

        Evergreen.V24.Types.NoOp ->
            Evergreen.V25.Types.NoOp


migrate_Types_TypingStatisticModel : Evergreen.V24.Types.TypingStatisticModel -> Evergreen.V25.Types.TypingStatisticModel
migrate_Types_TypingStatisticModel old =
    { past = old.past |> migrate_Types_PastDictation
    , allPoints = old.allPoints
    , fromLesson = old.fromLesson
    }


migrate_Types_WaitingFor : Evergreen.V24.Types.WaitingFor -> Evergreen.V25.Types.WaitingFor
migrate_Types_WaitingFor old =
    case old of
        Evergreen.V24.Types.OneBackspace ->
            Evergreen.V25.Types.OneBackspace

        Evergreen.V24.Types.CorrectLetter ->
            Evergreen.V25.Types.CorrectLetter