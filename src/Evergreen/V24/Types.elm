module Evergreen.V24.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Evergreen.V24.Translation
import Lamdera
import Time
import Url


type Layout
    = Neo
    | NeoQwertz
    | Bone
    | AdNW
    | KOY
    | NeoQwerty
    | Vou
    | Mine
    | Noted


type alias Lesson =
    { layout : Maybe Layout
    , title : String
    , content : String
    }


type alias Menu =
    { current : Maybe Lesson
    }


type alias Dictation =
    { prev : String
    , current : Char
    , next : String
    }


type alias TypeError =
    { was : Char
    , should : Char
    }


type alias Mods =
    { shift : Bool
    , mod3 : Bool
    , mod4 : Bool
    }


type alias TypingModel =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , mods : Mods
    , lesson : Lesson
    , duration : Float
    , paused : Bool
    , showKeyboard : Bool
    , textOffset : Float
    , textSpeed : Float
    }


type alias PastDictation =
    { errors : List TypeError
    , lesson : Lesson
    , duration : Float
    , finished : Time.Posix
    }


type alias TypingStatisticModel =
    { past : PastDictation
    , allPoints : Maybe (List Int)
    , fromLesson : Bool
    }


type alias Bucket =
    List PastDictation


type alias Hover =
    Bucket


type LoginFail
    = NotAsked
    | WrongUsernameOrPassword
    | UsernameOrPasswordInvalid


type alias AuthModel =
    { username : String
    , password : String
    , passwordVisibility : Bool
    , failed : LoginFail
    }


type Page
    = MenuPage Menu
    | TypingPage TypingModel
    | TypingStatisticPage TypingStatisticModel
    | SettingsPage
        { layer : Int
        }
    | StatisticPage Hover
    | AuthPage AuthModel
    | InfoPage


type WaitingFor
    = OneBackspace
    | CorrectLetter


type ThemeName
    = WheatField
    | ElectricFields
    | CandyLand
    | NeoClassic
    | Dracula


type alias Theme =
    { name : ThemeName
    , dark : Bool
    , rounding : Int
    , borderWidth : Int
    }


type alias Settings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    , theme : Theme
    , language : Evergreen.V24.Translation.Language
    }


type alias AppStatistic =
    { userCount : Int
    , pastDictationCount : Int
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    , authorised : Bool
    , appStatistic : AppStatistic
    }


type alias Username =
    String


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
    , pastDictations : List PastDictation
    }


type alias Session =
    { username : String
    , created : Time.Posix
    }


type alias BackendModel =
    { currentSaltIndex : Int
    , users : Dict.Dict Username User
    , sessions : Dict.Dict Lamdera.SessionId Session
    , currentTime : Time.Posix
    }


type KeyboardKey
    = Character Char
    | Control String


type TypingMsg
    = KeyDown KeyboardKey
    | KeyUp KeyboardKey
    | KeyDownBatch String
    | TickTypingTime
    | AnimationFrameDelta Float
    | Pause
    | Play
    | ToggleKeyboard
    | Exit
    | NoOp


type AuthMsg
    = SetUsername String
    | SetPassword String
    | SetVisibility Bool
    | TryLogin String String
    | TryRegister String String
    | ToInfo
    | WithoutLogin
    | ToggleTranslation


type SettingsMsg
    = SetLayer Int


type PageMsg
    = TypingMsg TypingMsg
    | AuthMsg AuthMsg
    | SettingsMsg SettingsMsg


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | PageMsg PageMsg
    | Back
    | SetSettings Settings
    | OnHover Hover
    | Logout
    | FinishedDictation (List TypeError) Lesson Float Time.Posix
    | ChangePage Page


type ToBackend
    = UpdateSettings Settings
    | GetSettings
    | InsertUser Username String
    | InsertSession Username String
    | GetSession
    | RemoveSession
    | ConsStatistic PastDictation
    | GetStatistic
    | GetAppStatistic
    | GetAllPoints Lesson


type BackendMsg
    = TimeTick Time.Posix


type ToFrontend
    = GotSettings Settings
    | KickOut
    | RegisterFailed
    | LoginSuccessful
    | LoginFailed
    | UpdateStatistic (List PastDictation)
    | UpdateAppStatistic AppStatistic
    | UpdateAllPoints (List Int)
