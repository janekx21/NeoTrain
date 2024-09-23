module Evergreen.V33.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Element
import Evergreen.V33.Translation
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


type alias PastDictationBucket =
    List PastDictation


type alias Hover =
    PastDictationBucket


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
    | Contrast


type MonoFont
    = PTMono
    | RobotoMono
    | UbuntuSansMono
    | JetBrainsMono
    | IbmPlexMono


type alias Theme =
    { name : ThemeName
    , dark : Bool
    , rounding : Int
    , borderWidth : Int
    , monoFont : MonoFont
    }


type alias Settings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    , theme : Theme
    , language : Evergreen.V33.Translation.Language
    }


type alias AppStatistic =
    { userCount : Int
    , pastDictationCount : Int
    , pastDictationCurve : List ( Time.Posix, Int )
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    , authorised : Bool
    , appStatistic : AppStatistic
    , device : Element.Device
    }


type alias Username =
    String


type alias Hash =
    String


type alias PastDictationStat =
    { errors : List TypeError
    , lessonKey : Hash
    , duration : Float
    , finished : Time.Posix
    }


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
    , pastDictationStats : List PastDictationStat
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
    , lessons : Dict.Dict Hash Lesson
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
    | Restart
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
    | Resize Int Int


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
