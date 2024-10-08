module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Element exposing (Device)
import Lamdera exposing (SessionId)
import Time exposing (Posix)
import Translation exposing (Language)
import Url exposing (Url)



-- Model


type alias FrontendModel =
    { key : Key
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    , authorised : Bool
    , appStatistic : AppStatistic
    , device : Device
    }


type alias Menu =
    { current : Maybe Lesson
    }


type alias Lesson =
    { layout : Maybe Layout, title : String, content : String }


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


type alias Mods =
    { shift : Bool
    , mod3 : Bool
    , mod4 : Bool
    }


type Mod
    = Shift
    | Mod3
    | Mod4


type alias TypingStatisticModel =
    { past : PastDictation, allPoints : Maybe (List Int), fromLesson : Bool }


type alias TypeError =
    { was : Char
    , should : Char
    }


type alias Dictation =
    { prev : String
    , current : Char
    , next : String
    }


type alias PastDictation =
    { errors : List TypeError
    , lesson : Lesson
    , duration : Float
    , finished : Posix
    }


type alias PastDictationStat =
    { errors : List TypeError
    , lessonKey : Hash
    , duration : Float
    , finished : Posix
    }


type alias PastDictationBucket =
    List PastDictation


type alias Hover =
    PastDictationBucket


type alias BackendModel =
    { currentSaltIndex : Int
    , users : Dict Username User
    , sessions : Dict SessionId Session
    , currentTime : Posix
    , lessons : Dict Hash Lesson -- this is only for making the model smaller aka caching
    }


type alias Hash =
    String


type alias Session =
    { username : String, created : Posix }


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
    , pastDictationStats : List PastDictationStat
    }


type alias Username =
    String


type alias Settings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    , theme : Theme
    , language : Language
    }


type MonoFont
    = PTMono
    | RobotoMono
    | UbuntuSansMono
    | JetBrainsMono
    | IbmPlexMono


type alias AppStatistic =
    { userCount : Int
    , pastDictationCount : Int
    , pastDictationCurve : List ( Posix, Int )
    }



-- Messages


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PageMsg PageMsg
    | Back
    | SetSettings Settings
    | OnHover Hover
    | Logout
    | FinishedDictation (List TypeError) Lesson Float Posix
    | ChangePage Page
    | Resize Int Int


type PageMsg
    = TypingMsg TypingMsg
    | AuthMsg AuthMsg
    | SettingsMsg SettingsMsg


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


type BackendMsg
    = TimeTick Posix


type ToBackend
    = UpdateSettings Settings
    | GetSettings
    | InsertUser Username String
      -- | todo RemoveUser
    | InsertSession Username String
    | GetSession
    | RemoveSession
    | ConsStatistic PastDictation
    | GetStatistic
    | GetAppStatistic
    | GetAllPoints Lesson


type ToFrontend
    = GotSettings Settings
    | KickOut
    | RegisterFailed
    | LoginSuccessful
    | LoginFailed
    | UpdateStatistic (List PastDictation)
    | UpdateAppStatistic AppStatistic
    | UpdateAllPoints (List Int)



-- Types


type Page
    = MenuPage Menu
    | TypingPage TypingModel
    | TypingStatisticPage TypingStatisticModel
    | SettingsPage { layer : Int }
    | StatisticPage Hover
    | AuthPage AuthModel
    | InfoPage


type alias AuthModel =
    { username : String
    , password : String
    , passwordVisibility : Bool
    , failed : LoginFail
    }


type AuthResult
    = JustModel AuthModel
    | ToInfoPage
    | ToMenuPage


type LoginFail
    = NotAsked
    | WrongUsernameOrPassword
    | UsernameOrPasswordInvalid


type WaitingFor
    = OneBackspace
    | CorrectLetter


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


type KeyboardKey
    = Character Char
    | Control String


type alias Theme =
    { name : ThemeName, dark : Bool, rounding : Int, borderWidth : Int, monoFont : MonoFont }


type ThemeName
    = WheatField
    | ElectricFields
    | CandyLand
    | NeoClassic
    | Dracula
    | Contrast


type NamedColor
    = Primary
    | Secondary
    | White
    | Black
