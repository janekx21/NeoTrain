module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (SessionId)
import Time exposing (Posix)
import Url exposing (Url)



-- Model


type alias FrontendModel =
    { key : Key
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    , authorised : Bool
    , usersCount : Int
    }


type alias Menu =
    { current : Maybe Lesson
    }


type alias Lesson =
    { title : String, content : String }


type alias TypingModel =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , layer : Int
    , lesson : Lesson
    , duration : Float
    , paused : Bool
    , showKeyboard : Bool
    }


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


type alias Bucket =
    List PastDictation


type alias BackendModel =
    { currentSaltIndex : Int
    , passiveUsers : Dict Username User
    , activeSessions : Dict SessionId Session
    , currentTime : Posix
    }


type alias Session =
    { user : User, created : Posix }


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
    , pastDictations : List PastDictation
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


type PageMsg
    = TypingMsg TypingMsg
    | AuthMsg AuthMsg
    | SettingsMsg SettingsMsg


type TypingMsg
    = KeyDown KeyboardKey
    | KeyUp KeyboardKey
    | TickTypingTime
    | Pause
    | Play
    | ToggleKeyboard
    | Exit


type AuthMsg
    = SetUsername String
    | SetPassword String
    | SetVisibility Bool
    | TryLogin String String
    | TryRegister String String
    | ToInfo
    | WithoutLogin


type SettingsMsg
    = SetLayer Int


type alias Hover =
    Bucket


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
    | GetUserCount


type ToFrontend
    = GotSettings Settings
    | KickOut
    | RegisterFailed
    | LoginSuccessful
    | LoginFailed
    | UpdateStatistic (List PastDictation)
    | UpdateUserCount Int



-- Types


type Page
    = MenuPage Menu
    | TypingPage TypingModel
    | TypingStatisticPage PastDictation
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


type KeyboardKey
    = Character Char
    | Control String


type alias Theme =
    { name : ThemeName, dark : Bool }


type ThemeName
    = WheatField
    | ElectricFields
    | CandyLand
    | NeoClassic


type NamedColor
    = Primary
    | Secondary
    | White
    | Black
