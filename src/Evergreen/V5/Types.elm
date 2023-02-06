module Evergreen.V5.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type alias Lesson =
    { title : String
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


type alias PastDictation =
    { errors : List TypeError
    , lesson : Lesson
    , duration : Float
    , finished : Time.Posix
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
    | TypingStatisticPage PastDictation
    | SettingsPage
    | StatisticPage Hover
    | AuthPage AuthModel
    | InfoPage


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


type Theme
    = WheatField
    | ElectricFields
    | CandyLand


type alias Settings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    , theme : Theme
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    , authorised : Bool
    , usersCount2 : Int
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
    { user : User
    , created : Time.Posix
    }


type alias BackendModel =
    { currentSaltIndex : Int
    , passiveUsers : Dict.Dict Username User
    , activeSessions : Dict.Dict Lamdera.SessionId Session
    , currentTime : Time.Posix
    }


type KeyboardKey
    = Character Char
    | Control String


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


type PageMsg
    = TypingMsg TypingMsg
    | AuthMsg AuthMsg


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
    | GetUserCount


type BackendMsg
    = TimeTick Time.Posix


type ToFrontend
    = GotSettings Settings
    | KickOut
    | RegisterFailed
    | LoginSuccessful
    | LoginFailed
    | UpdateStatistic (List PastDictation)
    | UpdateUserCount Int
