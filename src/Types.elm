module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time exposing (Posix)
import Url exposing (Url)



-- Model


type alias FrontendModel =
    { key : Key
    , items : List Lesson
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    }


type alias Menu =
    { current : Maybe Lesson
    }


type alias Lesson =
    { title : String, content : String }


type alias Typing =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , layer : Int
    , lesson : Lesson
    , duration : Float
    , paused : Bool
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


{-| String that only allows chars and numbers
-}
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
    }



-- Messages


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | PreviewBook Lesson
    | OpenBook Lesson
    | ToMenu
    | KeyDown KeyboardKey
    | KeyUp KeyboardKey
    | ToSettings
    | SetSettings Settings
    | ToStatistic
    | ToTypingStatistic PastDictation
    | TickTypingTime
    | Pause
    | Play
    | OnHover Hover
    | SetUsername String
    | SetPassword String
    | SetVisibility Bool
    | TryLogin String String
    | TryRegister String String
    | Logout
    | FinishedDictation (List TypeError) Lesson Float Posix


type alias Hover =
    Bucket


type BackendMsg
    = TimeTick Posix


type ToBackend
    = UpdateSettings Settings
    | GetSettings
    | InsertUser Username String
      -- | RemoveUser
    | InsertSession Username String
    | GetSession
    | RemoveSession
    | ConsStatistic PastDictation
    | GetStatistic


type ToFrontend
    = GotSettings Settings
    | KickOut
    | RegisterFailed
    | LoginSuccessful
    | LoginFailed
    | UpdateStatistic (List PastDictation)



-- Types


type Page
    = MenuPage Menu
    | TypingPage Typing
    | TypingStatisticPage PastDictation
    | SettingsPage
    | StatisticPage Hover
    | LoginAndRegisterPage LoginAndRegister


type alias LoginAndRegister =
    { username : String
    , password : String
    , visibility : Bool
    , failed : LoginFail
    }


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


defaultSettings : Settings
defaultSettings =
    { blockOnError = CorrectLetter, fontSize = 32, paddingLeft = 20, paddingRight = 20, layout = Neo }