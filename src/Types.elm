module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)



-- Model


type alias FrontendModel =
    { key : Key
    , items : List Book
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    }


type alias Menu =
    { current : Maybe Book
    }


type alias Book =
    { title : String, content : String }


type alias Typing =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , layer : Int
    , book : Book
    , time : Float
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
    , book : Book
    , duration : Float
    }


{-| String that only allows chars and numbers
-}
type alias BackendModel =
    { currentSaltIndex : Int
    , passiveUsers : Dict Username User
    , activeSessions : Dict SessionId Session
    }


type alias Session =
    { user : User }


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
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
    | PreviewBook Book
    | OpenBook Book
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
    | OnHover (Maybe PastDictation)
    | SetUsername String
    | SetPassword String
    | SetVisibility Bool
    | TryLogin String String
    | TryRegister String String
    | Logout


type BackendMsg
    = Never


type ToBackend
    = UpdateSettings Settings
    | GetSettings
    | Register String String
    | Login String String
    | SessionCheck
    | BackendLogout


type ToFrontend
    = GotSettings Settings
    | ThrowOut
    | LoginSuccessful
    | LoginFailed



-- Types


type Page
    = MenuPage Menu
    | TypingPage Typing
    | TypingStatisticPage PastDictation
    | SettingsPage
    | StatisticPage (Maybe PastDictation)
    | LoginPage LoginState


type alias LoginState =
    { username : String
    , password : String
    , visibility : Bool
    , failed : Bool
    }


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
    { blockOnError = CorrectLetter, fontSize = 32, paddingLeft = 20, paddingRight = 20, layout = NeoQwertz }
