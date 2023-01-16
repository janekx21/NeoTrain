module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Url


type alias Book =
    { title : String
    , content : String
    }


type alias Menu =
    { current : Maybe Book
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


type alias Typing =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , layer : Int
    , book : Book
    , time : Float
    , paused : Bool
    }


type alias PastDictation =
    { errors : List TypeError
    , book : Book
    , duration : Float
    }


type alias LoginState =
    { username : String
    , password : String
    , visibility : Bool
    , failed : Bool
    }


type Page
    = MenuPage Menu
    | TypingPage Typing
    | TypingStatisticPage PastDictation
    | SettingsPage
    | StatisticPage (Maybe PastDictation)
    | LoginPage LoginState


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


type alias Settings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , items : List Book
    , page : Page
    , settings : Settings
    , statistic : List PastDictation
    }


type alias Username =
    String


type alias User =
    { username : Username
    , passwordHash : String
    , passwordSalt : String
    , settings : Settings
    }


type alias Session =
    { user : User
    }


type alias BackendModel =
    { currentSaltIndex : Int
    , passiveUsers : Dict.Dict Username User
    , activeSessions : Dict.Dict Lamdera.SessionId Session
    }


type KeyboardKey
    = Character Char
    | Control String


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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


type ToBackend
    = UpdateSettings Settings
    | GetSettings
    | Register String String
    | Login String String
    | SessionCheck
    | BackendLogout


type BackendMsg
    = Never


type ToFrontend
    = GotSettings Settings
    | ThrowOut
    | LoginSuccessful
    | LoginFailed
