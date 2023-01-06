module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Url


type alias Book =
    { title : String
    , content : String
    }


type alias MenuPage =
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


type alias TypingPage =
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


type Page
    = Menu MenuPage
    | Typing TypingPage
    | TypingStatistic PastDictation
    | Settings
    | Statistic


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


type alias UserSettings =
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
    , settings : UserSettings
    , statistic : List PastDictation
    }


type alias BackendModel =
    { settings : UserSettings
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
    | SetSettings UserSettings
    | ToStatistic
    | ToTypingStatistic PastDictation
    | TickTypingTime
    | Pause
    | Play


type ToBackend
    = UploadSettings UserSettings
    | FetchSettings


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = DownloadSettings UserSettings
