module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , items : List Book
    , page : Page
    , settings : UserSettings
    , statistic : List PastDictation
    }



-- TODO


type alias UserSettings =
    { blockOnError : WaitingFor
    , fontSize : Int
    , paddingLeft : Int
    , paddingRight : Int
    , layout : Layout
    }


defaultSettings : UserSettings
defaultSettings =
    { blockOnError = CorrectLetter, fontSize = 32, paddingLeft = 20, paddingRight = 20, layout = NeoQwertz }


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


type Page
    = Menu MenuPage
    | Typing TypingPage
    | TypingStatistic (List TypeError)
    | Settings
    | Statistic


type alias MenuPage =
    { current : Maybe Book
    }


type alias Book =
    { title : String, content : String }


type alias TypingPage =
    { dictation : Dictation
    , madeError : Bool
    , errors : List TypeError
    , layer : Int
    , bookTitle : String
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
    , bookTitle : String
    }


type alias BackendModel =
    { settings : UserSettings
    }


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
    | SetSettings UserSettings
    | ToStatistic


type KeyboardKey
    = Character Char
    | Control String


type ToBackend
    = UploadSettings UserSettings
    | FetchSettings


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = DownloadSettings UserSettings
