module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Html.Attributes
import Lamdera
import Pages.Auth
import Pages.Info
import Pages.Menu
import Pages.Settings
import Pages.Statistic
import Pages.Typing
import Pages.TypingStatistic as TypingStatistic
import Translation exposing (..)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel



--noinspection ElmUnusedSymbol


app : { init : Lamdera.Url -> Nav.Key -> ( Model, Cmd FrontendMsg ), view : Model -> Browser.Document FrontendMsg, update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg ), updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg ), subscriptions : Model -> Sub FrontendMsg, onUrlRequest : UrlRequest -> FrontendMsg, onUrlChange : Url.Url -> FrontendMsg }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , page = AuthPage defaultAuth
      , settings = defaultSettings
      , statistic = []
      , authorised = False
      , appStatistic =
            { userCount = 1 --min one
            , pastDictationCount = 0
            }
      }
    , Cmd.batch [ Lamdera.sendToBackend GetSession, Lamdera.sendToBackend GetAppStatistic ]
    )



-- Update


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update frontendMsg model =
    case frontendMsg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        Back ->
            let
                page =
                    case model.page of
                        MenuPage menu ->
                            MenuPage menu

                        TypingPage { lesson } ->
                            MenuPage { current = Just lesson }

                        TypingStatisticPage _ ->
                            StatisticPage []

                        SettingsPage _ ->
                            MenuPage { current = Nothing }

                        StatisticPage _ ->
                            MenuPage { current = Nothing }

                        AuthPage loginAndRegister ->
                            AuthPage loginAndRegister

                        InfoPage ->
                            MenuPage { current = Nothing }
            in
            ( { model | page = page }, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Lamdera.sendToBackend <| UpdateSettings settings )

        OnHover hovering ->
            case model.page of
                StatisticPage _ ->
                    ( { model | page = StatisticPage hovering }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Logout ->
            ( { model | page = AuthPage defaultAuth }, Lamdera.sendToBackend <| RemoveSession )

        FinishedDictation errors lesson duration time ->
            let
                past : PastDictation
                past =
                    { errors = errors, lesson = lesson, duration = duration, finished = time }

                newModel =
                    { model | statistic = past :: model.statistic }

                ( model2, cmd ) =
                    update (ChangePage <| TypingStatistic.init past True) newModel
            in
            ( model2, Cmd.batch [ Lamdera.sendToBackend <| ConsStatistic past, cmd ] )

        ChangePage page ->
            ( { model | page = page }
            , case page of
                InfoPage ->
                    Lamdera.sendToBackend GetAppStatistic

                TypingStatisticPage { past } ->
                    Lamdera.sendToBackend <| GetAllPoints past.lesson

                _ ->
                    Cmd.none
            )

        PageMsg pageMsg ->
            case pageMsg of
                AuthMsg ToggleTranslation ->
                    let
                        settings =
                            model.settings

                        toggleSettings =
                            case settings.language of
                                English ->
                                    German

                                German ->
                                    English

                        newSettings =
                            { settings | language = toggleSettings }
                    in
                    ( { model | settings = newSettings }, Cmd.none )

                _ ->
                    updatePage pageMsg model |> Tuple.mapFirst (\page -> { model | page = page })


updatePage : PageMsg -> Model -> ( Page, Cmd FrontendMsg )
updatePage pageMsg model =
    case ( pageMsg, model.page ) of
        ( TypingMsg msg, TypingPage pageModel ) ->
            Pages.Typing.update msg model.settings pageModel
                |> Tuple.mapFirst (Maybe.map TypingPage >> Maybe.withDefault (MenuPage { current = Nothing }))

        ( AuthMsg authMsg, AuthPage authModel ) ->
            Pages.Auth.update authMsg authModel
                |> Tuple.mapFirst mapAuth

        ( SettingsMsg msg, SettingsPage page ) ->
            Pages.Settings.update msg page
                |> Tuple.mapFirst SettingsPage

        _ ->
            ( model.page, Cmd.none )


mapAuth : AuthResult -> Page
mapAuth authResult =
    case authResult of
        JustModel m ->
            AuthPage m

        ToInfoPage ->
            InfoPage

        ToMenuPage ->
            MenuPage { current = Nothing }


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        GotSettings settings ->
            ( { model | settings = settings }, Cmd.none )

        UpdateStatistic pastDictations ->
            ( { model | statistic = pastDictations }, Cmd.none )

        KickOut ->
            if model.authorised then
                ( { model | authorised = False, page = AuthPage defaultAuth }, Cmd.none )

            else
                ( { model | authorised = False }, Cmd.none )

        LoginSuccessful ->
            ( { model | page = MenuPage <| Menu Nothing, authorised = True }, Cmd.batch [ Lamdera.sendToBackend GetSettings, Lamdera.sendToBackend GetStatistic ] )

        LoginFailed ->
            case model.page of
                AuthPage page ->
                    ( { model | page = AuthPage { page | failed = Types.WrongUsernameOrPassword } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegisterFailed ->
            case model.page of
                AuthPage page ->
                    ( { model | page = AuthPage { page | failed = Types.UsernameOrPasswordInvalid } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateAppStatistic statistic ->
            ( { model | appStatistic = statistic }, Cmd.none )

        UpdateAllPoints allPoints ->
            case model.page of
                TypingStatisticPage pageModel ->
                    let
                        newPage =
                            { pageModel | allPoints = Just allPoints }
                    in
                    ( { model | page = TypingStatisticPage newPage }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.page of
        TypingPage pageModel ->
            Pages.Typing.subscriptions pageModel |> Sub.map (PageMsg << TypingMsg)

        _ ->
            Sub.none


view : Model -> Browser.Document FrontendMsg
view model =
    let
        t =
            model.settings.theme

        l =
            model.settings.language
    in
    { title = pageTitle model.page ++ " - " ++ "Neo Train"
    , body =
        [ ptMonoLink
        , styleTag t
        , layoutWith (layoutOptions t)
            [ width fill, height fill, Background.color <| wheat t, Font.color <| black t, scrollbarY, padding 32 ]
          <|
            el
                (itemBorder t
                    ++ [ centerX
                       , centerY
                       , Border.rounded (t.rounding * 2)
                       , padding appPadding
                       , authorisedMessage l model.authorised
                       ]
                )
            <|
                body model
        ]
    }


authorisedMessage : Language -> Bool -> Attribute msg
authorisedMessage l authorized =
    if authorized then
        inFront <| none

    else
        inFront <|
            el
                [ alignBottom
                , alignLeft
                , moveDown 32
                , alpha 0.2
                , tooltip <| translate ProgressWillGetLost l
                ]
            <|
                text <|
                    translate Translation.NotLoggedIn l


body : Model -> Element FrontendMsg
body model =
    let
        t =
            model.settings.theme

        l =
            model.settings.language
    in
    case model.page of
        MenuPage menu ->
            Pages.Menu.view t model menu

        TypingPage typing ->
            Pages.Typing.view t typing model.settings |> map (PageMsg << TypingMsg)

        TypingStatisticPage pageModel ->
            TypingStatistic.view t pageModel

        SettingsPage page ->
            Pages.Settings.view t model.settings page

        StatisticPage hover ->
            Pages.Statistic.view t hover model.statistic

        AuthPage page ->
            Pages.Auth.view l t page |> map (PageMsg << AuthMsg)

        InfoPage ->
            Pages.Info.view model.appStatistic t


ptMonoLink : Html.Html msg
ptMonoLink =
    let
        fontNames =
            allMonoFonts
                |> List.map (monoFontName >> String.replace " " "+")
                |> List.map (\n -> "family=" ++ n)
                |> String.join "&"
    in
    Html.node "link"
        [ Html.Attributes.href <| "https://fonts.googleapis.com/css2?" ++ fontNames ++ "&display=swap"

        -- https://fonts.googleapis.com/css2?family=Itim&family=Roboto+Mono:ital,wght@0,100..700;1,100..700&display=swap
        , Html.Attributes.rel "stylesheet"
        ]
        []


{-| this changes the sidebar to look more like the theme
-}
styleTag : Theme -> Html.Html msg
styleTag t =
    Html.node "style"
        []
        [ Html.text <| """
*::-webkit-scrollbar {
    width: 24px;
}
*::-webkit-scrollbar-track {
    border-left: """ ++ String.fromInt t.borderWidth ++ """px solid """ ++ (toHex <| black t) ++ """;
}
*::-webkit-scrollbar-thumb {
    border-radius: """ ++ String.fromInt (t.rounding - 4) ++ """px;
    border-top-left-radius: 0;
    border-bottom-left-radius: 0;
    background-color: """ ++ (toHex <| black t) ++ """;
}
* {
    transition: color 50ms ease-out;
    transition: background-color 50ms ease-out;
    transition: scale 50ms ease-out;
}
:not(.s.sby) > *:hover[role=button], a:hover {
    z-index: 1;
    scale: 1.1;
}

input[type="range"]:hover ~ div > div > div > div:nth-child(2) {
    scale: 1.4;
}
/*animation-name: opacityOn;
animation-duration: 50ms;
@keyframes opacityOn {
    0% {
        scale: 0.9;
        opacity: 0;
    }
    100% {
        scale: 1;
        opacity: 1;
    }
}*/
"""
        ]
