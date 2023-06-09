module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Lazy
import Html
import Html.Attributes
import Lamdera
import Pages.Auth
import Pages.Info
import Pages.Menu
import Pages.Settings
import Pages.Statistic
import Pages.Typing
import Pages.TypingStatistic
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
      , usersCount = 1 --min one
      }
    , Cmd.batch [ Lamdera.sendToBackend GetSession, Lamdera.sendToBackend GetUserCount ]
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
            in
            ( { model | page = TypingStatisticPage past, statistic = past :: model.statistic }
            , Lamdera.sendToBackend <| ConsStatistic past
            )

        ChangePage page ->
            ( { model | page = page }
            , if page == InfoPage then
                Lamdera.sendToBackend GetUserCount

              else
                Cmd.none
            )

        PageMsg pageMsg ->
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
                    ( { model | page = AuthPage { page | failed = WrongUsernameOrPassword } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegisterFailed ->
            case model.page of
                AuthPage page ->
                    ( { model | page = AuthPage { page | failed = UsernameOrPasswordInvalid } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateUserCount count ->
            ( { model | usersCount = count }, Cmd.none )


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
                       , authorisedMessage model.authorised
                       , previewLabel
                       ]
                )
            <|
                body model
        ]
    }


authorisedMessage : Bool -> Attribute msg
authorisedMessage authorized =
    if authorized then
        inFront <| none

    else
        inFront <|
            el
                [ alignBottom
                , alignLeft
                , moveDown 32
                , alpha 0.2
                , tooltip "You are not logged in and you progress will be lost"
                ]
            <|
                text "not logged in"


body : Model -> Element FrontendMsg
body model =
    let
        t =
            model.settings.theme
    in
    case model.page of
        MenuPage menu ->
            Pages.Menu.view t model menu

        TypingPage typing ->
            Pages.Typing.view t typing model.settings |> map (PageMsg << TypingMsg)

        TypingStatisticPage past ->
            Pages.TypingStatistic.view t past

        SettingsPage page ->
            Pages.Settings.view t model.settings page

        StatisticPage hover ->
            Pages.Statistic.view t hover model.statistic

        AuthPage page ->
            Pages.Auth.view t page |> map (PageMsg << AuthMsg)

        InfoPage ->
            Pages.Info.view model.usersCount t


previewLabel : Attribute msg
previewLabel =
    inFront <|
        el
            [ alignBottom
            , alignRight
            , moveDown 32
            , alpha 0.2
            , tooltip "your progress or account could get lost in an update"
            ]
        <|
            text "preview"


ptMonoLink : Html.Html msg
ptMonoLink =
    Html.node "link"
        [ Html.Attributes.href "https://fonts.googleapis.com/css2?family=PT+Mono&display=swap"
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
