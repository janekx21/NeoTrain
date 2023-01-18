module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Hex
import Html
import Html.Attributes
import Lamdera
import Lamdera.Json as Decode exposing (Decoder)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import Task
import Time exposing (Posix)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel



--noinspection ElmUnusedSymbol


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
init _ key =
    let
        items =
            [ Lesson "Foo" "foobar"
            , Lesson "Ein Satz" "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern"
            , Lesson "Diktat | Zucker im Urin" textOne
            , Lesson "Elm" elmText
            , Lesson "Special Chars" ":: -- \\\\ // !?!?!? ?! !? ^ ?! ^ ?!!??! ++ -- ++ -- +- -+ ; () () {} [] [] () {} \\\\ // ___ ||| $$ ## <> <> == && <= >= '' '' \"\" \"\" ~~ %% ``` ``` *** (+~}/$|…\\#/$|[%)+?$=)^|](>=\\/{[(`]?-(\\{/(->=_><!^/`~&…_[<_=&[>]^<;'\"#\"$%+|~`+?)(/{*"
            , Lesson "Return" "hallo\nwelt"
            ]
                ++ dictates
                ++ List.repeat 40 { title = "todo", content = String.repeat 200 "this is just a placeholder" }
    in
    ( { key = key
      , items = items
      , page = LoginAndRegisterPage emptyLoginPage
      , settings = defaultSettings
      , statistic = []
      }
    , Cmd.batch [ Lamdera.sendToBackend GetSession ]
    )


textOne =
    "Ein Professor betonte in seinen Vorlesungen immer wieder, dass man sich als baldiger Arzt vor nichts ekeln darf. Außerdem müsse man beweisen, dass man eine herausragende Beobachtungsgabe hat. Er erklärte, dass man früher nicht die Möglichkeiten besaß, die man heute hat. So musste man beispielsweise mit der Zunge testen, wie viel Zucker im Urin ist. Die Studenten bekamen ein ungutes Gefühl, als der Professor schließlich ein Glas mit einer gelben Flüssigkeit hervorholte. Die Studenten sollten den Urin so testen wie früher. Der Professor machte es vor, tauchte den Zeigefinger in das Glas und leckte anschließend den Finger ab. Die Studenten waren schockiert. Dann hatten sie sich wieder gefasst und jeder machte es dem Professor nach. Manchmal verzog ein Student das Gesicht kurz zu einer Grimasse, aber alle leckten ihre Finger mit dem Urin ab. Dann sagte der Professor: „Ausgezeichnet. Sie alle haben ihren Ekel überwunden. Allerdings gibt es noch große Probleme bei der Beobachtung. Denn niemand hat anscheinend gesehen, dass ich zwar den Zeigefinger in das Glas gesteckt, aber den Mittelfinger abgeleckt habe."


dictates : List Lesson
dictates =
    [ Lesson "asdfjklö" "fjfj jfjf ff jj dd kk kddk dkdk ss ll llss slsl aa öö öaöa ööaa\ndas las jaja laff ja all fass lass fad da falls dass ff lala lfd alaaf als öd fall ad"
    , Lesson "erui" "ee ii rr uu eiru ruii erru reiu rurr iuer reir erri reii irre iuer\nalufrei ruderaler reife alufreier lesefauleres kille arider irrer residuelle leises sauerer alaaf sie allerlei deliriöse reellere ruf ausfiele frisiere dies"
    , Lesson "qwop" "qq pp ww oo qwop powp qwow powo pqqw owpq pqow powp woqp wwpo pqpp\nuferlose pseudofossil spediere erkiese wassre ersöffe au paarweiser diesfalls sakrale rural auffasere dieserlei kauere rasa persifliere krassere pofe kordialer spurloser"
    , Lesson "ghtz" "\ntarifierst reliefartiges weiterdelegiert ausgekehlte rezipiert weiterfahrt kalfaterte rostigster auszahltet fastetest fortgejagtes weglegtest taktlosester fortzieht alleeartiger herausoperiertest getrotzter wohldosierte lautstark topaktuelles"
    , Lesson "vncm" "\nunversehrtestes kastrierenden weltvergessenen zugespachtelt spielstark einheizen umzuquartierenden werksinterne zweiziffriges jugendfreier weiterrutschte durchpausten zupfenden vertrottelt chiffreartiges durchgefallen entdecktes wasserlöslicheres anfindende antioligarchischen"
    , Lesson "yb" "\nwissenschaftsfreundlichster krampfig orgastische herumlenkender sechsundsechzigstes hochzurechnende schallschluckendsteserlaubnisfreiem problemlösendem nachkorrigiertest hinschmissen kriegsbeteiligter auszuschelten vorberechneter herniederschwebende vorgemachten aufbauschenden durchkramendes geköpften anmusterten"
    , Lesson "x,." "\n"
    ]


elmText =
    String.repeat 10 "|> aö "
        ++ String.repeat 10 "<| aö "
        ++ String.repeat 10 "++ aj "
        ++ String.repeat 3 "a -> b -> c "
        ++ String.repeat 3 "el [] <| text \"foo\" "
        ++ String.repeat 3 "foo | bar | foobar | barfoo "
        ++ String.repeat 3 "( { foo | bar = bar } ) "


layouts =
    [ ( "Neo", Neo )
    , ( "Bone", Bone )
    , ( "NeoQwertz", NeoQwertz )
    , ( "AdNW", AdNW )
    , ( "KOY", KOY )
    , ( "NeoQwerty", NeoQwerty )
    , ( "Vou", Vou )
    , ( "Mine", Mine )
    ]



-- Update


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
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

        PreviewBook book ->
            ( { model | page = MenuPage { current = Just book } }, Cmd.none )

        OpenBook book ->
            ( { model
                | page =
                    TypingPage
                        { dictation = bookToDictation book
                        , madeError = False
                        , errors = []
                        , layer = 1
                        , lesson = book
                        , duration = 0
                        , paused = True
                        }
              }
            , Cmd.none
            )

        ToMenu ->
            ( { model | page = MenuPage { current = Nothing } }, Cmd.none )

        ToSettings ->
            ( { model | page = SettingsPage }, Cmd.none )

        KeyDown keyboardKey ->
            case model.page of
                TypingPage typing ->
                    case updateDictation keyboardKey model.settings typing of
                        Just page ->
                            ( { model | page = TypingPage page }, Cmd.none )

                        Nothing ->
                            ( model, Time.now |> Task.perform (FinishedDictation typing.errors typing.lesson typing.duration) )

                _ ->
                    ( model, Cmd.none )

        KeyUp keyboardKey ->
            case model.page of
                TypingPage typing ->
                    let
                        layer =
                            case keyboardKey of
                                Control key ->
                                    case key of
                                        -- Shift
                                        "CapsLock" ->
                                            1

                                        "AltGraph" ->
                                            1

                                        -- ShiftLevel5
                                        "Unidentified" ->
                                            1

                                        _ ->
                                            typing.layer

                                _ ->
                                    typing.layer
                    in
                    ( { model | page = TypingPage { typing | layer = layer } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Lamdera.sendToBackend <| UpdateSettings settings )

        ToStatistic ->
            ( { model | page = StatisticPage Nothing }, Cmd.none )

        ToTypingStatistic past ->
            ( { model | page = TypingStatisticPage past }, Cmd.none )

        TickTypingTime ->
            case model.page of
                TypingPage typing ->
                    let
                        next =
                            { typing | duration = typing.duration + ticksPerSecond }
                    in
                    ( { model | page = TypingPage next }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Pause ->
            case model.page of
                TypingPage typing ->
                    let
                        next =
                            { typing | paused = True }
                    in
                    ( { model | page = TypingPage next }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Play ->
            case model.page of
                TypingPage typing ->
                    let
                        next =
                            { typing | paused = False }
                    in
                    ( { model | page = TypingPage next }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        OnHover hovering ->
            case model.page of
                StatisticPage _ ->
                    ( { model | page = StatisticPage hovering }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetUsername string ->
            case model.page of
                LoginAndRegisterPage page ->
                    ( { model | page = LoginAndRegisterPage { page | username = string } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetPassword string ->
            case model.page of
                LoginAndRegisterPage page ->
                    ( { model | page = LoginAndRegisterPage { page | password = string } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetVisibility bool ->
            case model.page of
                LoginAndRegisterPage page ->
                    ( { model | page = LoginAndRegisterPage { page | visibility = bool } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TryLogin username password ->
            ( model, Lamdera.sendToBackend <| InsertSession username password )

        TryRegister username password ->
            ( model, Lamdera.sendToBackend <| InsertUser username password )

        Logout ->
            ( model, Lamdera.sendToBackend <| RemoveSession )

        FinishedDictation errors lesson duration time ->
            let
                past : PastDictation
                past =
                    { errors = errors, lesson = lesson, duration = duration, finished = time }
            in
            ( { model | page = TypingStatisticPage past, statistic = past :: model.statistic }
            , Lamdera.sendToBackend <| ConsStatistic past
            )


updateDictation : KeyboardKey -> Settings -> Typing -> Maybe Typing
updateDictation keyboardKey settings typing =
    let
        decodedKey =
            case keyboardKey of
                Control key ->
                    case key of
                        "Enter" ->
                            Character '\n'

                        _ ->
                            Control key

                Character char ->
                    Character char
    in
    case decodedKey of
        Control key ->
            case key of
                "Shift" ->
                    Just { typing | layer = 2 }

                "AltGraph" ->
                    Just { typing | layer = 3 }

                "ShiftLevel5" ->
                    Just { typing | layer = 4 }

                "Backspace" ->
                    Just { typing | madeError = False }

                _ ->
                    Just typing

        Character char ->
            if typing.paused then
                Just { typing | paused = False }

            else if settings.blockOnError == OneBackspace && typing.madeError then
                Just typing

            else if char /= typing.dictation.current && typing.madeError then
                Just typing

            else if char /= typing.dictation.current then
                Just { typing | madeError = True, errors = typing.errors ++ [ { was = char, should = typing.dictation.current } ] }

            else
                typing.dictation
                    |> advanceDictation
                    |> Maybe.map (\dictation -> { typing | dictation = dictation, madeError = False })


bookToDictation : Lesson -> Dictation
bookToDictation book =
    let
        ( current, next ) =
            book.content
                |> String.uncons
                |> Maybe.withDefault ( '?', "" )
    in
    { prev = "", current = current, next = next }


advanceDictation : Dictation -> Maybe Dictation
advanceDictation dict =
    dict.next
        |> String.uncons
        |> Maybe.map
            (\( nextCurrent, next ) ->
                { prev = dict.prev ++ String.fromChar dict.current
                , current = nextCurrent
                , next = next
                }
            )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        GotSettings settings ->
            ( { model | settings = settings }, Cmd.none )

        KickOut ->
            ( { model | page = LoginAndRegisterPage emptyLoginPage }, Cmd.none )

        LoginSuccessful ->
            ( { model | page = MenuPage <| Menu Nothing }, Cmd.batch [ Lamdera.sendToBackend GetSettings, Lamdera.sendToBackend GetStatistic ] )

        LoginFailed ->
            case model.page of
                LoginAndRegisterPage page ->
                    ( { model | page = LoginAndRegisterPage { page | failed = WrongUsernameOrPassword } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        RegisterFailed ->
            case model.page of
                LoginAndRegisterPage page ->
                    ( { model | page = LoginAndRegisterPage { page | failed = UsernameOrPasswordInvalid } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateStatistic pastDictations ->
            ( { model | statistic = pastDictations }, Cmd.none )


emptyLoginPage =
    { username = "", password = "", visibility = False, failed = NotAsked }


ticksPerSecond =
    0.1


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.page of
        TypingPage { paused } ->
            Sub.batch
                [ keyDecoder |> Decode.map (KeyDown << toKey) |> Browser.Events.onKeyDown
                , keyDecoder |> Decode.map (KeyUp << toKey) |> Browser.Events.onKeyUp
                , if paused then
                    Sub.none

                  else
                    Time.every (ticksPerSecond * 1000) (\_ -> TickTypingTime)
                ]

        _ ->
            Sub.none


toKey : String -> KeyboardKey
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.decoderString


view : Model -> Browser.Document FrontendMsg
view model =
    let
        body =
            case model.page of
                MenuPage menu ->
                    viewMenu model menu

                TypingPage typing ->
                    viewTyping typing model.settings

                TypingStatisticPage past ->
                    viewTypingStatistic past

                SettingsPage ->
                    viewSettings model.settings

                StatisticPage hover ->
                    viewStatistic hover model.statistic

                LoginAndRegisterPage page ->
                    viewLogin page

        pageTitle =
            case model.page of
                MenuPage _ ->
                    "Menu"

                TypingPage _ ->
                    "Typing"

                TypingStatisticPage _ ->
                    "Typing Statistic"

                SettingsPage ->
                    "Settings"

                StatisticPage _ ->
                    "Statistic"

                LoginAndRegisterPage _ ->
                    "Login"
    in
    { title = pageTitle ++ " - " ++ "Neo Train"
    , body =
        [ ptMonoLink
        , styleTag
        , iconTag
        , layout [ width fill, height fill, Background.color wheat, Font.color black ]
            (el
                [ centerX, centerY, Border.color black, Border.width 1, padding appPadding, Border.rounded 16 ]
             <|
                body
            )
        ]
    }


viewLogin : LoginAndRegister -> Element FrontendMsg
viewLogin { username, password, visibility, failed } =
    let
        inputStyle =
            [ width fill, Background.color wheat, Border.width 1, Border.color black, Border.rounded 0 ]

        eyeButton =
            Input.button [ height fill, padding 11, tooltip "Show password" ]
                { label =
                    materialIcon
                        (if visibility then
                            Icons.visibility_off

                         else
                            Icons.visibility
                        )
                , onPress = Just <| SetVisibility <| not visibility
                }
    in
    column [ spacing 32 ]
        [ title "Login / Register"
        , case failed of
            NotAsked ->
                none

            WrongUsernameOrPassword ->
                el [ Font.color primary ] <| text "Wrong username or password!"

            UsernameOrPasswordInvalid ->
                paragraph [ Font.color primary, width (px 500) ] [ text "Username or password invalid. Username should be alphanumeric. Password should be length 10, alphanumeric with some special chars (IBM valid character)." ]
        , column [ spacing 8, width fill ]
            [ subTitle "Username"
            , Input.username inputStyle
                { text = username
                , label = Input.labelHidden "username"
                , placeholder = Nothing
                , onChange = SetUsername
                }
            ]
        , column [ spacing 8, width fill ]
            [ subTitle "Password"
            , Input.currentPassword (inputStyle ++ [ inFront <| el [ alignRight ] <| eyeButton ])
                { text = password
                , label = Input.labelHidden "password"
                , placeholder = Nothing
                , onChange = SetPassword
                , show = visibility
                }
            ]
        , row [ spacing 16, width fill ]
            [ Input.button
                [ width fill
                , Border.color black
                , Border.width 1
                , padding 8
                , mouseOver [ Background.color primary ]
                ]
                { label = el [ centerX ] <| text "Login", onPress = Just <| TryLogin username password }
            , Input.button
                [ width fill
                , Border.color black
                , Border.width 1
                , padding 8
                , mouseOver [ Background.color primary ]
                ]
                { label = el [ centerX ] <| text "Register", onPress = Just <| TryRegister username password }
            ]
        ]


appPadding =
    24


ptMonoLink =
    Html.node "link"
        [ Html.Attributes.href "https://fonts.googleapis.com/css2?family=PT+Mono&display=swap"
        , Html.Attributes.rel "stylesheet"
        ]
        []


{-| this changes the sidebar to look more like the theme
-}
styleTag =
    Html.node "style"
        []
        [ Html.text """
*::-webkit-scrollbar {
    width: 24px;
}
*::-webkit-scrollbar-track {
    border-left: 1px solid black;
}
*::-webkit-scrollbar-thumb {
  background-color: black;
}
 """
        ]


iconTag =
    Html.node "link"
        [ Html.Attributes.rel "shortcut icon", Html.Attributes.type_ "image/x-icon", Html.Attributes.href "/favicon.svg" ]
        []



-- Menu


viewMenu : Model -> Menu -> Element FrontendMsg
viewMenu model menu =
    let
        block label child =
            column [ spacing 8 ] [ subTitle label, child ]

        sidebar =
            case menu.current of
                Just book ->
                    column [ height fill, spacing 32 ]
                        [ block "Char count" <| text <| String.fromInt <| String.length book.content
                        , block "Word count" <| text <| String.fromInt <| List.length <| String.split " " <| book.content
                        , block "Preview" <| paragraph [ width (px 400), height fill ] [ text (book.content |> truncate 140) ]
                        , Input.button
                            [ alignRight
                            , alignBottom
                            , Border.color black
                            , Border.width 1
                            , padding 8
                            , mouseOver [ Background.color primary ]
                            ]
                            { onPress = Just (OpenBook book), label = text "Start" }
                        ]

                Nothing ->
                    none

        topBar =
            row [ spacing 16, alignRight, moveRight (appPadding + 21), moveUp (appPadding + 21) ] [ statisticButton, settingsButton ]
    in
    column [ spacing 32, inFront <| topBar ]
        [ title "Dictations"
        , row
            [ spacing 40 ]
            [ column
                [ Border.color black, Border.width 1, height (fill |> maximum 512), scrollbarY, width fill ]
                (model.items |> List.map viewMenuItem)
            , sidebar
            ]
        ]


viewMenuItem : Lesson -> Element FrontendMsg
viewMenuItem book =
    let
        charsPerSecond =
            5
    in
    Input.button
        [ width fill, padding 8, mouseOver [ Background.color primary ] ]
        { label = text (book.title ++ " (" ++ printSeconds ((book.content |> String.length |> toFloat) / charsPerSecond) ++ ")")
        , onPress = Just <| PreviewBook book
        }



-- Settings


viewSettings : Settings -> Element FrontendMsg
viewSettings settings =
    let
        settingsBlock label child =
            column [ width fill, spacing 8 ]
                [ subTitle label
                , child
                ]

        blockSettingItem label setting =
            viewSettingsItem label (SetSettings { settings | blockOnError = setting }) (settings.blockOnError == setting)

        layoutSettingItem label setting =
            viewSettingsItem label (SetSettings { settings | layout = setting }) (settings.layout == setting)

        paddingSlider value msg =
            row [ width fill, spacing 8 ]
                [ el [ monospace ] <| text <| String.padLeft 2 ' ' (String.fromInt value)
                , el [ width fill, Border.color black, Border.width 1 ] <|
                    Input.slider [ width fill ]
                        { onChange = msg
                        , label = Input.labelHidden ""
                        , min = 0
                        , max = 50
                        , value = toFloat value
                        , thumb = Input.thumb [ width (px 20), height (px 20), Background.color black ]
                        , step = Just 1
                        }
                ]

        topBar =
            row [ moveLeft (appPadding + 21), moveUp (appPadding + 21), spacing 8 ] [ backButton, logoutButton ]
    in
    column [ inFront <| topBar, spacing 48 ]
        [ title "Settings"
        , settingsBlock
            "Layout Preview"
            (column [ width fill, Border.color black, Border.width 1 ]
                (layouts |> List.map (\( name, layout ) -> layoutSettingItem name layout))
            )
        , settingsBlock "Blocking"
            (column [ width fill, Border.color black, Border.width 1 ]
                [ blockSettingItem "Waiting for one backspace" OneBackspace
                , blockSettingItem "Waiting for the correct letter" CorrectLetter
                ]
            )
        , settingsBlock "Chars left and right of cursor" <|
            column [ width fill, spacing 8 ]
                [ paddingSlider settings.paddingLeft (\value -> SetSettings { settings | paddingLeft = round value })
                , paddingSlider settings.paddingRight (\value -> SetSettings { settings | paddingRight = round value })
                ]
        ]


viewSettingsItem labelText msg active =
    Input.button
        ([ width fill, padding 8, mouseOver [ Background.color primary ] ]
            ++ (if active then
                    [ Background.color secondary, Font.color wheat ]

                else
                    []
               )
        )
        { label = text labelText, onPress = Just msg }


printSeconds seconds =
    if seconds > 60 then
        ((seconds / 60) |> round |> String.fromInt) ++ "min"

    else
        (seconds |> round |> String.fromInt) ++ "sec"



-- Typing


viewTyping : Typing -> Settings -> Element FrontendMsg
viewTyping { dictation, layer, madeError, paused } settings =
    let
        color =
            if madeError then
                primary

            else
                secondary

        prev =
            dictation.prev
                |> String.right settings.paddingLeft
                |> String.padLeft settings.paddingLeft '\u{0000}'
                |> String.toList

        next =
            dictation.next
                |> String.left settings.paddingRight
                |> String.padRight settings.paddingRight '\u{0000}'
                |> String.toList

        typewriter =
            row [ monospace, Font.size 32 ]
                [ row [] (List.map viewChar prev)
                , el [ Background.color color, Font.color wheat ] <| viewChar dictation.current
                , row [ Font.color secondary ] (List.map viewChar next)
                ]

        pausedEl =
            el [ centerX, monospace, Font.size 32 ] <|
                text <|
                    String.pad (settings.paddingLeft + settings.paddingRight + 1) ' ' <|
                        "Paused. Press Any Key"
    in
    column
        [ spacing 64
        , inFront <|
            row [ moveLeft (appPadding + 21), moveUp (appPadding + 21), spacing 8 ]
                [ backButton
                , if paused then
                    playButton

                  else
                    pauseButton
                ]
        ]
        [ if paused then
            pausedEl

          else
            typewriter
        , image [ width fill ] { src = layerUrl settings.layout layer, description = "" }
        ]


layerUrl layout layer =
    let
        name =
            case layout of
                Neo ->
                    "neo"

                NeoQwertz ->
                    "neo_qwertz"

                Bone ->
                    "bone"

                AdNW ->
                    "adnw"

                KOY ->
                    "koy"

                NeoQwerty ->
                    "neo_qwerty"

                Vou ->
                    "vou"

                Mine ->
                    "mine"
    in
    "https://dl.neo-layout.org/grafik/bilder-einzeln/flat/" ++ name ++ "-" ++ String.fromInt layer ++ "-tkl.svg"



-- Statistic


viewStatistic : Maybe PastDictation -> List PastDictation -> Element FrontendMsg
viewStatistic hovering statistic =
    let
        viewPast : PastDictation -> Element FrontendMsg
        viewPast past =
            Input.button
                ([ width fill
                 , padding 8
                 , mouseOver [ Background.color primary ]
                 , Element.Events.onMouseEnter <| OnHover <| Just past
                 ]
                    ++ (if Just past == hovering then
                            [ Background.color primary ]

                        else
                            []
                       )
                )
                { label = text <| (String.fromInt <| points past) ++ "\t" ++ past.lesson.title
                , onPress = Just (ToTypingStatistic past)
                }

        viewHover =
            case hovering of
                Just h ->
                    column [ spacing 8 ]
                        [ text h.lesson.title
                        , text <| "Points: " ++ (String.fromInt <| points h)
                        ]

                Nothing ->
                    -- placeholder because of hover jitter
                    column [ spacing 8 ] [ text " ", text " " ]
    in
    column [ inFront <| el [ moveLeft (appPadding + 21), moveUp (appPadding + 21) ] <| backButton, spacing 48, width fill ]
        [ title "Statistic"
        , row [ width fill, spacing 48 ]
            [ column [ spacing 8, width fill, alignTop ]
                [ subTitle "Past Dictations"
                , if List.isEmpty statistic then
                    info "You have no past dictations"

                  else
                    el [] <|
                        column
                            [ width fill
                            , height (fill |> maximum 512)
                            , scrollbarY
                            , Border.width 1
                            , Border.color black
                            , Element.Events.onMouseLeave <| OnHover <| Nothing
                            ]
                        <|
                            List.map viewPast statistic
                ]
            , column [ spacing 8, alignTop ] [ subTitle "Point Progression", viewPointsGraph hovering statistic, viewHover ]
            ]
        ]


points : PastDictation -> Int
points past =
    let
        err =
            toFloat (List.length past.errors) / toFloat (String.length past.lesson.content)
    in
    max 0 <| round <| ((charsPerMinute past.lesson past.duration / 5) - (err * 5)) * 10


wordsPerMinute book duration =
    let
        words =
            book.content |> String.words |> List.length

        durationInMin =
            duration / 60
    in
    toFloat words / durationInMin


charsPerMinute book duration =
    let
        chars =
            String.length book.content

        durationInMin =
            duration / 60
    in
    toFloat chars / durationInMin


info labelText =
    row [ spacing 8 ] [ materialIcon Icons.info, text labelText ]


errorPercent { content } errors =
    let
        percent =
            round ((toFloat (List.length errors) / toFloat (String.length content)) * 100)
    in
    String.fromInt percent ++ "%"


viewTypingStatistic : PastDictation -> Element FrontendMsg
viewTypingStatistic past =
    let
        { lesson, errors, duration } =
            past

        grouped =
            errors
                |> List.foldr (\error -> Dict.update error.should (\y -> Just <| Maybe.withDefault [] y ++ [ error ])) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> List.length)
                |> List.reverse
    in
    column
        [ spacing 48
        , inFront <|
            row [ centerX, spacing 16, alignBottom, moveDown (appPadding + 21) ]
                [ homeButton
                , statisticButton
                , button (Just <| OpenBook lesson) (materialIcon Icons.refresh) 'r'
                ]
        ]
        [ title "Your Typing Statistic"
        , column [ spacing 8 ]
            [ subTitle "Time"
            , text <| "Duration: " ++ printSeconds duration
            , text <| "Chars per Minute: " ++ (String.fromInt <| round <| charsPerMinute lesson duration)
            , text <| "Words per Minute: " ++ (String.fromInt <| round <| wordsPerMinute lesson duration)
            ]
        , column [ spacing 8 ]
            [ subTitle "Errors Rate"
            , text <| "Count: " ++ String.fromInt (List.length errors)
            , text <| "Percent: " ++ errorPercent lesson errors
            ]
        , column [ spacing 8 ]
            [ subTitle "Errors"
            , if List.isEmpty grouped then
                info "There are no errors"

              else
                wrappedRow [ spacing 16, width (fill |> maximum 650) ] (grouped |> List.map viewError)
            ]
        , column [ spacing 8 ]
            [ subTitle "Points"
            , text <| String.fromInt <| points past
            ]
        , el [] none -- blocker
        ]


viewChar char =
    let
        charSize =
            [ htmlAttribute <| Html.Attributes.style "width" "0.6em"
            , htmlAttribute <| Html.Attributes.style "height" "1em"
            ]
    in
    case char of
        '\u{0000}' ->
            -- placeholder for no character
            el charSize <| text " "

        ' ' ->
            el charSize <|
                el [ alignBottom, centerX, moveDown 8, alpha 0.5 ] <|
                    materialIcon Icons.space_bar

        '\n' ->
            el charSize <|
                el [ alignBottom, centerX ] <|
                    materialIcon Icons.keyboard_return

        _ ->
            text <|
                String.fromChar char


viewError : ( Char, List TypeError ) -> Element msg
viewError ( char, typeErrors ) =
    row [ spacing 8, Border.width 1, Border.color black, padding 4, Border.rounded 999 ]
        [ el
            [ monospace
            , Font.color wheat
            , Background.color primary
            , Border.rounded 999
            , width (px 24)
            , height (px 24)
            ]
          <|
            el [ centerX, centerY ] <|
                viewChar char
        , text <| (String.fromInt <| List.length typeErrors) ++ "x"
        ]


viewPointsGraph : Maybe PastDictation -> List PastDictation -> Element FrontendMsg
viewPointsGraph hovering dictations =
    let
        items =
            dictations
                |> List.take 15
                |> List.reverse
    in
    el [ width <| px 300, height fill ] <|
        html <|
            C.chart
                ([ CA.height 300
                 , CA.width 300
                 , CA.margin { top = 8, bottom = 32, left = 48, right = 8 }
                 , CA.padding { top = 4, bottom = 4, left = 4, right = 4 }
                 , CE.onMouseMove (List.map CI.getData >> List.head >> OnHover) (CE.getNearest CI.dots)
                 , CE.onMouseLeave (OnHover Nothing)
                 , CE.onMouseUp
                    (List.map CI.getData >> List.head >> Maybe.map ToTypingStatistic >> Maybe.withDefault NoOpFrontendMsg)
                    (CE.getNearest CI.dots)
                 ]
                    ++ (if hovering == Nothing then
                            []

                        else
                            [ CA.htmlAttrs [ Html.Attributes.style "cursor" "pointer" ] ]
                       )
                )
                [ C.yTicks [ CA.color (toHex black) ]
                , C.yLabels [ CA.color (toHex black) ]
                , C.grid [ CA.color (toHex secondary) ]
                , C.series (.finished >> Time.posixToMillis >> toFloat)
                    [ C.interpolated (points >> toFloat) [ CA.color <| toHex primary, CA.width 4 ] []
                        |> C.variation
                            (\_ data ->
                                if Just data == hovering then
                                    [ CA.circle, CA.size 48, CA.color (toHex primary) ]

                                else
                                    []
                            )
                    ]
                    items
                ]


bucketStatistic : List PastDictation -> Dict Int (List PastDictation)
bucketStatistic pastDictations =
    let
        insert : PastDictation -> Dict Int (List PastDictation) -> Dict Int (List PastDictation)
        insert pastDictation dict =
            let
                key =
                    posixToDay pastDictation.finished
            in
            if Dict.member key dict then
                Dict.update key (Maybe.map (\v -> pastDictation :: v)) dict

            else
                Dict.insert key [ pastDictation ] dict
    in
    pastDictations |> List.foldl insert Dict.empty


posixToDay : Posix -> Int
posixToDay posix =
    Time.posixToMillis posix // (1000 * 60 * 60 * 24)



-- C.xAxis [ CA.color (toHex black) ]
-- C.xTicks [ CA.color (toHex black), CA.ints, CA.noGrid ]
--, C.xLabels [ CA.color (toHex black), CA.ints ]
--, C.yAxis [ CA.color (toHex black) ]
--, C.each (List.map CI.Item hovering) <|
--   \p item ->
--      [ C.tooltip item [] [] [] ]


toHex color =
    color
        |> toRgb
        |> (\{ red, green, blue } -> [ red, green, blue ])
        |> List.map (\c -> floor <| c * 256)
        |> List.map Hex.toString
        |> String.concat
        |> String.cons '#'



-- Common
{-
   lineClamp lines =
       -- todo make work when needed
       [ htmlAttribute (Html.Attributes.style "display" "-webkit-box")
       , htmlAttribute (Html.Attributes.style "-webkit-line-clamp" (String.fromInt lines))
       , htmlAttribute (Html.Attributes.style "-webkit-box-orient" "vertical")
       , htmlAttribute (Html.Attributes.style "overflow" "hidden")
       ]
-}


backButton =
    button (Just ToMenu) (materialIcon Icons.arrow_back) 'b'


logoutButton =
    button (Just Logout) (materialIcon Icons.logout) 'l'


pauseButton =
    button (Just Pause) (materialIcon Icons.pause) 'p'


playButton =
    button (Just Play) (materialIcon Icons.play_arrow) 'p'


settingsButton =
    button (Just ToSettings) (materialIcon Icons.settings) 's'


statisticButton =
    button (Just ToStatistic) (materialIcon Icons.query_stats) 't'


homeButton =
    button (Just ToMenu) (materialIcon Icons.home) 'h'


button onPress label shortcut =
    Input.button
        ([ Background.color wheat
         , Border.color black
         , Border.width 1
         , Border.rounded 999
         , padding 8
         , mouseOver [ Background.color primary ]
         ]
            ++ accessKey shortcut
        )
        { label = label, onPress = onPress }


accessKey key =
    [ htmlAttribute (Html.Attributes.accesskey key)
    , tooltip <| "Shortcut: <Alt-" ++ String.toUpper (String.fromChar key) ++ ">"
    ]


tooltip string =
    htmlAttribute <| Html.Attributes.title string



--aspect ration =
--    htmlAttribute (Html.Attributes.style "aspect-ration" <| String.fromFloat ration)


title labelText =
    el [ Font.size 32 ] <| text labelText


subTitle labelText =
    el [ Font.bold ] <| text labelText


materialIcon : Icon msg -> Element msg
materialIcon icon =
    el [] <| html <| icon 24 Inherit


truncate limit text =
    if String.length text > (limit - 3) then
        (text |> String.left (limit - 3) |> String.trimRight) ++ "…"

    else
        text


monospace =
    Font.family [ Font.typeface "PT Mono", Font.monospace ]


black =
    rgb255 51 51 41


primary =
    rgb255 243 145 146


secondary =
    rgb255 175 134 132


wheat =
    rgb255 255 249 231



--zero4 =
--    { left = 0, top = 0, right = 0, bottom = 0 }
