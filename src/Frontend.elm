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
import Icon
import Lamdera
import Lamdera.Json as Decode exposing (Decoder)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import Set exposing (Set)
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
    ( { key = key
      , page = AuthPage defaultAuth
      , settings = defaultSettings
      , statistic = []
      , authorised = False
      }
    , Cmd.batch [ Lamdera.sendToBackend GetSession ]
    )


lessons : List Lesson
lessons =
    let
        elmText =
            String.repeat 5 "|> "
                ++ String.repeat 5 "<| "
                ++ String.repeat 5 "++ \"\" "
                ++ String.repeat 2 "a -> b -> c "
                ++ String.repeat 2 "el [] <| text \"foo\" "
                ++ String.repeat 2 "foo | bar | foobar | barfoo "
                ++ String.repeat 2 "( { foo | bar = bar } ) "
    in
    [ Lesson "asdfjklö" "fjfj jfjf ff jj dd kk kddk dkdk ss ll llss slsl aa öö öaöa ööaa\ndas las jaja laff ja all fass lass fad da falls dass ff lala lfd alaaf als öd fall ad"
    , Lesson "erui" "ee ii rr uu eiru ruii erru reiu rurr iuer reir erri reii irre iuer\nalufrei ruderaler reife alufreier lesefauleres kille arider irrer residuelle leises sauerer alaaf sie allerlei deliriöse reellere ruf ausfiele frisiere dies"
    , Lesson "qwop" "qq pp ww oo qwop powp qwow powo pqqw owpq pqow powp woqp wwpo pqpp\nuferlose pseudofossil spediere erkiese wassre ersöffe au paarweiser diesfalls sakrale rural auffasere dieserlei kauere rasa persifliere krassere pofe kordialer spurloser"
    , Lesson "ghtz" "ghtz tzgh ghtz ghgh tztz zztt hhgg gghh ttzz ghtz hgzt hgtz ghzt\ntarifierst reliefartiges weiterdelegiert ausgekehlte rezipiert weiterfahrt kalfaterte rostigster auszahltet fastetest fortgejagtes weglegtest taktlosester fortzieht alleeartiger herausoperiertest getrotzter wohldosierte lautstark topaktuelles"
    , Lesson "vncm" "vncm mcnv vncm cvnm mnvc cvnm mmcc nnvv cmvn mvnc cmcn ccmm vncm\nunversehrtestes kastrierenden weltvergessenen zugespachtelt spielstark einheizen umzuquartierenden werksinterne zweiziffriges jugendfreier weiterrutschte durchpausten zupfenden vertrottelt chiffreartiges durchgefallen entdecktes wasserlöslicheres anfindende antioligarchischen"
    , Lesson "yb" "yy bb yy bb by yb yyyb bbby bbyy yybb bbbb yyyb\nalkylsubstituierten altbayerische hypnophob nylonbestrumpftem hyperbolischem hybridogenes hypnophober currygelben branchentypischen embryologischem synchronisierbaren methylenblauer altlibyschem subsynaptischen royalblaue kybernetisch nichtbayerischen systembedingtem polyalphabetisch boykottbedingter"
    , Lesson "x,." "xx .. xx .. x.x .x. xx.. x..x xxx. .... x...\nschulexternen asexuelles kontextspezifischem. textiler praxisfeldbezogenen oxidfrei. extrapolierende durchexerziertest kontextbasiertem textlastige exhumierter. textnahes retroflex wetterexponierter extrascharfe. explodierender linksextremster fehlerfixiert. annexionistischer zytotoxisch"
    , Lesson "Ein Satz" "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern"
    , Lesson "Spezial Zeichen" ":: -- \\\\ // !?!?!? ?! !? ^ ?! ^ ?!!??! ++ -- ++ -- +- -+ ; () () {} [] [] () {} \\\\ // ___ ||| $$ ## <> <> == && <= >= '' '' \"\" \"\" ~~ %% ``` ``` *** (+~}/$|\\#/$|[%)+?$=) ^|](>=\\/{[(`]?-(\\{/(-> =_><!^/`~&_[<_=&[>] ^<;'\"#\"$%+|~`+?)(/{*"
    , Lesson "Elm" elmText
    , Lesson "Junge und Berg von Chat GPT" "Es war einmal ein kleiner Junge, der in einem Dorf am Fuße eines großen Berges wohnte. Eines Tages beschloss er, den Berg zu besteigen, um zu sehen, was oben war. Er packte seinen Rucksack mit Proviant und Wasser und begann seine Reise. Der Aufstieg war beschwerlich, aber er gab nicht auf. Nach vielen Stunden erreichte er die Spitze des Berges und was er sah, übertraf seine kühnsten Träume. Er sah unendliche Wälder, kristallklare Flüsse und Täler voller wilder Blumen. Er beschloss, dass er immer wieder hierher zurückkehren würde, um die Schönheit dieses Ortes zu genießen."
    , Lesson "Klimawandel Debatte von Chat GPT" "Die Debatte um den Klimawandel hat in den letzten Jahren stark zugenommen. Experten sind sich einig, dass der Ausstoß von Treibhausgasen und die Abholzung von Wäldern die Erderwärmung beschleunigen. Regierungen auf der ganzen Welt haben sich verpflichtet, Maßnahmen zu ergreifen, um diesem Problem entgegenzuwirken. Ein wichtiger Schritt ist die Förderung erneuerbarer Energien und die Verringerung des CO2-Ausstoßes. Auch die Wiederaufforstung von Wäldern und die Schaffung von Schutzgebieten können dazu beitragen, den Klimawandel zu bekämpfen."
    , Lesson "DebugText" "foobar"
    ]


layoutNames layout =
    case layout of
        Neo ->
            "Neo"

        Bone ->
            "Bone"

        NeoQwertz ->
            "NeoQwertz"

        AdNW ->
            "AdNW"

        KOY ->
            "KOY"

        NeoQwerty ->
            "NeoQwerty"

        Vou ->
            "Vou"

        Mine ->
            "Mine"


layouts =
    [ Neo, Bone, NeoQwertz, AdNW, KOY, NeoQwerty, Vou, Mine ]


themes =
    [ ( "WheatField", WheatField )
    , ( "ElectricFields", ElectricFields )
    , ( "CandyLand", CandyLand )
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
                        , showKeyboard = True
                        }
              }
            , Cmd.none
            )

        Back ->
            let
                page =
                    case model.page of
                        MenuPage menu ->
                            MenuPage menu

                        TypingPage _ ->
                            MenuPage { current = Nothing }

                        TypingStatisticPage _ ->
                            StatisticPage []

                        SettingsPage ->
                            MenuPage { current = Nothing }

                        StatisticPage _ ->
                            MenuPage { current = Nothing }

                        AuthPage loginAndRegister ->
                            AuthPage loginAndRegister

                        InfoPage ->
                            if model.authorised then
                                MenuPage { current = Nothing }

                            else
                                AuthPage defaultAuth
            in
            ( { model | page = page }, Cmd.none )

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
            ( { model | page = StatisticPage [] }, Cmd.none )

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
                AuthPage page ->
                    ( { model | page = AuthPage { page | username = string } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetPassword string ->
            case model.page of
                AuthPage page ->
                    ( { model | page = AuthPage { page | password = string } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetVisibility bool ->
            case model.page of
                AuthPage page ->
                    ( { model | page = AuthPage { page | passwordVisibility = bool } }, Cmd.none )

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

        ToggleKeyboard ->
            case model.page of
                TypingPage typing ->
                    ( { model | page = TypingPage { typing | showKeyboard = not typing.showKeyboard } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangePage page ->
            ( { model | page = page }, Cmd.none )


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

        UpdateStatistic pastDictations ->
            ( { model | statistic = pastDictations }, Cmd.none )

        KickOut ->
            ( { model | page = AuthPage defaultAuth, authorised = False }, Cmd.none )

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
        t =
            model.settings.theme

        body =
            case model.page of
                MenuPage menu ->
                    viewMenu t model menu

                TypingPage typing ->
                    viewTyping t typing model.settings

                TypingStatisticPage past ->
                    viewTypingStatistic t past

                SettingsPage ->
                    viewSettings t model.settings

                StatisticPage hover ->
                    viewStatistic t hover model.statistic

                AuthPage page ->
                    viewAuth t page

                InfoPage ->
                    viewInfo t

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

                AuthPage _ ->
                    "Login"

                InfoPage ->
                    "Info"
    in
    { title = pageTitle ++ " - " ++ "Neo Train"
    , body =
        [ ptMonoLink
        , styleTag t
        , iconTag
        , layoutWith
            { options =
                [ focusStyle
                    { backgroundColor = Nothing
                    , borderColor = Just <| wheat t
                    , shadow = Just { color = primary t, offset = ( 0, 0 ), blur = 0, size = 4 }
                    }
                ]
            }
            [ width fill, height fill, Background.color <| wheat t, Font.color <| black t ]
            (el
                [ centerX
                , centerY
                , Border.color <| black t
                , Border.width 1
                , padding appPadding
                , Border.rounded 16
                , inFront <|
                    el
                        [ alignBottom
                        , alignRight
                        , moveDown 32
                        , alpha 0.2
                        , tooltip "your progress or account could get lost in an update"
                        ]
                    <|
                        text "preview"
                ]
             <|
                body
            )
        ]
    }


viewInfo : Theme -> Element FrontendMsg
viewInfo t =
    column [ topLeftBar [ backButton t ], spacing 16, width (px 512) ]
        [ subTitle "Über Neo"
        , paragraph [] [ text "Neo ist eine ergonomische Tastaturbelegung, welche für die deutsche Sprache optimiert ist. Wenn du noch mehr über Neo erfahren möchstes besuch bitte die Homepage." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://www.neo-layout.org/", label = text "Neo Homepage" }
        , subTitle "Über Mich"
        , paragraph [] [ text "Hallo ich bin Janek aus Magdeburg. Ich studiere Informatik an der OvGU und programmiere super gerne in Elm. Habe diese Web-App aus Spaß gebaut." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21", label = text "Mein Github" }
        ]


viewAuth : Theme -> Auth -> Element FrontendMsg
viewAuth t { username, password, passwordVisibility, failed } =
    let
        inputStyle =
            [ width fill, Background.color <| wheat t, Border.width 1, Border.color <| black t, Border.rounded 0 ]

        eyeButton =
            Input.button [ height fill, padding 10, tooltip "Show password" ]
                { label =
                    materialIcon
                        (if passwordVisibility then
                            Icons.visibility_off

                         else
                            Icons.visibility
                        )
                , onPress = Just <| SetVisibility <| not passwordVisibility
                }

        heading =
            inFront <| row [ centerX, Font.size 72, moveUp 156, Font.underline ] [ text "Neo Train ", el [ moveUp 14, moveLeft 1 ] <| html <| Icon.icon <| toHex <| black t ]
    in
    column [ spacing 32, heading, topRightBar [ infoButton t ] ]
        [ title "Login / Register"
        , case failed of
            NotAsked ->
                none

            WrongUsernameOrPassword ->
                el [ Font.color <| primary t ] <| text "Wrong username or password!"

            UsernameOrPasswordInvalid ->
                paragraph [ Font.color <| primary t, width (px 500) ] [ text "Username or password invalid. Username should be alphanumeric. Password should be length 10, alphanumeric with some special chars (IBM valid character)." ]
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
                , show = passwordVisibility
                }
            ]
        , row [ spacing 16, width fill ]
            [ Input.button
                ([ width fill ] ++ buttonAttributes t)
                { label = el [ centerX ] <| text "Login", onPress = Just <| TryLogin username password }
            , Input.button
                ([ width fill ] ++ buttonAttributes t)
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
styleTag t =
    Html.node "style"
        []
        [ Html.text <| """
*::-webkit-scrollbar {
    width: 24px;
}
*::-webkit-scrollbar-track {
    border-left: 1px solid """ ++ (toHex <| black t) ++ """;
}
*::-webkit-scrollbar-thumb {
  background-color: """ ++ (toHex <| black t) ++ """;
}
"""
        ]


iconTag =
    Html.node "link"
        [ Html.Attributes.rel "shortcut icon", Html.Attributes.type_ "image/x-icon", Html.Attributes.href "/favicon.svg" ]
        []



-- Menu


viewMenu : Theme -> Model -> Menu -> Element FrontendMsg
viewMenu t model menu =
    let
        block label child =
            column [ spacing 8 ] [ subTitle label, child ]

        sidebar =
            case menu.current of
                Just lesson ->
                    column [ height fill, spacing 32 ]
                        [ block "Char count" <| text <| String.fromInt <| String.length lesson.content
                        , block "Word count" <| text <| String.fromInt <| List.length <| String.split " " <| lesson.content
                        , block "Preview" <| paragraph [ width (px 400), height fill ] [ text (lesson.content |> truncate 140) ]
                        , el [ alignRight, alignBottom ] <| squareButton t (OpenBook lesson) (text "Start") 'r'
                        ]

                Nothing ->
                    none

        lessonDoneCount lesson =
            model.statistic
                |> List.filter (\p -> p.lesson == lesson)
                |> List.length

        settingsButton =
            roundedButton t ToSettings (materialIcon Icons.settings) 's'
    in
    column [ spacing 32, topRightBar [ infoButton t, statisticButton t, settingsButton ] ]
        [ title "Dictations"
        , row
            [ spacing 40 ]
            [ column
                [ Border.color <| black t, Border.width 1, height (fill |> maximum 512), scrollbarY, width fill ]
                (lessons |> List.map (\l -> viewMenuItem t (lessonDoneCount l) l))
            , sidebar
            ]
        ]


viewMenuItem : Theme -> Int -> Lesson -> Element FrontendMsg
viewMenuItem t doneCount book =
    let
        charsPerSecond =
            5

        check =
            if doneCount > 1 then
                materialIcon Icons.done_all

            else if doneCount > 0 then
                materialIcon Icons.done

            else
                none
    in
    Input.button
        ([ width fill ] ++ itemAttributes t)
        { label =
            row [ spacing 8 ]
                [ text (book.title ++ " (" ++ printSeconds ((book.content |> String.length |> toFloat) / charsPerSecond) ++ ")")
                , el [ tooltip <| "done this " ++ String.fromInt doneCount ++ "x" ] <| check
                ]
        , onPress = Just <| PreviewBook book
        }



-- Settings


viewSettings : Theme -> Settings -> Element FrontendMsg
viewSettings t settings =
    let
        settingsBlock label child =
            column [ width fill, spacing 8 ]
                [ subTitle label
                , child
                ]

        blockSettingItem label setting =
            viewSettingsItem t label (SetSettings { settings | blockOnError = setting }) (settings.blockOnError == setting)

        layoutSettingItem layout =
            viewSettingsItem t (layoutNames layout) (SetSettings { settings | layout = layout }) (settings.layout == layout)

        themeSettingItem ( label, setting ) =
            viewSettingsItem t label (SetSettings { settings | theme = setting }) (settings.theme == setting)

        logoutButton =
            roundedButton t Logout (materialIcon Icons.logout) 'l'
    in
    column [ topLeftBar [ backButton t, logoutButton ], spacing 48 ]
        [ title "Settings"
        , row [ spacing 48 ]
            [ column
                [ spacing 32, alignTop ]
                [ settingsBlock "Layout Preview" <|
                    column [ width fill, Border.color <| black t, Border.width 1 ] <|
                        List.map layoutSettingItem layouts
                , settingsBlock "Blocking"
                    (column [ width fill, Border.color <| black t, Border.width 1 ]
                        [ blockSettingItem "Waiting for one backspace" OneBackspace
                        , blockSettingItem "Waiting for the correct letter" CorrectLetter
                        ]
                    )
                ]
            , column [ spacing 32, alignTop ]
                [ settingsBlock "Chars left and right of cursor" <|
                    column [ width fill, spacing 8 ]
                        [ slider t 0 50 settings.paddingLeft (\value -> SetSettings { settings | paddingLeft = value })
                        , slider t 0 50 settings.paddingRight (\value -> SetSettings { settings | paddingRight = value })
                        ]
                , settingsBlock "Theme" <|
                    column [ width fill, Border.color <| black t, Border.width 1 ] <|
                        List.map themeSettingItem themes
                ]
            ]
        ]


slider : Theme -> Float -> Float -> Int -> (Int -> msg) -> Element msg
slider t min max value msg =
    let
        padding =
            logBase 10 max |> ceiling
    in
    row [ width fill, spacing 8 ]
        [ el [ monospace ] <| text <| String.padLeft padding ' ' (String.fromInt value)
        , el [ width fill, Border.color <| black t, Border.width 1 ] <|
            Input.slider [ width fill ]
                { onChange = round >> msg
                , label = Input.labelHidden ""
                , min = min
                , max = max
                , value = toFloat value
                , thumb = Input.thumb [ width (px 20), height (px 20), Background.color <| black t ]
                , step = Just 1
                }
        ]


viewSettingsItem t labelText msg active =
    Input.button
        ([ width fill ]
            ++ itemAttributes t
            ++ (if active then
                    [ Background.color <| secondary t, Font.color <| wheat t ]

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


viewTyping : Theme -> Typing -> Settings -> Element FrontendMsg
viewTyping t { dictation, layer, madeError, paused, showKeyboard } settings =
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
                , el [ Background.color <| color t, Font.color <| wheat t ] <| viewChar dictation.current
                , row [ Font.color <| secondary t ] (List.map viewChar next)
                ]

        pausedEl =
            el [ centerX, monospace, Font.size 32 ] <|
                text <|
                    String.pad (settings.paddingLeft + settings.paddingRight + 1) ' ' <|
                        "Paused. Press Any Key"

        pauseButton =
            roundedButton t Pause (materialIcon Icons.pause) 'p'

        keyboardButton =
            roundedButton t ToggleKeyboard (materialIcon Icons.keyboard) 'k'

        hideKeyboardButton =
            roundedButton t ToggleKeyboard (materialIcon Icons.keyboard_hide) 'k'

        playButton =
            roundedButton t Play (materialIcon Icons.play_arrow) 'p'
    in
    column
        [ spacing 64
        , topLeftBar
            [ backButton t
            , if paused then
                playButton

              else
                pauseButton
            , if showKeyboard then
                hideKeyboardButton

              else
                keyboardButton
            ]
        ]
        [ if paused then
            pausedEl

          else
            typewriter
        , if showKeyboard then
            image [ width fill ] { src = layerUrl settings.layout layer, description = "" }

          else
            none
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


viewStatistic : Theme -> Hover -> List PastDictation -> Element FrontendMsg
viewStatistic t hovering statistic =
    let
        viewPast : PastDictation -> Element FrontendMsg
        viewPast past =
            Input.button
                ([ width fill
                 , Element.Events.onMouseEnter <| OnHover [ past ]
                 ]
                    ++ itemAttributes t
                    ++ (if List.member past hovering then
                            mouseOverAttributes t

                        else
                            []
                       )
                )
                { label = text <| (String.fromInt <| points past) ++ "\t" ++ past.lesson.title
                , onPress = Just (ToTypingStatistic past)
                }

        viewHover =
            case median hovering of
                Just h ->
                    column [ spacing 8 ]
                        [ text h.lesson.title
                        , text <| "Points: " ++ (String.fromInt <| points h)
                        ]

                Nothing ->
                    -- placeholder because of hover jitter
                    column [ spacing 8 ] [ text " ", text " " ]
    in
    column [ topLeftBar [ backButton t ], spacing 48, width fill ]
        [ title "Statistic"
        , row [ width fill, spacing 48 ]
            [ column [ spacing 8, width fill, alignTop ]
                [ subTitle "Past Dictations"
                , if List.isEmpty statistic then
                    info "You have no past dictations"

                  else
                    el [ width fill ] <|
                        column
                            [ width fill
                            , height (fill |> maximum 512)
                            , scrollbarY
                            , Border.width 1
                            , Border.color <| black t
                            , Element.Events.onMouseLeave <| OnHover <| []
                            ]
                        <|
                            List.map viewPast statistic
                ]
            , column [ spacing 8, alignTop ] [ subTitle "Point Progression", viewPointsGraph t hovering statistic, viewHover ]
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


smile labelText =
    row [ spacing 8 ] [ materialIcon Icons.sentiment_very_satisfied, text labelText ]


errorPercent { content } errors =
    let
        percent =
            round ((toFloat (List.length errors) / toFloat (String.length content)) * 100)
    in
    String.fromInt percent ++ "%"


viewTypingStatistic : Theme -> PastDictation -> Element FrontendMsg
viewTypingStatistic t past =
    let
        { lesson, errors, duration } =
            past

        grouped =
            errors
                |> List.foldr (\error -> Dict.update error.should (\y -> Just <| Maybe.withDefault [] y ++ [ error ])) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> List.length)
                |> List.reverse

        homeButton =
            roundedButton t Back (materialIcon Icons.home) 'h'
    in
    column
        [ spacing 48
        , topLeftBar [ homeButton, statisticButton t ]
        , bottomCenterBar [ roundedButton t (OpenBook lesson) (materialIcon Icons.refresh) 'r' ]
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
                smile "There are no errors"

              else
                wrappedRow [ spacing 16, width (fill |> maximum 650) ] (grouped |> List.map (viewError t))
            ]
        , column [ spacing 8 ]
            [ subTitle "Points"
            , text <| String.fromInt <| points past
            ]
        ]


topLeftBar =
    inFront << row [ moveUp (appPadding + 21), moveLeft (appPadding + 21), spacing 16 ]


topRightBar =
    inFront << row [ moveUp (appPadding + 21), moveRight (appPadding + 21), spacing 16, alignRight ]


bottomCenterBar =
    inFront << row [ moveDown (appPadding + 21), spacing 16, centerX, alignBottom ]


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
                el [ alignBottom, centerX, moveDown 4, alpha 0.5 ] <|
                    materialIcon Icons.space_bar

        '\n' ->
            el charSize <|
                el [ alignBottom, centerX ] <|
                    materialIcon Icons.keyboard_return

        _ ->
            text <|
                String.fromChar char


viewError : Theme -> ( Char, List TypeError ) -> Element msg
viewError t ( char, typeErrors ) =
    row [ spacing 8, Border.width 1, Border.color <| black t, padding 4, Border.rounded 999 ]
        [ el
            [ monospace
            , Font.color <| wheat t
            , Background.color <| primary t
            , Border.rounded 999
            , width (px 24)
            , height (px 24)
            ]
          <|
            el [ centerX, centerY ] <|
                viewChar char
        , text <| (String.fromInt <| List.length typeErrors) ++ "x"
        ]


viewPointsGraph : Theme -> Hover -> List PastDictation -> Element FrontendMsg
viewPointsGraph t hovering dictations =
    let
        items =
            dictations
                |> bucketStatistic
                |> Dict.toList
    in
    el [ width <| px 300, height fill ] <|
        html <|
            C.chart
                ([ CA.height 300
                 , CA.width 300
                 , CA.margin { top = 8, bottom = 32, left = 48, right = 8 }
                 , CA.padding { top = 4, bottom = 4, left = 4, right = 4 }
                 , CE.onMouseMove (List.concatMap (CI.getData >> Tuple.second) >> OnHover) (CE.getNearest CI.dots)
                 , CE.onMouseLeave (OnHover [])
                 , CE.onMouseUp
                    (List.concatMap (CI.getData >> Tuple.second)
                        >> median
                        >> Maybe.map ToTypingStatistic
                        >> Maybe.withDefault NoOpFrontendMsg
                    )
                    (CE.getNearest CI.dots)
                 ]
                    ++ (if List.isEmpty hovering then
                            []

                        else
                            [ CA.htmlAttrs [ Html.Attributes.style "cursor" "pointer" ] ]
                       )
                )
                [ C.yTicks [ CA.color (toHex <| black t) ]
                , C.yLabels [ CA.color (toHex <| black t) ]
                , C.grid [ CA.color (toHex <| secondary t) ]
                , C.series (Tuple.first >> toFloat)
                    [ C.interpolated (Tuple.second >> medianPoints >> toFloat) [ CA.color <| toHex <| primary t, CA.width 4 ] [ CA.circle, CA.size 8 ]
                        |> C.variation
                            (\_ data ->
                                let
                                    createSet a =
                                        a |> List.map .finished |> List.map Time.posixToMillis |> Set.fromList
                                in
                                if haveCommon (hovering |> createSet) (data |> Tuple.second |> createSet) then
                                    [ CA.circle, CA.size 48, CA.color (toHex <| primary t) ]

                                else
                                    []
                            )
                    ]
                    items
                ]


haveCommon : Set comparable -> Set comparable -> Bool
haveCommon a b =
    not (Set.isEmpty (Set.intersect a b))


median : List PastDictation -> Maybe PastDictation
median pastDictations =
    let
        sorted =
            List.sortBy points pastDictations
    in
    sorted
        |> List.drop (List.length sorted // 2)
        |> List.head


medianPoints : List PastDictation -> Int
medianPoints pastDictations =
    pastDictations
        |> median
        |> Maybe.map points
        |> Maybe.withDefault 0


bucketStatistic : List PastDictation -> Dict Int Bucket
bucketStatistic pastDictations =
    let
        insert : PastDictation -> Dict Int Bucket -> Dict Int Bucket
        insert pastDictation dict =
            let
                posixToBucketKey : Posix -> Int
                posixToBucketKey posix =
                    Time.posixToMillis posix // (1000 * 60 * 10)

                key =
                    posixToBucketKey pastDictation.finished
            in
            if Dict.member key dict then
                Dict.update key (Maybe.map (\v -> pastDictation :: v)) dict

            else
                Dict.insert key [ pastDictation ] dict
    in
    pastDictations |> List.foldl insert Dict.empty


toHex color =
    color
        |> toRgb
        |> (\{ red, green, blue } -> [ red, green, blue ])
        |> List.map (\c -> floor <| c * 256)
        |> List.map Hex.toString
        |> List.map (String.padLeft 2 '0')
        |> String.concat
        |> String.cons '#'


backButton t =
    roundedButton t Back (materialIcon Icons.arrow_back) 'b'


infoButton t =
    roundedButton t (ChangePage InfoPage) (materialIcon Icons.info) 'i'


statisticButton t =
    roundedButton t ToStatistic (materialIcon Icons.query_stats) 't'


roundedButton t onPress label shortcut =
    Input.button
        ([ Border.rounded 999 ] ++ buttonAttributes t ++ accessKey shortcut)
        { label = label, onPress = Just onPress }


squareButton t onPress label shortcut =
    Input.button
        (buttonAttributes t ++ accessKey shortcut)
        { label = label, onPress = Just onPress }


buttonAttributes t =
    [ Border.color <| black t, Border.width 1, Background.color <| wheat t ] ++ itemAttributes t


itemAttributes t =
    [ padding 8
    , mouseOver (mouseOverAttributes t)
    ]


mouseOverAttributes t =
    [ Background.color <| primary t, Font.color <| wheat t ]


accessKey key =
    [ htmlAttribute (Html.Attributes.accesskey key)
    , tooltip <| "Shortcut: <Alt-" ++ String.toUpper (String.fromChar key) ++ ">"
    ]


tooltip string =
    htmlAttribute <| Html.Attributes.title string


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


primary : Theme -> Color
primary =
    resolveColor Primary


secondary =
    resolveColor Secondary


wheat : Theme -> Color
wheat =
    resolveColor White


black =
    resolveColor Black


resolveColor : NamedColor -> Theme -> Color
resolveColor color theme =
    case theme of
        WheatField ->
            case color of
                Primary ->
                    rgb255 243 145 146

                Secondary ->
                    rgb255 175 134 132

                White ->
                    rgb255 255 249 231

                Black ->
                    rgb255 51 51 41

        ElectricFields ->
            case color of
                Primary ->
                    rgb255 180 210 115

                Secondary ->
                    rgb255 176 82 121

                White ->
                    rgb255 46 46 46

                Black ->
                    rgb255 214 214 214

        CandyLand ->
            case color of
                Primary ->
                    rgb255 174 255 74

                Secondary ->
                    rgb255 247 162 172

                White ->
                    rgb255 255 255 255

                Black ->
                    rgb255 91 27 6



--aspect ration =
--    htmlAttribute (Html.Attributes.style "aspect-ration" <| String.fromFloat ration)
--zero4 =
--    { left = 0, top = 0, right = 0, bottom = 0 }
