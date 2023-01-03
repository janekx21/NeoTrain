module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Lamdera
import Lamdera.Json as Decode exposing (Decoder)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
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
init _ key =
    let
        items =
            [ Book "Der Igel" "foo"
            , Book "Das Foo Buch" "Das ist ein etwas längerer Text. Bitte schau mal ob das so passt. Danke."
            , Book "Zucker im Urin" textOne
            , Book "Diana" "Ich bin dolle cute hehe aber sehr dolle"
            , Book "Elm" elmText
            , Book "Special Chars" ":: -- \\\\ // !?!?!? ?! !? ^ ?! ^ ?!!??! ++ -- ++ -- +- -+ ; () () {} [] [] () {} \\\\ // ___ ||| $$ ## <> <> == && <= >= '' '' \"\" \"\" ~~ %% ``` ``` *** (+~}/$|…\\#/$|[%)+?$=)^|](>=\\/{[(`]?-(\\{/(->=_><!^/`~&…_[<_=&[>]^<;'\"#\"$%+|~`+?)(/{*"
            ]
                ++ List.repeat 40 { title = "Foo", content = String.repeat 200 "Bla" }
    in
    ( { key = key
      , items = items
      , page = Menu { current = Nothing }
      , settings = defaultSettings
      , statistic = []
      }
    , Lamdera.sendToBackend FetchSettings
    )


textOne =
    "Ein Professor betonte in seinen Vorlesungen immer wieder, dass man sich als baldiger Arzt vor nichts ekeln darf. Außerdem müsse man beweisen, dass man eine herausragende Beobachtungsgabe hat. Er erklärte, dass man früher nicht die Möglichkeiten besaß, die man heute hat. So musste man beispielsweise mit der Zunge testen, wie viel Zucker im Urin ist. Die Studenten bekamen ein ungutes Gefühl, als der Professor schließlich ein Glas mit einer gelben Flüssigkeit hervorholte. Die Studenten sollten den Urin so testen wie früher. Der Professor machte es vor, tauchte den Zeigefinger in das Glas und leckte anschließend den Finger ab. Die Studenten waren schockiert. Dann hatten sie sich wieder gefasst und jeder machte es dem Professor nach. Manchmal verzog ein Student das Gesicht kurz zu einer Grimasse, aber alle leckten ihre Finger mit dem Urin ab. Dann sagte der Professor: „Ausgezeichnet. Sie alle haben ihren Ekel überwunden. Allerdings gibt es noch große Probleme bei der Beobachtung. Denn niemand hat anscheinend gesehen, dass ich zwar den Zeigefinger in das Glas gesteckt, aber den Mittelfinger abgeleckt habe."


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
            ( { model | page = Menu { current = Just book } }, Cmd.none )

        OpenBook book ->
            ( { model | page = Typing { dictation = bookToDictation book, madeError = False, errors = [], layer = 1, bookTitle = book.title } }, Cmd.none )

        ToMenu ->
            ( { model | page = Menu { current = Nothing } }, Cmd.none )

        ToSettings ->
            ( { model | page = Settings }, Cmd.none )

        KeyDown keyboardKey ->
            case model.page of
                Typing typing ->
                    let
                        maybeTyping =
                            updateDictation keyboardKey model.settings typing

                        next =
                            case maybeTyping of
                                Just page ->
                                    { model | page = Typing page }

                                Nothing ->
                                    { model | page = TypingStatistic typing.errors, statistic = model.statistic ++ [ { errors = typing.errors, bookTitle = typing.bookTitle } ] }
                    in
                    ( next, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp keyboardKey ->
            case model.page of
                Typing typing ->
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
                    ( { model | page = Typing { typing | layer = layer } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetSettings settings ->
            ( { model | settings = settings }, Lamdera.sendToBackend <| UploadSettings settings )

        ToStatistic ->
            ( { model | page = Statistic }, Cmd.none )


updateDictation : KeyboardKey -> UserSettings -> TypingPage -> Maybe TypingPage
updateDictation keyboardKey settings typing =
    case keyboardKey of
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
            if settings.blockOnError == OneBackspace && typing.madeError then
                Just typing

            else if char == typing.dictation.current then
                typing.dictation |> advanceDictation |> Maybe.map (\dictation -> { typing | dictation = dictation, madeError = False })

            else
                Just { typing | madeError = True, errors = typing.errors ++ [ { was = char, should = typing.dictation.current } ] }


bookToDictation : Book -> Dictation
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
        |> Maybe.map (\( nextCurrent, next ) -> { prev = dict.prev ++ String.fromChar dict.current, current = nextCurrent, next = next })


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        DownloadSettings settings ->
            ( { model | settings = settings }, Cmd.none )


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    case model.page of
        Typing _ ->
            Sub.batch
                [ keyDecoder |> Decode.map (KeyDown << toKey) |> Browser.Events.onKeyDown
                , keyDecoder |> Decode.map (KeyUp << toKey) |> Browser.Events.onKeyUp
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
                Menu menu ->
                    viewMenu model menu

                Typing typing ->
                    viewTyping typing model.settings

                TypingStatistic errors ->
                    viewTypingStatistic errors

                Settings ->
                    viewSettings model.settings

                Statistic ->
                    viewStatistic model.statistic
    in
    { title = "JTipp"
    , body =
        [ ptMonoLink
        , layout [ width fill, height fill, Background.color wheat, Font.color black ]
            (el
                [ centerX, centerY, Border.color black, Border.width 1, padding 20, Border.rounded 16 ]
             <|
                body
            )
        ]
    }


ptMonoLink =
    Html.node "link" [ Html.Attributes.href "https://fonts.googleapis.com/css2?family=PT+Mono&display=swap", Html.Attributes.rel "stylesheet" ] []



-- Menu


viewMenu : Model -> MenuPage -> Element FrontendMsg
viewMenu model menu =
    let
        block label child =
            column [ spacing 8 ] [ el [ Font.bold ] <| text label, child ]

        sidebar =
            case menu.current of
                Just book ->
                    column [ height fill, spacing 32 ]
                        [ block "Char count" <| text <| String.fromInt <| String.length book.content
                        , block "Word count" <| text <| String.fromInt <| List.length <| String.split " " <| book.content
                        , block "Preview" <| paragraph [ width (px 400), height fill ] [ text (book.content |> truncate 140) ]
                        , Input.button [ alignRight, alignBottom, Border.color black, Border.width 1, padding 8, mouseOver [ Background.color primary ] ] { onPress = Just (OpenBook book), label = text "Start" }
                        ]

                Nothing ->
                    none
    in
    column [ spacing 32, inFront <| row [ spacing 16, alignRight, moveRight 40, moveUp 40 ] [ statisticButton, settingsButton ] ]
        [ title "Dictations"
        , row
            [ spacing 40 ]
            [ column
                [ Border.color black, Border.width 1, height (fill |> maximum 512), scrollbarY, width fill ]
                (model.items |> List.map viewMenuItem)
            , sidebar
            ]
        ]


viewMenuItem : Book -> Element FrontendMsg
viewMenuItem book =
    let
        charsPerSecond =
            4
    in
    Input.button
        [ width fill, padding 8, mouseOver [ Background.color primary ] ]
        { label = text (book.title ++ " (" ++ printSeconds ((book.content |> String.length |> toFloat) / charsPerSecond) ++ ")")
        , onPress = Just <| PreviewBook book
        }



-- Settings


viewSettings : UserSettings -> Element FrontendMsg
viewSettings settings =
    let
        settingsBlock label child =
            column [ width fill, spacing 8 ]
                [ el [ Font.bold ] <|
                    text label
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
    in
    column [ inFront <| el [ moveLeft 40, moveUp 40 ] <| backButton, spacing 48 ]
        [ title "Settings"
        , settingsBlock
            "Layout"
            (column [ width fill, Border.color black, Border.width 1 ]
                (layouts |> List.map (\( name, layout ) -> layoutSettingItem name layout))
            )
        , settingsBlock "Blocking"
            (column [ width fill, Border.color black, Border.width 1 ]
                [ blockSettingItem "Waiting for one backspace" OneBackspace
                , blockSettingItem "Waiting for the correct letter" CorrectLetter
                ]
            )
        , settingsBlock "Padding left" <|
            paddingSlider settings.paddingLeft (\value -> SetSettings { settings | paddingLeft = round value })
        , settingsBlock "Padding right" <|
            paddingSlider settings.paddingRight (\value -> SetSettings { settings | paddingRight = round value })
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


title labelText =
    el [ Font.size 32 ] <| text labelText


printSeconds seconds =
    if seconds > 60 then
        ((seconds / 60) |> round |> String.fromInt) ++ "min"

    else
        (seconds |> round |> String.fromInt) ++ "sec"



-- Typing


viewTyping : TypingPage -> UserSettings -> Element FrontendMsg
viewTyping typing settings =
    let
        color =
            if typing.madeError then
                primary

            else
                secondary
    in
    column [ spacing 64, inFront <| el [ moveLeft 40, moveUp 40 ] <| backButton ]
        [ row [ monospace, Font.size 32 ]
            [ text (typing.dictation.prev |> String.right settings.paddingLeft |> String.padLeft settings.paddingLeft ' ')
            , el [ Background.color color, Font.color wheat ] <| text <| String.fromChar typing.dictation.current
            , el [ Font.color secondary ] <| text (typing.dictation.next |> String.left settings.paddingRight |> String.padRight settings.paddingRight ' ')
            ]
        , image [ width fill ] { src = layerUrl settings.layout typing.layer, description = "" }
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


viewStatistic : List PastDictation -> Element FrontendMsg
viewStatistic pastDictations =
    column
        [ inFront <| el [ moveLeft 40, moveUp 40 ] <| backButton ]
        (if List.isEmpty pastDictations then
            [ text "no past dictations" ]

         else
            List.map (\dictation -> text dictation.bookTitle) pastDictations
        )


viewTypingStatistic : List TypeError -> Element FrontendMsg
viewTypingStatistic errors =
    let
        grouped =
            errors
                |> List.foldr (\error -> Dict.update error.should (\y -> Just <| Maybe.withDefault [] y ++ [ error ])) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> List.length)
                |> List.reverse
    in
    column [ spacing 16 ]
        [ title "das sind deine Statistiken"
        , column [ spacing 8 ] (grouped |> List.map viewError)
        , el [ centerX ] <| button (Just ToMenu) (materialIcon Icons.arrow_back)
        ]


viewError : ( Char, List TypeError ) -> Element msg
viewError ( char, typeErrors ) =
    row [ spacing 16 ]
        [ el
            [ monospace
            , Font.color wheat
            , Background.color primary
            , Border.rounded 999
            , width (px 24)
            , height (px 24)
            ]
          <|
            el
                [ centerX
                , centerY
                ]
            <|
                text <|
                    String.fromChar char
        , text "Fehler"
        , text <| String.fromInt <| List.length typeErrors
        ]



-- Common


lineClamp lines =
    -- todo make work when needed
    [ htmlAttribute (Html.Attributes.style "display" "-webkit-box")
    , htmlAttribute (Html.Attributes.style "-webkit-line-clamp" (String.fromInt lines))
    , htmlAttribute (Html.Attributes.style "-webkit-box-orient" "vertical")
    , htmlAttribute (Html.Attributes.style "overflow" "hidden")
    ]


backButton =
    button (Just ToMenu) (materialIcon Icons.arrow_back)


settingsButton =
    button (Just ToSettings) (materialIcon Icons.settings)


statisticButton =
    button (Just ToStatistic) (materialIcon Icons.query_stats)


button onPress label =
    Input.button [ Background.color wheat, Border.color black, Border.width 1, Border.rounded 999, padding 8, mouseOver [ Background.color primary ] ] { label = label, onPress = onPress }


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


zero4 =
    { left = 0, top = 0, right = 0, bottom = 0 }
