module Common exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Layouts
import Hex
import Html.Attributes
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import Types exposing (..)



-- Util


points : PastDictation -> Int
points past =
    let
        err =
            toFloat (List.length past.errors) / toFloat (String.length past.lesson.content)
    in
    max 0 <| round <| ((charsPerMinute past.lesson past.duration / 5) - (err * 5)) * 10


charsPerMinute book duration =
    let
        chars =
            String.length book.content

        durationInMin =
            duration / 60
    in
    toFloat chars / durationInMin


wordsPerMinute book duration =
    let
        words =
            book.content |> String.words |> List.length

        durationInMin =
            duration / 60
    in
    toFloat words / durationInMin


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


printSeconds seconds =
    if seconds > 60 then
        ((seconds / 60) |> round |> String.fromInt) ++ "min"

    else
        (seconds |> round |> String.fromInt) ++ "sec"


printTime seconds =
    let
        min =
            String.fromInt (round seconds // 60)

        sec =
            (round seconds |> modBy 60) |> String.fromInt |> String.padLeft 2 '0'
    in
    min ++ ":" ++ sec


topLeftBar =
    inFront << row [ moveUp (appPadding + 21), moveLeft (appPadding + 21), spacing 16 ]


topRightBar =
    inFront << row [ moveUp (appPadding + 21), moveRight (appPadding + 21), spacing 16, alignRight ]


bottomCenterBar =
    inFront << row [ moveDown (appPadding + 21), spacing 16, centerX, alignBottom ]


toHex =
    toRgb
        >> (\{ red, green, blue } -> [ red, green, blue ])
        >> List.map (\c -> c * 255)
        >> List.map floor
        >> List.map Hex.toString
        >> List.map (String.padLeft 2 '0')
        >> String.concat
        >> String.cons '#'


backButton t msg =
    roundedButton t msg (materialIcon Icons.arrow_back) 'b'


infoButton t msg =
    roundedButton t msg (materialIcon Icons.info) 'i'


statisticButton t =
    roundedButton t (ChangePage <| StatisticPage []) (materialIcon Icons.query_stats) 't'


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
    let
        rgb =
            case theme.name of
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
                            rgb255 82 97 53

                        Secondary ->
                            rgb255 105 35 70

                        White ->
                            rgb255 209 209 209

                        Black ->
                            rgb255 41 41 41

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

                NeoClassic ->
                    case color of
                        Primary ->
                            rgb255 6 136 156

                        Secondary ->
                            rgb255 74 164 74

                        White ->
                            rgb255 255 255 255

                        Black ->
                            rgb255 0 0 0
    in
    if theme.dark then
        invertLightness rgb

    else
        rgb


invertLightness : Color -> Color
invertLightness color =
    toRgb color
        |> Color.fromRgba
        |> Color.toHsla
        |> (\hsla -> { hsla | lightness = 1 - hsla.lightness })
        |> Color.fromHsla
        |> Color.toRgba
        |> fromRgb



-- Data


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

        toLesson : Layout -> ( String, String, List String ) -> Lesson
        toLesson layout ( _, must, words ) =
            Lesson (layoutNames layout ++ " " ++ must) (String.join " " words)

        toLessons : ( Layout, List ( String, String, List String ) ) -> List Lesson
        toLessons ( layout, list ) =
            list |> List.map (toLesson layout)

        wordLessons =
            Generated.Layouts.data |> List.concatMap toLessons
    in
    wordLessons
        ++ [ Lesson "Ein Satz" "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern"
           , Lesson "Spezial Zeichen" ":: -- \\\\ // !?!?!? ?! !? ^ ?! ^ ?!!??! ++ -- ++ -- +- -+ ; () () {} [] [] () {} \\\\ // ___ ||| $$ ## <> <> == && <= >= '' '' \"\" \"\" ~~ %% ``` ``` *** (+~}/$|\\#/$|[%)+?$=) ^|](>=\\/{[(`]?-(\\{/(-> =_><!^/`~&_[<_=&[>] ^<;'\"#\"$%+|~`+?)(/{*"
           , Lesson "Junge und Berg von Chat GPT" "Es war einmal ein kleiner Junge, der in einem Dorf am Fuße eines großen Berges wohnte. Eines Tages beschloss er, den Berg zu besteigen, um zu sehen, was oben war. Er packte seinen Rucksack mit Proviant und Wasser und begann seine Reise. Der Aufstieg war beschwerlich, aber er gab nicht auf. Nach vielen Stunden erreichte er die Spitze des Berges und was er sah, übertraf seine kühnsten Träume. Er sah unendliche Wälder, kristallklare Flüsse und Täler voller wilder Blumen. Er beschloss, dass er immer wieder hierher zurückkehren würde, um die Schönheit dieses Ortes zu genießen."
           , Lesson "Klimawandel Debatte von Chat GPT" "Die Debatte um den Klimawandel hat in den letzten Jahren stark zugenommen. Experten sind sich einig, dass der Ausstoß von Treibhausgasen und die Abholzung von Wäldern die Erderwärmung beschleunigen. Regierungen auf der ganzen Welt haben sich verpflichtet, Maßnahmen zu ergreifen, um diesem Problem entgegenzuwirken. Ein wichtiger Schritt ist die Förderung erneuerbarer Energien und die Verringerung des CO2-Ausstoßes. Auch die Wiederaufforstung von Wäldern und die Schaffung von Schutzgebieten können dazu beitragen, den Klimawandel zu bekämpfen."
           , Lesson "Elm Zeichen" elmText
           , Lesson "Debug Text" "foobar"
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
    [ ( "Wheat Field", WheatField )
    , ( "Electric Fields", ElectricFields )
    , ( "Candy Land", CandyLand )
    , ( "Neo Classic", NeoClassic )
    ]


appPadding =
    24


pageTitle page =
    case page of
        MenuPage _ ->
            "Menu"

        TypingPage _ ->
            "Tippen"

        TypingStatisticPage _ ->
            "Typing Statistic"

        SettingsPage _ ->
            "Einstellungen"

        StatisticPage _ ->
            "Statistik"

        AuthPage _ ->
            "Login"

        InfoPage ->
            "Info"


defaultAuth : AuthModel
defaultAuth =
    { username = "", failed = NotAsked, password = "", passwordVisibility = False }


defaultSettings : Settings
defaultSettings =
    { blockOnError = CorrectLetter
    , fontSize = 32
    , paddingLeft = 20
    , paddingRight = 20
    , layout = Neo
    , theme = { name = ElectricFields, dark = True }
    }


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
