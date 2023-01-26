module Common exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
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


topLeftBar =
    inFront << row [ moveUp (appPadding + 21), moveLeft (appPadding + 21), spacing 16 ]


topRightBar =
    inFront << row [ moveUp (appPadding + 21), moveRight (appPadding + 21), spacing 16, alignRight ]


bottomCenterBar =
    inFront << row [ moveDown (appPadding + 21), spacing 16, centerX, alignBottom ]


toHex color =
    color
        |> toRgb
        |> (\{ red, green, blue } -> [ red, green, blue ])
        |> List.map (\c -> floor <| c * 256)
        |> List.map Hex.toString
        |> List.map (String.padLeft 2 '0')
        |> String.concat
        |> String.cons '#'


backButton t msg =
    roundedButton t msg (materialIcon Icons.arrow_back) 'b'


infoButton t =
    roundedButton t (ChangePage InfoPage) (materialIcon Icons.info) 'i'


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


appPadding =
    24


pageTitle page =
    case page of
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


defaultAuth : Auth
defaultAuth =
    { username = "", failed = NotAsked, password = "", passwordVisibility = False }


defaultSettings : Settings
defaultSettings =
    { blockOnError = CorrectLetter
    , fontSize = 32
    , paddingLeft = 20
    , paddingRight = 20
    , layout = Neo
    , theme = ElectricFields
    }
