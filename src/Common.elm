module Common exposing (..)

import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed
import Generated.Layouts
import Hex
import Html.Attributes
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import Time exposing (Posix)
import Translation exposing (..)
import Types exposing (..)



-- Util


points : PastDictation -> Int
points past =
    let
        err =
            errorRate past

        cpm =
            charsPerMinute past.lesson past.duration

        p1 =
            (cpm * 0.2 - err * 110) * 10

        logged =
            logBase 2 p1 * 10 + p1
    in
    max 0 <| round <| logged


errorRate : PastDictation -> Float
errorRate { errors, lesson } =
    toFloat (List.length errors) / toFloat (String.length lesson.content)


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


viewChar char opacity =
    let
        charSize =
            [ htmlAttribute <| Html.Attributes.style "width" "0.6em"
            , htmlAttribute <| Html.Attributes.style "height" "1em"
            , alpha opacity
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
                el [ centerX, centerY ] <|
                    materialIcon Icons.keyboard_return

        '\t' ->
            el charSize <|
                el [ centerX, centerY ] <|
                    materialIcon Icons.keyboard_tab

        _ ->
            el charSize <|
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


topLeftBar device =
    let
        goingRight =
            case device.class of
                Phone ->
                    32

                _ ->
                    0
    in
    inFront << row [ moveUp (appPadding device + 21), moveLeft (appPadding device + 21 - goingRight), spacing 16 ]


topRightBar device =
    let
        goingLeft =
            case device.class of
                Phone ->
                    32

                _ ->
                    0
    in
    inFront << row [ moveUp (appPadding device + 21), moveRight (appPadding device + 21 - goingLeft), spacing 16, alignRight ]


bottomCenterBar device =
    inFront << row [ moveDown (appPadding device + 21), spacing 16, centerX, alignBottom ]


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


translateButton t msg =
    roundedButton t msg (materialIcon Icons.translate) 't'


statisticButton t =
    roundedButton t (ChangePage <| StatisticPage []) (materialIcon Icons.query_stats) 't'


roundedButton t onPress label shortcut =
    Element.Keyed.el [] <|
        ( String.fromChar shortcut
        , Input.button
            (buttonAttributes t ++ [ Border.rounded <| t.rounding * 2 ] ++ accessKey shortcut)
            { label = label, onPress = Just onPress }
        )


squareButton t onPress label shortcut =
    Input.button
        (buttonAttributes t ++ accessKey shortcut)
        { label = label, onPress = Just onPress }


logoutButton t msg =
    roundedButton t msg (materialIcon Icons.logout) 'l'


buttonAttributes t =
    [ Background.color <| wheat t ] ++ itemBorder t ++ itemAttributes t


itemAttributes t =
    [ padding 8
    , mouseOver (mouseOverAttributes t)
    , Border.rounded (t.rounding - 4)
    ]


itemBorder t =
    [ Border.width t.borderWidth
    , Border.rounded (t.rounding + t.borderWidth - 4)
    , Border.color <| black t
    ]


mouseOverAttributes t =
    [ Background.color <| primary t, Font.color <| wheat t ]


accessKey key =
    let
        label =
            String.toUpper (String.fromChar key)
    in
    [ htmlAttribute (Html.Attributes.accesskey key)
    , tooltip <| "Shortcut: <Alt-" ++ label ++ "> or <Shift-Alt-" ++ label ++ ">"
    ]


tooltip string =
    htmlAttribute <| Html.Attributes.title string


title labelText =
    paragraph [ Font.size 32, width shrink ] [ text labelText ]


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


monospace font =
    Font.family [ Font.typeface <| monoFontName font, Font.monospace ]



-- COLORS


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
                    invertLightness <|
                        invertLightnessLog 1.1 <|
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

                Dracula ->
                    invertLightness <|
                        case color of
                            Primary ->
                                rgb255 80 250 123

                            Secondary ->
                                rgb255 255 121 198

                            Black ->
                                rgb255 248 248 242

                            White ->
                                rgb255 40 42 54

                Contrast ->
                    case color of
                        Primary ->
                            rgb255 254 158 24

                        Secondary ->
                            rgb255 4 200 200

                        Black ->
                            rgb255 0 0 0

                        White ->
                            rgb255 255 255 255
    in
    if theme.dark then
        invertLightness rgb

    else
        rgb


invertLightness : Color -> Color
invertLightness =
    invertLightnessLog 12


invertLightnessLog : Float -> Color -> Color
invertLightnessLog a =
    let
        func x =
            logBase a (1 - x * (1 - 1 / a)) + 1
    in
    mapColorLightness func


mapColorLightness : (Float -> Float) -> Color -> Color
mapColorLightness func color =
    toRgb color
        |> Color.fromRgba
        |> Color.toHsla
        |> (\hsla -> { hsla | lightness = func hsla.lightness })
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

        toLesson : Layout -> ( Int, String, List String ) -> Lesson
        toLesson layout ( index, must, words ) =
            Lesson (Just layout) (layoutNames layout ++ " " ++ must ++ " " ++ String.fromInt index) (String.join " " words)

        toLessons : ( Layout, List ( Int, String, List String ) ) -> List Lesson
        toLessons ( layout, list ) =
            list |> List.map (toLesson layout)

        wordLessons =
            Generated.Layouts.data |> List.concatMap toLessons |> List.filter (\l -> not <| String.isEmpty l.content)
    in
    wordLessons
        ++ [ Lesson Nothing "Nummern und Wörter" "Zehn 10 und 5 ist 15. Dann noch eine Zwei 2 und eine 3 und 4 4ren oder waren es doch eher 80? Warum kann ich nicht einfach 20 oder 55 oder 89 nehmen? 70er und 80er und manchmal auch die 90er. 6"
           , Lesson Nothing "Nummern" "1581023402052935065005866138134109387560173"
           , Lesson Nothing "Ein Satz" "Franz jagt im komplett verwahrlosten Taxi quer durch Bayern"
           , Lesson Nothing "Weltrekord" "Wusstest du, dass der längste Slackline-Weg über 1 Kilometer gespannt war? Ein unglaublicher Weltrekord, der Balance und Mut erforderte!"
           , Lesson Nothing "Javascript Zeichen" "; {} () [] , . = += - -= * *= / /= % == === != !== > < >= <= && || ! ++ -- ? : \\ '' \"\" // /* */"
           , Lesson Nothing "Musik" "In der Welt der Musik gibt es unendliche Möglichkeiten zur kreativen Entfaltung. Klaviere und Violinen, Trommeln und Flöten fügen sich zu harmonischen Melodien. Töne werden zu Emotionen, und Musiker teilen ihre Leidenschaft mit der Welt. Die Musik verbindet uns und lässt unsere Herzen im Takt schlagen."
           , Lesson Nothing "Netflix" "Wusstest du, dass Netflix an einem Tag über 1 Milliarde Stunden Streaming-Inhalte liefert? Das entspricht 115.000 Jahren Nonstop-Filme und Serien! Außerdem begann Netflix ursprünglich als DVD-Verleihdienst. Heute hat es mehr als 200 Millionen Abonnenten weltweit. Beeindruckend, oder?"
           , Lesson Nothing "Techno" "Techno-Musik, auch als elektronische Tanzmusik bekannt, wurde in den späten 1980er Jahren in Detroit und Berlin geboren. Ein interessanter Fakt ist, dass der berühmte DJ Carl Cox 1 Million Pfund für seine linke Hand versichert hat, da sie seine Karriere definiert. Außerdem war die längste Techno-Party der Welt 30 Tage lang und fand 2018 in Berlin statt."
           , Lesson Nothing "Symbols" "In der Mathematik sind griechische Symbole von großer Bedeutung. Zum Beispiel repräsentiert das griechische Alpha (α) oft Unbekanntes in Gleichungen. Das Summensymbol Σ zeigt eine Summe an, während das Integralzeichen ∫ den Flächeninhalt unter einer Kurve beschreibt. Vertrautheit mit diesen Symbolen ist für Mathematiker unerlässlich."
           , Lesson Nothing "Funktionale Programmierung" "Die funktionale Programmierung ist ein Programmierparadigma, bei dem Programme hauptsächlich aus Funktionen bestehen. Daten werden als unveränderbare Werte behandelt, und Funktionen haben keine Seiteneffekte. Dadurch ist die funktionale Programmierung gut geeignet für parallele und nebenläufige Systeme. Bekannte funktionale Programmiersprachen sind z.B. Haskell, Lisp und Clojure."
           , Lesson Nothing "New York" "Ein Tauchgang in die faszinierende Unterwasserwelt vor New York City ist eine unvergessliche Erfahrung. Die Strömungen des Atlantiks offenbaren eine erstaunliche Vielfalt mariner Lebensformen, von farbenfrohen Korallenriffen bis zu majestätischen Mantarochen. Während Sie in die Tiefe gleiten, entdecken Sie versunkene Schiffswracks und antike Überreste. Die Meeresbewohner, darunter Barrakudas und Seepferdchen, begleiten Ihren Abstieg in diese geheimnisvolle Welt. Ein Tauchgang vor der Skyline von Manhattan ist ein wahrhaft magisches Erlebnis, das die Schönheit der Stadt von einer neuen Perspektive zeigt."
           , Lesson Nothing "Junge und Berg" "Es war einmal ein kleiner Junge, der in einem Dorf am Fuße eines großen Berges wohnte. Eines Tages beschloss er, den Berg zu besteigen, um zu sehen, was oben war. Er packte seinen Rucksack mit Proviant und Wasser und begann seine Reise. Der Aufstieg war beschwerlich, aber er gab nicht auf. Nach vielen Stunden erreichte er die Spitze des Berges und was er sah, übertraf seine kühnsten Träume. Er sah unendliche Wälder, kristallklare Flüsse und Täler voller wilder Blumen. Er beschloss, dass er immer wieder hierher zurückkehren würde, um die Schönheit dieses Ortes zu genießen."
           , Lesson Nothing "Klimawandel Debatte" "Die Debatte um den Klimawandel hat in den letzten Jahren stark zugenommen. Experten sind sich einig, dass der Ausstoß von Treibhausgasen und die Abholzung von Wäldern die Erderwärmung beschleunigen. Regierungen auf der ganzen Welt haben sich verpflichtet, Maßnahmen zu ergreifen, um diesem Problem entgegenzuwirken. Ein wichtiger Schritt ist die Förderung erneuerbarer Energien und die Verringerung des CO2-Ausstoßes. Auch die Wiederaufforstung von Wäldern und die Schaffung von Schutzgebieten können dazu beitragen, den Klimawandel zu bekämpfen."
           , Lesson Nothing "Pauls Wanderung" "Das schöne Wetter lockte die Menschen nach draußen. Die Sonne strahlte am blauen Himmel und ein leichter Wind wehte durch die Bäume. Es war ein perfekter Tag, um die Natur zu genießen.\nIn einem kleinen Dorf, umgeben von grünen Wiesen und sanften Hügeln, lebte ein junger Mann namens Paul. Er war ein aufgeschlossener und neugieriger Mensch, der immer auf der Suche nach neuen Abenteuern war.\nEines Tages beschloss Paul, einen Ausflug in den nahegelegenen Wald zu machen. Er schnürte seine Wanderschuhe, packte seinen Rucksack und machte sich auf den Weg. Der Wald war ein wahrer Zauberwald, voller exotischer Pflanzen und geheimnisvoller Geräusche.\nPaul wanderte stundenlang durch den dichten Wald, genoss die Stille und die frische Luft. Plötzlich hörte er ein leises Rascheln hinter sich. Er drehte sich um und entdeckte eine Gruppe neugieriger Eichhörnchen, die ihm aus sicherer Entfernung zuschauten.\nFasziniert blieb Paul stehen und beobachtete die niedlichen Tiere. Sie spielten und sprangen von Ast zu Ast. Paul holte eine kleine Tüte mit Nüssen aus seinem Rucksack und streute sie vorsichtig auf den Boden. Die Eichhörnchen kamen neugierig näher und begannen, die Nüsse zu sammeln.\nPaul lächelte und fühlte sich glücklich, diese besondere Begegnung erleben zu dürfen. Er wusste, dass er diesen Moment nie vergessen würde. Mit einem zufriedenen Gefühl setzte er seinen Weg fort und erkundete weiter den Wald.\nDer Tag verging wie im Flug, und als die Sonne langsam unterging, machte sich Paul auf den Rückweg zum Dorf. Er fühlte sich erfüllt von den Eindrücken und Erlebnissen des Tages. Der Wald hatte ihm eine neue Perspektive auf die Schönheit der Natur geschenkt.\nAls Paul schließlich in sein Dorf zurückkehrte, war er müde, aber glücklich. Er hatte gelernt, wie wichtig es ist, die kleinen Dinge im Leben zu schätzen und sich Zeit für Abenteuer und Erkundungen zu nehmen."
           , Lesson Nothing "Unserer Erde" "Die Erde ist der dritte Planet in unserem Sonnensystem und der einzige bekannte Planet, auf dem Leben existiert. Mit einem Durchmesser von etwa 12.742 Kilometern und einer Oberfläche von rund 510 Millionen Quadratkilometern ist die Erde ein faszinierender Ort.\nUnser Planet ist größtenteils von Wasser bedeckt, etwa 71 Prozent der Oberfläche sind von Ozeanen umgeben. Es gibt sieben Kontinente, darunter Asien, Afrika, Nordamerika, Südamerika, Europa, Australien und die Antarktis.\nUnsere Atmosphäre, eine Schicht aus Gasen, umgibt die Erde und spielt eine entscheidende Rolle für das Leben auf unserem Planeten. Sie besteht hauptsächlich aus Stickstoff (etwa 78 Prozent) und Sauerstoff (etwa 21 Prozent).\nDie Erde ist auch der einzige bekannte Ort im Universum, der flüssiges Wasser in ausreichender Menge aufweist. Wasser ist von entscheidender Bedeutung für das Leben und spielt eine wichtige Rolle bei zahlreichen geologischen Prozessen auf der Erde.\nUnser Planet beherbergt eine beeindruckende Vielfalt an Lebensformen. Von winzigen Bakterien bis zu majestätischen Walen und riesigen Bäumen gibt es eine unglaubliche Artenvielfalt auf der Erde. Die Erhaltung der Biodiversität ist daher von großer Bedeutung, um das Gleichgewicht und die Gesundheit unseres Planeten zu bewahren.\nDie Erde ist auch von geologischer Bedeutung. Sie verfügt über Kontinente, die auf den ozeanischen Platten schwimmen. Diese Platten bewegen sich langsam im Laufe der Zeit, was zur Entstehung von Gebirgen, Erdbeben und Vulkanen führt.\nEs ist wichtig, dass wir unseren Planeten schützen und nachhaltige Praktiken entwickeln, um die Umwelt zu erhalten. Jeder von uns kann einen Beitrag leisten, sei es durch Recycling, Energieeinsparung oder den Schutz natürlicher Lebensräume."
           , Lesson Nothing "Das QWERTY Layout" "Das QWERTY-Tastaturlayout ist das gebräuchlichste Tastaturlayout für englischsprachige Länder und wird auch in vielen anderen Ländern verwendet. Die Geschichte dieses Layouts reicht bis in das 19. Jahrhundert zurück.\nDie erste Schreibmaschine wurde 1868 von Christopher Latham Sholes erfunden. Sholes war ein US-amerikanischer Erfinder und Journalist. Die frühen Schreibmaschinen hatten mechanische Tasten, die auf Hebeln montiert waren. Diese Hebel bewegten ein Metallstück, das mit einem Buchstaben oder einer Zeichenplatte verbunden war, um das entsprechende Zeichen auf das Papier zu drucken.\nAls die Schreibmaschine immer populärer wurde, traten jedoch Probleme auf. Die Hebel, die benachbarte Buchstaben wie \" E \" und \" R \" aktivierten, neigten dazu, sich beim schnellen Tippen zu verheddern. Dies führte zu häufigen Verstopfungen und Verzögerungen beim Schreiben.\nUm dieses Problem zu lösen, entwickelte Sholes das QWERTY-Layout. Der Name leitet sich von den ersten sechs Buchstaben in der oberen linken Ecke der Tastatur ab. Durch die Anordnung der Buchstaben auf diese Weise konnte Sholes die häufig verwendeten Buchstaben voneinander trennen und so die Verwicklungen der Hebel reduzieren.\nDas QWERTY-Layout wurde 1873 mit der Einführung der Remington No. 2-Schreibmaschine, einer der ersten kommerziell erfolgreichen Schreibmaschinen, weit verbreitet. Dieses Layout wurde schnell zur Norm und setzte sich als Standard in der Schreibmaschinenindustrie durch.\nObwohl das QWERTY-Layout heute noch verwendet wird, wird es oft kritisiert. Einige behaupten, dass es ineffizient sei und zu Ermüdung und Verletzungen führen könne, da die häufigsten Buchstaben nicht optimal platziert seien. Es wurden alternative Tastaturlayouts wie das Dvorak Simplified Keyboard entwickelt, um diese Probleme zu beheben. Dennoch bleibt das QWERTY-Layout aufgrund seiner weit verbreiteten Akzeptanz und seiner historischen Bedeutung weiterhin das dominierende Tastaturlayout."
           , Lesson Nothing "English: Ada Lovelace" "Ada Lovelace, born in 1815, was a very smart girl from London, England. She was the only child of a famous poet named Lord Byron. Ada loved math and computers. She worked on a machine made by a man named Charles Babbage, called the Analytical Engine. This machine was supposed to do calculations faster than humans could do by hand. Ada didn't just read about the machine, she translated a French document about it into English and added her own ideas. She thought that the machine could not only do calculations but also use symbols and words. This was a big deal because it meant that the machine could be used for more than just numbers. People didn't pay much attention to Ada's work when she was alive. But later, when people started to understand what she had done, they said she was the first programmer. This means she was the first person to write instructions for a machine to follow. In 1979, a new computer language was named after her, Ada, to honor her work. Ada passed away when she was 36 years old. But her work lives on. Every year, on the second Tuesday in October, people remember her and other women who made important contributions to science, technology, engineering, and math."
           , Lesson Nothing "English: Alan Turing" "Alan Turing was a very smart man born on June 23, 1912, in London, England. He was a mathematician and played a key role in breaking enemy codes during World War II. He went to a famous school called Sherborne School when he was 13 and showed great interest in math and science. After that, he went to King's College in Cambridge, England, where he studied from 1931 to 1934. One of his biggest accomplishments was a paper he wrote in 1936. He proved that there are things in math that we can't figure out using a universal method. This paper was very important for the development of computer science and artificial intelligence. During the war, Turing worked at a place called Bletchley Park, where they broke enemy codes. He invented several ways to break these codes, which helped the Allies win many battles. In 1946, Turing designed the first electronic computer, the Automatic Computing Engine (ACE). He also wrote a paper asking if computers could ever think, and designed a way to judge artificial intelligence. This method is still used today. Alan Turing died on June 7, 1954. Even though he's gone, his work continues to influence the fields of computer science and artificial intelligence."
           , Lesson Nothing "Spezial Zeichen" ":: -- \\\\ // !?!?!? ?! !? ^ ?! ^ ?!!??! ++ -- ++ -- +- -+ ; () () {} [] [] () {} \\\\ // ___ ||| $$ ## <> <> == && <= >= '' '' \"\" \"\" ~~ %% ``` ``` *** (+~}/$|\\#/$|[%)+?$=) ^|](>=\\/{[(`]?-(\\{/(-> =_><!^/`~&_[<_=&[>] ^<;'\"#\"$%+|~`+?)(/{*"
           , Lesson Nothing "Griechisches Alphabet" "ABΓΔEZHΘIKΛMNΞOΠPΣTYΦXΨΩ αβγδεζηθικλμvξoπρστυφχψω ΞζΘωΠΔυPαΨMΛoYXKσyφITεψOBηv ωYyπXBμθvδHΦPσIζρΞTψεAφKαΩλO"
           , Lesson Nothing "Aussagenlogik" "p ¬p (p ∧ q) (p ∨ q) ¬(p ∧ q) (q ∧ ¬q) (¬(p ∧ q) ⇒ (q ∧ ¬q))"
           , Lesson Nothing "Elm Zeichen" elmText
           , Lesson Nothing "Java Fragments" """// Basic Variable Assignment
int age = 25;
String name = "John";
double salary = 50000.0;
// Conditional Statements
if (age >= 18) {
\tSystem.out.println("You are an adult.");
} else {
\tSystem.out.println("You are a minor.");
}
// Loops
for (int i = 1; i <= 10; i++) {
\tSystem.out.print(i + " ");
}
// Function Definition
public int add(int a, int b) {
\treturn a + b;
}
// Arrays
int[] numbers = {1, 2, 3, 4, 5};
String[] fruits = {"apple", "banana", "cherry"};
// Object-Oriented Programming
class Person {
\tString name;
\tint age;

\tvoid sayHello() {
\tSystem.out.println("Hello, my name is " + name);
\t}
}
// Creating an instance of the Person class
Person person1 = new Person();
person1.name = "Alice";
person1.age = 30;
person1.sayHello();
// Exception Handling
try {
\tint result = 10 / 0;
} catch (ArithmeticException e) {
\tSystem.err.println("Error: Division by zero");
}"""
           , Lesson Nothing "Debug Text" "foobar"
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

        Noted ->
            "Noted"


layouts =
    [ Neo, Bone, NeoQwertz, AdNW, KOY, NeoQwerty, Vou, Mine, Noted ]


themes =
    [ ( "Wheat Field", WheatField )
    , ( "Electric Fields", ElectricFields )
    , ( "Candy Land", CandyLand )
    , ( "Neo Classic", NeoClassic )
    , ( "Dracula", Dracula )
    , ( "Contrast", Contrast )
    ]


appPadding : Device -> number
appPadding device =
    ifMobile device 8 24


spacer : Int -> Element msg
spacer n =
    el [ width (px n), height (px n) ] none


topBarPadding =
    paddingEach { top = 24, bottom = 0, left = 0, right = 0 }


mobileRow : Device -> List (Attribute msg) -> List (Element msg) -> Element msg
mobileRow device =
    ifMobile device column row


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
    , theme =
        { name = ElectricFields
        , dark = True
        , rounding = 10
        , borderWidth = 1
        , monoFont = PTMono
        }
    , language = German
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

                Noted ->
                    "noted"
    in
    "https://dl.neo-layout.org/grafik/bilder-einzeln/flat/" ++ name ++ "-" ++ String.fromInt layer ++ "-tkl.svg"


aspect w h =
    htmlAttribute <| Html.Attributes.style "aspect-ratio" (String.fromInt w ++ " / " ++ String.fromInt h)


sizedImage w h attr options =
    el
        [ width fill
        , aspect w h
        , htmlAttribute <| Html.Attributes.style "flex-basis" "auto"
        , inFront <| image attr options
        ]
        none


layoutOptions : Theme -> { options : List Option }
layoutOptions t =
    { options =
        [ focusStyle
            { backgroundColor = Nothing
            , borderColor = Just <| wheat t
            , shadow = Just { color = primary t, offset = ( 0, 0 ), blur = 0, size = 4 }
            }
        ]
    }


allMonoFonts =
    [ PTMono
    , RobotoMono
    , UbuntuSansMono
    , JetBrainsMono
    , IbmPlexMono
    ]


monoFontName font =
    case font of
        PTMono ->
            "PT Mono"

        RobotoMono ->
            "Roboto Mono"

        UbuntuSansMono ->
            "Ubuntu Sans Mono"

        JetBrainsMono ->
            "JetBrains Mono"

        IbmPlexMono ->
            "IBM Plex Mono"


bucketStatistic : Int -> List { a | finished : Posix } -> Dict Int (List { a | finished : Posix })
bucketStatistic bucketSizeMilliseconds pastDictations =
    let
        insert pastDictation dict =
            let
                posixToBucketKey : Posix -> Int
                posixToBucketKey posix =
                    Time.posixToMillis posix // bucketSizeMilliseconds

                key =
                    posixToBucketKey pastDictation.finished
            in
            if Dict.member key dict then
                Dict.update key (Maybe.map (\v -> pastDictation :: v)) dict

            else
                Dict.insert key [ pastDictation ] dict
    in
    pastDictations |> List.foldl insert Dict.empty


bucketStatisticDaily : List { a | finished : Posix } -> Dict Int (List { a | finished : Posix })
bucketStatisticDaily =
    bucketStatistic day


globalDictationCurveInterval =
    day * 14


day =
    1000 * 60 * 60 * 24


ifMobile : Device -> a -> a -> a
ifMobile device a b =
    if device.class == Phone then
        a

    else
        b


inputStyle t =
    [ width fill, Background.color <| wheat t, paddingEach inputPadding ] ++ itemBorder t


inputPadding =
    { top = 12, bottom = 12, left = 12, right = 12 }
