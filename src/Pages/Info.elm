module Pages.Info exposing (..)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Common exposing (..)
import Element exposing (..)
import Element.Border as Border
import Time exposing (Posix)
import Types exposing (..)


view : Device -> AppStatistic -> Theme -> Element FrontendMsg
view device { userCount, pastDictationCount, pastDictationCurve } t =
    let
        now =
            1717505923

        maxWidth =
            width (fill |> maximum 800)
    in
    column [ topBarPadding, spacing 16, topLeftBar device [ backButton t Back, logoutButton t Logout ] ]
        [ subTitle "Über Neo"
        , paragraph [ maxWidth ] [ text "Neo ist eine ergonomische Tastaturbelegung, welche für die deutsche Sprache optimiert ist. Wenn du noch mehr über Neo erfahren möchstes besuch bitte die Homepage." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://www.neo-layout.org/", label = text "Neo Homepage" }
        , subTitle "Über Mich"
        , paragraph [ maxWidth ] [ text "Ich bin Janek, studiere Informatik an der OvGU und programmiere, aus Interesse an funktionalen Programmiersprachen, in Elm. Ich habe diese Web-App aus Liebe zu Neo und Elm gebaut und hoffe, dass sie dir gefällt." ]
        , column [ centerX ]
            [ mobileRow device
                [ centerX ]
                [ el [ padding 16 ] <| link (buttonAttributes t) { url = "https://github.com/janekx21/Neotrain", label = text "NeoTrain Github Repository" }
                , el [ padding 16 ] <| link (buttonAttributes t) { url = "https://github.com/janekx21", label = text "Mein Github" }
                ]
            , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21/NeoTrain/issues/new", label = text "Problem/Idee melden (1min)" }
            ]
        , subTitle "Über Neo Train"
        , paragraph [ maxWidth ] [ text "Die Web-Anwendung ist dafür gedacht Neo über Diktate zu lernen. Fang am besten mit der obersten Lektion an und arbeite dich nach unten weiter." ]
        , column [ width fill, spacing 8 ]
            [ row [ spacing 16, centerX ]
                [ text "Benutzerzahl"
                , el (itemBorder t ++ [ padding 4, Common.monospace t.monoFont, Border.color <| primary t ]) <| text <| String.fromInt userCount
                ]
            , row [ spacing 16, centerX ]
                [ text "Diktate Gesammt"
                , el (itemBorder t ++ [ padding 4, Common.monospace t.monoFont, Border.color <| secondary t ]) <| text <| String.fromInt pastDictationCount
                ]
            ]
        , dictationChart device t pastDictationCurve
        ]


dictationChart : Device -> Theme -> List ( Posix, Int ) -> Element msg
dictationChart device t dictationCurve =
    let
        data =
            dictationCurve
                |> List.map (\( x, y ) -> { x = toFloat (Time.posixToMillis x), x2 = toFloat (Time.posixToMillis x + globalDictationCurveInterval), y = toFloat y })

        widthPx =
            case device.class of
                Phone ->
                    200

                _ ->
                    400
    in
    el [ width (px widthPx), height (px 350), centerX ] <|
        html <|
            C.chart
                [ CA.height 300
                , CA.width widthPx
                ]
                [ C.xLabels [ CA.withGrid, CA.color (toHex <| black t), CA.times Time.utc ]
                , C.yLabels [ CA.color (toHex <| black t) ]
                , C.bars
                    [ CA.spacing 0.1, CA.roundTop <| toFloat t.rounding * 0.1, CA.x1 .x, CA.x2 .x2 ]
                    [ C.bar .y [ CA.color (toHex <| secondary t) ] ]
                    data
                ]
