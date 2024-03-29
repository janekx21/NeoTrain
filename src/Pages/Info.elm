module Pages.Info exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Border as Border
import Types exposing (..)


view : AppStatistic -> Theme -> Element FrontendMsg
view { userCount, pastDictationCount } t =
    column [ topLeftBar [ backButton t Back, logoutButton t Logout ], spacing 16, width (px 600) ]
        [ subTitle "Über Neo"
        , paragraph [] [ text "Neo ist eine ergonomische Tastaturbelegung, welche für die deutsche Sprache optimiert ist. Wenn du noch mehr über Neo erfahren möchstes besuch bitte die Homepage." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://www.neo-layout.org/", label = text "Neo Homepage" }
        , subTitle "Über Mich"
        , paragraph [] [ text "Ich bin Janek, studiere Informatik an der OvGU und programmiere, aus Interesse an funktionalen Programmiersprachen, in Elm. Ich habe diese Web-App aus Liebe zu Neo und Elm gebaut und hoffe, dass sie dir gefällt." ]
        , column [ centerX ]
            [ row [ centerX ]
                [ el [ padding 16 ] <| link (buttonAttributes t) { url = "https://github.com/janekx21/Neotrain", label = text "NeoTrain Github Repository" }
                , el [ padding 16 ] <| link (buttonAttributes t) { url = "https://github.com/janekx21", label = text "Mein Github" }
                ]
            , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21/NeoTrain/issues/new", label = text "Problem melden (1min)" }
            ]
        , subTitle "Über Neo Train"
        , paragraph [] [ text "Die Web-Anwendung ist dafür gedacht Neo über Diktate zu lernen. Fang am besten mit der obersten Lektion an und arbeite dich nach unten weiter." ]
        , column [ width fill, spacing 8 ]
            [ row [ spacing 16, centerX ]
                [ text "Benutzerzahl"
                , el (itemBorder t ++ [ padding 4, Common.monospace, Border.color <| primary t ]) <| text <| String.fromInt userCount
                ]
            , row [ spacing 16, centerX ]
                [ text "Diktate Gesammt"
                , el (itemBorder t ++ [ padding 4, Common.monospace, Border.color <| secondary t ]) <| text <| String.fromInt pastDictationCount
                ]
            ]
        ]
