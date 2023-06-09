module Pages.Info exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Border as Border
import Types exposing (..)


view : Int -> Theme -> Element FrontendMsg
view userCount t =
    column [ topLeftBar [ backButton t Back, logoutButton t Logout ], spacing 16, width (px 512) ]
        [ subTitle "Über Neo"
        , paragraph [] [ text "Neo ist eine ergonomische Tastaturbelegung, welche für die deutsche Sprache optimiert ist. Wenn du noch mehr über Neo erfahren möchstes besuch bitte die Homepage." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://www.neo-layout.org/", label = text "Neo Homepage" }
        , subTitle "Über Mich"
        , paragraph [] [ text "Hallo ich bin Janek aus Magdeburg. Ich studiere Informatik an der OvGU und programmiere super gerne in Elm. Habe diese Web-App aus Spaß gebaut." ]
        , row [ centerX ]
            [ el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21/Neotrain", label = text "Github Repository" }
            , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21", label = text "Mein Github" }
            ]
        , subTitle "Über Neo Train"
        , paragraph [] [ text "Die Web-Anwendung ist dafür gedacht Neo über Diktate zu lernen. Fang am besten mit der obersten Lektion an und arbeite dich nach unten weiter." ]
        , row [ spacing 16, centerX ]
            [ text "Benutzerzahl"
            , el (itemBorder t ++ [ padding 4, Common.monospace, Border.color <| primary t ]) <| text <| String.fromInt userCount
            ]
        ]
