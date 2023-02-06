module Pages.Info exposing (..)

import Common exposing (backButton, buttonAttributes, primary, subTitle, topLeftBar)
import Element exposing (..)
import Element.Border as Border
import Types exposing (..)


view : Int -> Theme -> Element FrontendMsg
view userCount t =
    column [ topLeftBar [ backButton t Back ], spacing 16, width (px 512) ]
        [ subTitle "Über Neo"
        , paragraph [] [ text "Neo ist eine ergonomische Tastaturbelegung, welche für die deutsche Sprache optimiert ist. Wenn du noch mehr über Neo erfahren möchstes besuch bitte die Homepage." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://www.neo-layout.org/", label = text "Neo Homepage" }
        , subTitle "Über Mich"
        , paragraph [] [ text "Hallo ich bin Janek aus Magdeburg. Ich studiere Informatik an der OvGU und programmiere super gerne in Elm. Habe diese Web-App aus Spaß gebaut." ]
        , el [ padding 16, centerX ] <| link (buttonAttributes t) { url = "https://github.com/janekx21", label = text "Mein Github" }
        , row [ spacing 16, centerX ] [ text "Benutzerzahl", el [ padding 4, Common.monospace, Border.width 1, Border.color <| primary t ] <| text <| String.fromInt userCount ]
        ]
