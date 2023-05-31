module Pages.Menu exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Html.Attributes as HA
import Material.Icons as Icons
import Pages.Typing
import Types exposing (..)


view : Theme -> FrontendModel -> Menu -> Element FrontendMsg
view t model menu =
    let
        block label child =
            column [ spacing 8 ] [ subTitle label, child ]

        sidebar =
            case menu.current of
                Just lesson ->
                    column [ height fill, spacing 32 ]
                        [ block "Zeichenanzahl" <| text <| String.fromInt <| String.length lesson.content
                        , block "Wortanzahl" <| text <| String.fromInt <| List.length <| String.split " " <| lesson.content
                        , block "Vorschau" <| paragraph [ width (px 500), height fill, htmlAttribute <| HA.style "text-align" "justify" ] [ text (lesson.content |> Common.truncate 350) ]
                        , el [ alignRight, alignBottom ] <| squareButton t (ChangePage <| TypingPage <| Pages.Typing.init lesson) (text "Start") 'r'
                        ]

                Nothing ->
                    none

        lessonDoneCount lesson =
            model.statistic
                |> List.filter (\p -> p.lesson == lesson)
                |> List.length

        settingsButton =
            roundedButton t (ChangePage <| SettingsPage { layer = 1 }) (materialIcon Icons.settings) 's'

        lessonFilter : Lesson -> Bool
        lessonFilter l =
            l.layout |> Maybe.map (\y -> y == model.settings.layout) |> Maybe.withDefault True
    in
    column [ spacing 32, topRightBar [ infoButton t (ChangePage InfoPage), statisticButton t, settingsButton ] ]
        [ title "Diktate"
        , row
            [ spacing 40 ]
            [ column
                (itemBorder t ++ [ height (fill |> maximum 512), scrollbarY, width fill ])
                (lessons |> List.filter lessonFilter |> List.map (\l -> viewMenuItem t (lessonDoneCount l) l))
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
                , el [ tooltip <| "geübt " ++ String.fromInt doneCount ++ "x" ] <| check
                ]
        , onPress = Just <| ChangePage <| MenuPage { current = Just book }
        }
