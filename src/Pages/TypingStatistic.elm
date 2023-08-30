module Pages.TypingStatistic exposing (..)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Common exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Lamdera
import Material.Icons as Icons
import Pages.Typing
import Set
import Types exposing (..)


init : PastDictation -> Page
init past =
    TypingStatisticPage { past = past, allPoints = Nothing }


view : Theme -> TypingStatisticModel -> Element FrontendMsg
view t { past, allPoints } =
    let
        { lesson, errors, duration } =
            past

        grouped =
            errors
                |> List.foldr (\error -> Dict.update error.should (\y -> Just <| Maybe.withDefault [] y ++ [ error ])) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> List.length)
                |> List.reverse

        maybeAllPoints =
            Maybe.andThen
                (\p ->
                    if List.length p < 5 then
                        Nothing

                    else
                        Just p
                )
                allPoints
    in
    row
        [ spacing 42
        , topLeftBar [ backButton t Back ]
        , bottomCenterBar [ roundedButton t (ChangePage <| TypingPage <| Pages.Typing.init lesson) (materialIcon Icons.refresh) 'r' ]
        , paddingEach { top = 0, left = 0, right = 0, bottom = 16 } -- extra bottom space for button
        ]
        [ column [ spacing 48 ]
            [ title "Deine Tippstatistik"
            , column [ spacing 8 ]
                [ subTitle "Zeit"
                , text <| "Dauer: " ++ printTime duration
                , text <| "Zeichen pro Minute(CPM): " ++ (String.fromInt <| round <| charsPerMinute lesson duration)
                , text <| "Wörter pro Minute(WPM): " ++ (String.fromInt <| round <| wordsPerMinute lesson duration)
                ]
            , column [ spacing 8 ]
                [ subTitle "Fehlerrate"
                , text <| "Anzahl: " ++ String.fromInt (List.length errors)
                , text <| "Prozent: " ++ errorPercent lesson errors
                ]
            , column [ spacing 8 ]
                [ subTitle "Fehler"
                , if List.isEmpty grouped then
                    smile "Du hast keine Fehler gemacht"

                  else
                    wrappedRow [ spacing 16, width (fill |> maximum 650) ] (grouped |> List.map (viewError t))
                ]
            ]
        , column [ spacing 8, width fill ]
            [ el [ padding 8, centerX ] <|
                el
                    (itemBorder t
                        ++ [ Font.size 32
                           , Font.bold
                           , padding 8
                           , Border.color <| primary t
                           ]
                    )
                <|
                    text <|
                        (String.fromInt <| points past)
                            ++ " Punkte"
            , case maybeAllPoints of
                Just allP ->
                    el [ width (px 300), padding 32, tooltip "weltweite Online Statistik" ] <| pointChart t allP <| points past

                Nothing ->
                    text "keine online Statistik"
            ]
        ]


pointChart : Theme -> List Int -> Int -> Element msg
pointChart t points myPoints =
    let
        factor =
            100

        start =
            points |> List.map (\p -> p // factor)

        data =
            start
                |> Set.fromList
                |> Set.toList
                |> List.map (\p -> { x = toFloat (p * factor), x2 = toFloat ((p + 1) * factor), y = start |> List.filter (\a -> a == p) |> List.length |> toFloat })
    in
    html <|
        C.chart
            [ CA.height 300
            , CA.width 300
            ]
            [ C.xLabels [ CA.withGrid, CA.color (toHex <| black t) ]
            , C.yLabels [ CA.color (toHex <| black t) ]
            , C.bars
                [ CA.spacing 0.1, CA.roundTop <| toFloat t.rounding * 0.1, CA.x1 .x, CA.x2 .x2 ]
                [ C.bar .y [ CA.color (toHex <| secondary t) ] ]
                data
            , C.withPlane <|
                \p ->
                    [ C.line
                        [ CA.x1 (toFloat myPoints)
                        , CA.y1 p.y.min
                        , CA.y2 p.y.max
                        , CA.dashed [ 5, 5 ]
                        , CA.width 5
                        , CA.color (toHex <| primary t)
                        ]
                    ]
            ]


viewError : Theme -> ( Char, List TypeError ) -> Element msg
viewError t ( char, typeErrors ) =
    row (itemBorder t ++ [ spacing 8, padding 4, Border.rounded 999 ])
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


errorPercent { content } errors =
    let
        percent =
            round ((toFloat (List.length errors) / toFloat (String.length content)) * 100)
    in
    String.fromInt percent ++ "%"


smile labelText =
    row [ spacing 8 ] [ materialIcon Icons.sentiment_very_satisfied, text labelText ]
