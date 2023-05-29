module Pages.TypingStatistic exposing (..)

import Common exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Material.Icons as Icons
import Pages.Typing
import Types exposing (..)


view : Theme -> PastDictation -> Element FrontendMsg
view t past =
    let
        { lesson, errors, duration } =
            past

        grouped =
            errors
                |> List.foldr (\error -> Dict.update error.should (\y -> Just <| Maybe.withDefault [] y ++ [ error ])) Dict.empty
                |> Dict.toList
                |> List.sortBy (Tuple.second >> List.length)
                |> List.reverse
    in
    column
        [ spacing 48
        , topLeftBar [ backButton t Back ]
        , bottomCenterBar [ roundedButton t (ChangePage <| TypingPage <| Pages.Typing.init lesson) (materialIcon Icons.refresh) 'r' ]
        ]
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
        , column [ spacing 8, width fill ]
            [ subTitle "Punkte"
            , el [ padding 8, centerX ] <|
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
                        String.fromInt <|
                            points past
            ]
        ]


viewError : Theme -> ( Char, List TypeError ) -> Element msg
viewError t ( char, typeErrors ) =
    row [ spacing 8, Border.width 1, Border.color <| black t, padding 4, Border.rounded 999 ]
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
