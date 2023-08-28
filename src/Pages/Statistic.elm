module Pages.Statistic exposing (..)

import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Common exposing (..)
import Dict exposing (Dict)
import Element exposing (..)
import Element.Border as Border
import Element.Events
import Element.Input as Input
import Html.Attributes
import Material.Icons as Icons
import Pages.TypingStatistic as TypingStatistic
import Set exposing (Set)
import Time exposing (Posix)
import Types exposing (..)


view : Theme -> Hover -> List PastDictation -> Element FrontendMsg
view t hovering statistic =
    let
        viewPast : PastDictation -> Element FrontendMsg
        viewPast past =
            Input.button
                ([ width fill
                 , Element.Events.onMouseEnter <| OnHover [ past ]
                 ]
                    ++ itemAttributes t
                    ++ (if List.member past hovering then
                            mouseOverAttributes t

                        else
                            []
                       )
                )
                { label = text <| (String.fromInt <| points past) ++ "\t" ++ past.lesson.title
                , onPress = Just (ChangePage <| TypingStatistic.init past)
                }

        viewHover =
            case median hovering of
                Just h ->
                    column [ spacing 8 ]
                        [ text h.lesson.title
                        , text <| "Punkte: " ++ (String.fromInt <| points h)
                        ]

                Nothing ->
                    -- placeholder because of hover jitter
                    column [ spacing 8 ] [ text " ", text " " ]
    in
    column [ topLeftBar [ backButton t Back ], spacing 48, width fill ]
        [ title "Statistik"
        , row [ width fill, spacing 48 ]
            [ column [ spacing 8, width fill, alignTop ]
                [ subTitle "Letzte Diktate"
                , if List.isEmpty statistic then
                    info "Du hast noch keine Diktate geschrieben"

                  else
                    el [ width fill ] <|
                        column
                            (itemBorder t
                                ++ [ width fill
                                   , height (fill |> maximum 512)
                                   , scrollbarY
                                   , Element.Events.onMouseLeave <| OnHover <| []
                                   ]
                            )
                        <|
                            List.map viewPast statistic
                ]
            , column [ spacing 8, alignTop ] [ subTitle "Punktefortschritt", viewPointsGraph t hovering statistic, viewHover ]
            ]
        ]


viewPointsGraph : Theme -> Hover -> List PastDictation -> Element FrontendMsg
viewPointsGraph t hovering dictations =
    let
        items =
            dictations
                |> bucketStatistic
                |> Dict.toList
    in
    el [ width <| px 400, height fill ] <|
        html <|
            C.chart
                ([ CA.height 300
                 , CA.width 300
                 , CA.margin { top = 8, bottom = 32, left = 48, right = 8 }
                 , CA.padding { top = 4, bottom = 4, left = 4, right = 4 }
                 , CE.onMouseMove (List.concatMap (CI.getData >> Tuple.second) >> OnHover) (CE.getNearest CI.dots)
                 , CE.onMouseLeave (OnHover [])
                 , CE.onMouseUp
                    (List.concatMap (CI.getData >> Tuple.second)
                        >> median
                        >> Maybe.map (\p -> { past = p, allPoints = Nothing })
                        >> Maybe.map (ChangePage << TypingStatisticPage)
                        >> Maybe.withDefault NoOpFrontendMsg
                    )
                    (CE.getNearest CI.dots)
                 ]
                    ++ (if List.isEmpty hovering then
                            []

                        else
                            [ CA.htmlAttrs [ Html.Attributes.style "cursor" "pointer" ] ]
                       )
                )
                [ C.yTicks [ CA.color (toHex <| black t) ]
                , C.yLabels [ CA.color (toHex <| black t), CA.fontSize 16 ]
                , C.grid [ CA.color (toHex <| secondary t) ]
                , C.series (Tuple.first >> toFloat)
                    [ C.interpolated (Tuple.second >> medianPoints >> toFloat) [ CA.color <| toHex <| primary t, CA.width 4 ] [ CA.circle, CA.size 8 ]
                        |> C.variation
                            (\_ data ->
                                let
                                    createSet a =
                                        a |> List.map .finished |> List.map Time.posixToMillis |> Set.fromList
                                in
                                if haveCommon (hovering |> createSet) (data |> Tuple.second |> createSet) then
                                    [ CA.circle, CA.size 48, CA.color (toHex <| primary t) ]

                                else
                                    []
                            )
                    ]
                    items
                ]


medianPoints : List PastDictation -> Int
medianPoints pastDictations =
    pastDictations
        |> median
        |> Maybe.map points
        |> Maybe.withDefault 0


bucketStatistic : List PastDictation -> Dict Int Bucket
bucketStatistic pastDictations =
    let
        insert : PastDictation -> Dict Int Bucket -> Dict Int Bucket
        insert pastDictation dict =
            let
                posixToBucketKey : Posix -> Int
                posixToBucketKey posix =
                    Time.posixToMillis posix // (1000 * 60 * 10)

                key =
                    posixToBucketKey pastDictation.finished
            in
            if Dict.member key dict then
                Dict.update key (Maybe.map (\v -> pastDictation :: v)) dict

            else
                Dict.insert key [ pastDictation ] dict
    in
    pastDictations |> List.foldl insert Dict.empty


median : List PastDictation -> Maybe PastDictation
median pastDictations =
    let
        sorted =
            List.sortBy points pastDictations
    in
    sorted
        |> List.drop (List.length sorted // 2)
        |> List.head


info labelText =
    row [ spacing 8 ] [ materialIcon Icons.info, text labelText ]


haveCommon : Set comparable -> Set comparable -> Bool
haveCommon a b =
    not (Set.isEmpty (Set.intersect a b))
