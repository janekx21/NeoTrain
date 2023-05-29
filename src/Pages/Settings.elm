module Pages.Settings exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Material.Icons as Icons
import Types exposing (..)


update : SettingsMsg -> { layer : Int } -> ( { layer : Int }, Cmd msg )
update msg model =
    case msg of
        SetLayer layer ->
            ( { layer = layer }, Cmd.none )


view : Theme -> Settings -> { layer : Int } -> Element FrontendMsg
view t settings { layer } =
    let
        settingsBlock label child =
            column [ width fill, spacing 8 ]
                [ subTitle label
                , child
                ]

        blockSettingItem label setting =
            viewSettingsItem t label (SetSettings { settings | blockOnError = setting }) (settings.blockOnError == setting)

        layoutSettingItem layout =
            viewSettingsItem t (text <| layoutNames layout) (SetSettings { settings | layout = layout }) (settings.layout == layout)

        themeSettingItem ( themeString, themeName ) =
            let
                theme =
                    { t | name = themeName }

                box color =
                    el
                        (itemBorder t
                            ++ [ width (px 16)
                               , height (px 16)
                               , Background.color color
                               , Border.color (invertLightness color)
                               ]
                        )
                    <|
                        none

                th =
                    { t | name = themeName }

                label =
                    row [ spacing 8 ]
                        [ box <| black th
                        , box <| wheat th
                        , box <| primary th
                        , box <| secondary th
                        , text themeString
                        ]
            in
            viewSettingsItem t label (SetSettings { settings | theme = theme }) (settings.theme.name == themeName)

        themeDarkSettingsButton : Bool -> Bool -> Element FrontendMsg
        themeDarkSettingsButton dark active =
            Input.button
                (buttonAttributes t
                    ++ (if active then
                            activeAttributes t

                        else
                            []
                       )
                )
                { label =
                    materialIcon
                        (if dark then
                            Icons.dark_mode

                         else
                            Icons.light_mode
                        )
                , onPress = Just <| SetSettings { settings | theme = { t | dark = dark } }
                }

        layerButton : Int -> Bool -> Element FrontendMsg
        layerButton layer2 active =
            viewSettingsItem t
                (text <| String.fromInt layer2)
                (PageMsg <| SettingsMsg <| SetLayer layer2)
                (layer2 == layer)

        logoutButton =
            roundedButton t Logout (materialIcon Icons.logout) 'l'
    in
    column [ topLeftBar [ backButton t Back, logoutButton ], spacing 48 ]
        [ title "Einstellungen"
        , row [ spacing 48 ]
            [ column
                [ spacing 32, alignTop ]
                [ settingsBlock "Layout Vorschau" <|
                    column
                        ([ width fill
                         ]
                            ++ itemBorder t
                        )
                    <|
                        List.map layoutSettingItem layouts
                , settingsBlock "Blockierung"
                    (column
                        ([ width fill
                         ]
                            ++ itemBorder t
                        )
                        [ blockSettingItem (text "Warte auf ein Backspace") OneBackspace
                        , blockSettingItem (text "Warte auf richtiges Zeichen") CorrectLetter
                        ]
                    )
                ]
            , column [ spacing 32, alignTop ]
                [ settingsBlock "Zeichen links und rechts vom Cursor" <|
                    column [ width fill, spacing 8 ]
                        [ slider t 0 50 settings.paddingLeft (\value -> SetSettings { settings | paddingLeft = value })
                        , slider t 0 50 settings.paddingRight (\value -> SetSettings { settings | paddingRight = value })
                        ]
                , settingsBlock "Theme" <|
                    column [ width fill, spacing 8 ]
                        [ column ([ width fill ] ++ itemBorder t) <|
                            List.map themeSettingItem themes
                        , row [ spacing 8 ]
                            [ themeDarkSettingsButton True t.dark
                            , themeDarkSettingsButton False (not t.dark)
                            ]
                        ]
                , settingsBlock "Rundung der Ecken" <|
                    slider t
                        0
                        20
                        settings.theme.rounding
                        (\value ->
                            let
                                theme =
                                    { t | rounding = value }
                            in
                            SetSettings { settings | theme = theme }
                        )
                ]
            ]
        , row [ width fill, spacing 16 ]
            [ column (itemBorder t) (List.range 1 6 |> List.map (\n -> layerButton n False))
            , sizedImage 535 183 [ width fill ] { src = layerUrl settings.layout layer, description = "" }
            ]
        ]


viewSettingsItem t label msg active =
    Input.button
        ([ width fill ]
            ++ itemAttributes t
            ++ (if active then
                    activeAttributes t

                else
                    []
               )
        )
        { label = label, onPress = Just msg }


activeAttributes t =
    [ Background.color <| secondary t, Font.color <| wheat t ]


slider : Theme -> Float -> Float -> Int -> (Int -> msg) -> Element msg
slider t min max value msg =
    let
        padding =
            logBase 10 max |> ceiling
    in
    row [ width fill, spacing 8 ]
        [ el [ monospace ] <| text <| String.padLeft padding ' ' (String.fromInt value)
        , el ([ width fill ] ++ itemBorder t) <|
            Input.slider [ width fill ]
                { onChange = round >> msg
                , label = Input.labelHidden ""
                , min = min
                , max = max
                , value = toFloat value
                , thumb = Input.thumb [ width (px 20), height (px 20), Background.color <| black t, Border.rounded t.rounding ]
                , step = Just 1
                }
        ]
