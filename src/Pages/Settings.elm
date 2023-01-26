module Pages.Settings exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Material.Icons as Icons
import Types exposing (..)


view : Theme -> Settings -> Element FrontendMsg
view t settings =
    let
        settingsBlock label child =
            column [ width fill, spacing 8 ]
                [ subTitle label
                , child
                ]

        blockSettingItem label setting =
            viewSettingsItem t label (SetSettings { settings | blockOnError = setting }) (settings.blockOnError == setting)

        layoutSettingItem layout =
            viewSettingsItem t (layoutNames layout) (SetSettings { settings | layout = layout }) (settings.layout == layout)

        themeSettingItem ( label, setting ) =
            viewSettingsItem t label (SetSettings { settings | theme = setting }) (settings.theme == setting)

        logoutButton =
            roundedButton t Logout (materialIcon Icons.logout) 'l'
    in
    column [ topLeftBar [ backButton t Back, logoutButton ], spacing 48 ]
        [ title "Settings"
        , row [ spacing 48 ]
            [ column
                [ spacing 32, alignTop ]
                [ settingsBlock "Layout Preview" <|
                    column [ width fill, Border.color <| black t, Border.width 1 ] <|
                        List.map layoutSettingItem layouts
                , settingsBlock "Blocking"
                    (column [ width fill, Border.color <| black t, Border.width 1 ]
                        [ blockSettingItem "Waiting for one backspace" OneBackspace
                        , blockSettingItem "Waiting for the correct letter" CorrectLetter
                        ]
                    )
                ]
            , column [ spacing 32, alignTop ]
                [ settingsBlock "Chars left and right of cursor" <|
                    column [ width fill, spacing 8 ]
                        [ slider t 0 50 settings.paddingLeft (\value -> SetSettings { settings | paddingLeft = value })
                        , slider t 0 50 settings.paddingRight (\value -> SetSettings { settings | paddingRight = value })
                        ]
                , settingsBlock "Theme" <|
                    column [ width fill, Border.color <| black t, Border.width 1 ] <|
                        List.map themeSettingItem themes
                ]
            ]
        ]


viewSettingsItem t labelText msg active =
    Input.button
        ([ width fill ]
            ++ itemAttributes t
            ++ (if active then
                    [ Background.color <| secondary t, Font.color <| wheat t ]

                else
                    []
               )
        )
        { label = text labelText, onPress = Just msg }


slider : Theme -> Float -> Float -> Int -> (Int -> msg) -> Element msg
slider t min max value msg =
    let
        padding =
            logBase 10 max |> ceiling
    in
    row [ width fill, spacing 8 ]
        [ el [ monospace ] <| text <| String.padLeft padding ' ' (String.fromInt value)
        , el [ width fill, Border.color <| black t, Border.width 1 ] <|
            Input.slider [ width fill ]
                { onChange = round >> msg
                , label = Input.labelHidden ""
                , min = min
                , max = max
                , value = toFloat value
                , thumb = Input.thumb [ width (px 20), height (px 20), Background.color <| black t ]
                , step = Just 1
                }
        ]
