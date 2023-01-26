module Pages.Auth exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icon
import Material.Icons as Icons
import Types exposing (..)


view : Theme -> Auth -> Element FrontendMsg
view t { username, password, passwordVisibility, failed } =
    let
        inputStyle =
            [ width fill, Background.color <| wheat t, Border.width 1, Border.color <| black t, Border.rounded 0 ]

        eyeButton =
            Input.button [ height fill, padding 10, tooltip "Show password" ]
                { label =
                    materialIcon
                        (if passwordVisibility then
                            Icons.visibility_off

                         else
                            Icons.visibility
                        )
                , onPress = Just <| SetVisibility <| not passwordVisibility
                }

        heading =
            inFront <| row [ centerX, Font.size 72, moveUp 156, Font.underline ] [ text "Neo Train ", el [ moveUp 14, moveLeft 1 ] <| html <| Icon.icon <| toHex <| black t ]
    in
    column [ spacing 32, heading, topRightBar [ infoButton t ] ]
        [ title "Login / Register"
        , case failed of
            NotAsked ->
                none

            WrongUsernameOrPassword ->
                el [ Font.color <| primary t ] <| text "Wrong username or password!"

            UsernameOrPasswordInvalid ->
                paragraph [ Font.color <| primary t, width (px 500) ] [ text "Username or password invalid. Username should be alphanumeric. Password should be length 10, alphanumeric with some special chars (IBM valid character)." ]
        , column [ spacing 8, width fill ]
            [ subTitle "Username"
            , Input.username inputStyle
                { text = username
                , label = Input.labelHidden "username"
                , placeholder = Nothing
                , onChange = SetUsername
                }
            ]
        , column [ spacing 8, width fill ]
            [ subTitle "Password"
            , Input.currentPassword (inputStyle ++ [ inFront <| el [ alignRight ] <| eyeButton ])
                { text = password
                , label = Input.labelHidden "password"
                , placeholder = Nothing
                , onChange = SetPassword
                , show = passwordVisibility
                }
            ]
        , row [ spacing 16, width fill ]
            [ Input.button
                ([ width fill ] ++ buttonAttributes t)
                { label = el [ centerX ] <| text "Login", onPress = Just <| TryLogin username password }
            , Input.button
                ([ width fill ] ++ buttonAttributes t)
                { label = el [ centerX ] <| text "Register", onPress = Just <| TryRegister username password }
            ]
        ]
