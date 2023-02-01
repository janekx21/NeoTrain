module Pages.Auth exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icon
import Lamdera
import Material.Icons as Icons
import Types exposing (..)


update : AuthMsg -> AuthModel -> ( AuthResult, Cmd FrontendMsg )
update authMsg authModel =
    case authMsg of
        SetUsername string ->
            ( JustModel { authModel | username = string }, Cmd.none )

        SetPassword string ->
            ( JustModel { authModel | password = string }, Cmd.none )

        SetVisibility bool ->
            ( JustModel { authModel | passwordVisibility = bool }, Cmd.none )

        TryLogin username password ->
            ( JustModel authModel, Lamdera.sendToBackend <| InsertSession username password )

        TryRegister username password ->
            ( JustModel authModel, Lamdera.sendToBackend <| InsertUser username password )

        ToInfo ->
            -- change page
            ( ToInfoPage, Cmd.none )


view : Theme -> AuthModel -> Element AuthMsg
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
    column [ spacing 32, heading, topRightBar [ infoButton t ToInfo ] ]
        [ title "Login / Register"
        , case failed of
            NotAsked ->
                none

            WrongUsernameOrPassword ->
                el [ Font.color <| primary t ] <| text "Wrong username or password!"

            UsernameOrPasswordInvalid ->
                paragraph [ Font.color <| primary t, width (px 300) ] [ text "Username should be [A-z]. Password should be IBM valid password length 10." ]
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
