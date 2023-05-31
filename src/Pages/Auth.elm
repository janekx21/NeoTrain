module Pages.Auth exposing (..)

import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Generated.Icon
import Html as Html
import Html.Events
import Json.Decode as Decode
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

        WithoutLogin ->
            ( ToMenuPage, Cmd.none )


view : Theme -> AuthModel -> Element AuthMsg
view t { username, password, passwordVisibility, failed } =
    let
        inputPadding =
            { top = 12, bottom = 12, left = 12, right = 12 }

        inputStyle =
            [ width fill, Background.color <| wheat t, paddingEach inputPadding ] ++ itemBorder t

        eyeButton =
            Input.button [ height fill, padding 10, tooltip "Zeige password an" ]
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
            inFront <| el [ centerX, moveUp 140, scale 1.4 ] <| html <| Generated.Icon.icon <| toHex <| black t
    in
    column [ spacing 32, heading, topRightBar [ infoButton t ToInfo ] ]
        [ title "Login / Register"
        , case failed of
            NotAsked ->
                none

            WrongUsernameOrPassword ->
                el [ Font.color <| primary t ] <| text "Falscher Username oder falsches Password!"

            UsernameOrPasswordInvalid ->
                paragraph [ Font.color <| primary t, width (px 300) ] [ text "Username muss [A-z] Länge min 3. Password muss IBM valide und Länge min. 10" ]
        , el [ width fill ] <|
            html <|
                Html.form [] <|
                    List.singleton <|
                        layoutWith (layoutOptions t) [ width fill, height fill, Background.color <| wheat t, Font.color <| black t ] <|
                            column [ spacing 16, width fill ]
                                [ column [ spacing 8, width fill ]
                                    [ subTitle "Benutzername"
                                    , Input.username inputStyle
                                        { text = username
                                        , label = Input.labelHidden "username"
                                        , placeholder = Nothing
                                        , onChange = SetUsername
                                        }
                                    ]
                                , column [ spacing 8, width fill ]
                                    [ subTitle "Password"
                                    , Input.currentPassword
                                        (inputStyle
                                            ++ [ inFront <| el [ alignRight ] <| eyeButton
                                               , paddingEach { inputPadding | right = 44 }
                                               , onEnter <| TryLogin username password
                                               ]
                                        )
                                        { text = password
                                        , label = Input.labelHidden "password"
                                        , placeholder = Nothing
                                        , onChange = SetPassword
                                        , show = passwordVisibility
                                        }
                                    ]
                                ]
        , column [ spacing 16, width fill ]
            [ row [ spacing 16, width fill ]
                [ Input.button
                    ([ width fill ] ++ buttonAttributes t)
                    { label = el [ centerX ] <| text "Login", onPress = Just <| TryLogin username password }
                , Input.button
                    ([ width fill ] ++ buttonAttributes t)
                    { label = el [ centerX ] <| text "Register", onPress = Just <| TryRegister username password }
                ]
            , Input.button
                ([ width fill ] ++ buttonAttributes t ++ primaryAttributes t)
                { label = row [ centerX, spacing 4 ] [ text "Ohne Login", materialIcon Icons.arrow_forward ], onPress = Just WithoutLogin }
            ]
        ]


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


primaryAttributes t =
    [ Background.color <| secondary t, Font.color <| wheat t ]
