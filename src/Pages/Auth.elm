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
import Translation exposing (..)
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

        Types.WithoutLogin ->
            ( ToMenuPage, Cmd.none )

        ToggleTranslation ->
            -- this should be done by something more upstream
            ( JustModel authModel, Cmd.none )


view : Language -> Theme -> AuthModel -> Element AuthMsg
view l t { username, password, passwordVisibility, failed } =
    let
        inputPadding =
            { top = 12, bottom = 12, left = 12, right = 12 }

        inputStyle =
            [ width fill, Background.color <| wheat t, paddingEach inputPadding ] ++ itemBorder t

        eyeButton =
            Input.button [ height fill, padding 10, tooltip <| translate ShowPassword l ]
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
    column [ spacing 32, heading, topRightBar [ translateButton t ToggleTranslation, infoButton t ToInfo ] ]
        [ title <| (translate Login l ++ " / " ++ translate Register l)
        , case failed of
            NotAsked ->
                none

            Types.WrongUsernameOrPassword ->
                el [ Font.color <| primary t ] <| text <| translate Translation.WrongUsernameOrPassword l

            Types.UsernameOrPasswordInvalid ->
                paragraph [ Font.color <| primary t, width (px 300) ] [ text <| translate Translation.UsernameOrPasswordInvalid l ]
        , el [ width fill ] <|
            html <|
                Html.form [] <|
                    List.singleton <|
                        layoutWith (layoutOptions t) [ width fill, height fill, Background.color <| wheat t, Font.color <| black t ] <|
                            column [ spacing 16, width fill ]
                                [ column [ spacing 8, width fill ]
                                    [ subTitle <| translate Username l
                                    , Input.username inputStyle
                                        { text = username
                                        , label = Input.labelHidden "username"
                                        , placeholder = Nothing
                                        , onChange = SetUsername
                                        }
                                    ]
                                , column [ spacing 8, width fill ]
                                    [ subTitle <| translate Password l
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
                    { label = el [ centerX ] <| text <| translate Login l, onPress = Just <| TryLogin username password }
                , Input.button
                    ([ width fill ] ++ buttonAttributes t)
                    { label = el [ centerX ] <| text <| translate Register l, onPress = Just <| TryRegister username password }
                ]
            , Input.button
                ([ width fill ] ++ buttonAttributes t ++ primaryAttributes t)
                { label = row [ centerX, spacing 4 ] [ text <| translate Translation.WithoutLogin l, materialIcon Icons.arrow_forward ], onPress = Just Types.WithoutLogin }
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
