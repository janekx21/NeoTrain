module Pages.Typing exposing (..)

import Browser.Dom as Dom
import Browser.Events
import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Html.Events
import Lamdera.Json as Decode exposing (Decoder)
import Material.Icons as Icons
import Svg.Attributes exposing (overflow)
import Task
import Time
import Types exposing (..)



-- Init


hiddenInputId : String
hiddenInputId =
    "hidden-input"


init : Lesson -> TypingModel
init lesson =
    { dictation = bookToDictation lesson
    , madeError = False
    , errors = []
    , mods = Mods False False False
    , lesson = lesson
    , duration = 0
    , paused = True
    , showKeyboard = True
    , textOffset = 0
    , textSpeed = 0
    }


bookToDictation : Lesson -> Dictation
bookToDictation book =
    let
        ( current, next ) =
            book.content
                |> String.uncons
                |> Maybe.withDefault ( '?', "" )
    in
    { prev = "", current = current, next = next }



-- Update


update : TypingMsg -> Settings -> TypingModel -> ( Maybe TypingModel, Cmd FrontendMsg )
update typingMsg settings model =
    let
        focusCommand =
            Dom.focus hiddenInputId |> Task.attempt (\_ -> NoOpFrontendMsg)
    in
    case typingMsg of
        NoOp ->
            ( Just model, Cmd.none )

        KeyDown keyboardKey ->
            case updateDictation keyboardKey settings model of
                Just next ->
                    ( Just next, focusCommand )

                Nothing ->
                    ( Just model, Time.now |> Task.perform (FinishedDictation model.errors model.lesson model.duration) )

        KeyDownBatch keys ->
            let
                keyboardKeys =
                    keys |> String.toList |> List.map Character

                _ =
                    Debug.log "keyboards keys" keyboardKeys
            in
            case updateDictationBatch keyboardKeys settings model of
                Just next ->
                    ( Just next, focusCommand )

                Nothing ->
                    ( Just model, Time.now |> Task.perform (FinishedDictation model.errors model.lesson model.duration) )

        KeyUp keyboardKey ->
            let
                mods =
                    model.mods

                newMods =
                    case keyboardKey of
                        Control code ->
                            case controllToMod code of
                                Just Shift ->
                                    { mods | shift = False }

                                Just Mod3 ->
                                    { mods | mod3 = False }

                                Just Mod4 ->
                                    { mods | mod4 = False }

                                Nothing ->
                                    mods

                        _ ->
                            mods
            in
            ( Just { model | mods = newMods }, Cmd.none )

        TickTypingTime ->
            ( Just { model | duration = model.duration + ticksPerSecond }, focusCommand )

        Pause ->
            ( Just { model | paused = True }, Cmd.none )

        Play ->
            ( Just { model | paused = False }, Cmd.none )

        Restart ->
            ( Just <| init model.lesson, Cmd.none )

        ToggleKeyboard ->
            ( Just { model | showKeyboard = not model.showKeyboard }, Cmd.none )

        Exit ->
            --change page
            ( Nothing, Cmd.none )

        AnimationFrameDelta delta ->
            let
                speed =
                    model.textSpeed

                distance =
                    model.textOffset

                nextSpeed =
                    speed + (distance * 0.02 - speed)
            in
            ( Just { model | textOffset = max (model.textOffset - (delta / 10) * speed) 0, textSpeed = nextSpeed }, Cmd.none )


updateDictationBatch : List KeyboardKey -> Settings -> TypingModel -> Maybe TypingModel
updateDictationBatch keys settings typing =
    -- This is propably not needed because "onInput" results in one character strings
    case keys of
        head :: tail ->
            let
                next =
                    updateDictation head settings typing
            in
            case next of
                Just nextTyping ->
                    updateDictationBatch tail settings nextTyping

                Nothing ->
                    next

        [] ->
            Just typing


updateDictation : KeyboardKey -> Settings -> TypingModel -> Maybe TypingModel
updateDictation keyboardKey settings typing =
    let
        decodedKey =
            case keyboardKey of
                Control key ->
                    case key of
                        "Enter" ->
                            Character '\n'

                        "Tab" ->
                            Character '\t'

                        _ ->
                            Control key

                Character char ->
                    Character char
    in
    case decodedKey of
        Control code ->
            let
                mods =
                    typing.mods
            in
            case controllToMod code of
                Just Shift ->
                    Just { typing | mods = { mods | shift = True } }

                Just Mod3 ->
                    Just { typing | mods = { mods | mod3 = True } }

                Just Mod4 ->
                    Just { typing | mods = { mods | mod4 = True } }

                Nothing ->
                    case code of
                        "Backspace" ->
                            Just { typing | madeError = False }

                        _ ->
                            Just typing

        Character char ->
            if typing.paused then
                Just { typing | paused = False }

            else if settings.blockOnError == OneBackspace && typing.madeError then
                Just typing

            else if char /= typing.dictation.current && typing.madeError then
                Just typing

            else if char /= typing.dictation.current then
                Just { typing | madeError = True, errors = typing.errors ++ [ { was = char, should = typing.dictation.current } ] }

            else
                typing.dictation
                    |> advanceDictation
                    |> Maybe.map
                        (\dictation ->
                            { typing
                                | dictation = dictation
                                , madeError = False
                                , textOffset = typing.textOffset + 19.2
                            }
                        )


controllToMod code =
    case code of
        "ShiftLeft" ->
            Just Shift

        "ShiftRight" ->
            Just Shift

        "CapsLock" ->
            Just Mod3

        "Backslash" ->
            Just Mod3

        "IntlBackslash" ->
            Just Mod4

        "AltRight" ->
            Just Mod4

        _ ->
            Nothing


advanceDictation : Dictation -> Maybe Dictation
advanceDictation dict =
    dict.next
        |> String.uncons
        |> Maybe.map
            (\( nextCurrent, next ) ->
                { prev = dict.prev ++ String.fromChar dict.current
                , current = nextCurrent
                , next = next
                }
            )



-- View


view : Device -> Theme -> TypingModel -> Settings -> Element TypingMsg
view device t { dictation, mods, madeError, paused, showKeyboard, duration, textOffset } settings =
    let
        fontSize =
            ifMobile device 24 32

        paddingLeft =
            ifMobile device 4 settings.paddingLeft

        paddingRight =
            ifMobile device 22 settings.paddingLeft

        color =
            if madeError then
                primary

            else
                secondary

        prev =
            dictation.prev
                |> String.right paddingLeft
                |> String.padLeft paddingLeft '\u{0000}'
                |> String.toList

        next =
            dictation.next
                |> String.left paddingRight
                |> String.padRight paddingRight '\u{0000}'
                |> String.toList

        indexToAlpha i off =
            min ((toFloat i + off / 19.2) / 8) 1

        typewriter =
            row [ monospace t.monoFont, Font.size fontSize, moveRight (textOffset * toFloat fontSize / 32.0) ]
                -- the i - 1 in there is a workaround to having a smoth tail at the end of the line
                [ row [] (List.indexedMap (\i c -> viewChar c (indexToAlpha (i - 1) textOffset)) prev)
                , el [ Background.color <| color t, Font.color <| wheat t ] <| viewChar dictation.current 1
                , row [ Font.color <| secondary t ] (List.indexedMap (\i c -> viewChar c (indexToAlpha (List.length next - i) -textOffset)) next)
                ]

        pausedEl =
            el [ centerX, monospace t.monoFont, Font.size fontSize ] <|
                text <|
                    String.pad (paddingLeft + paddingRight + 1) ' ' <|
                        "Pausiert. Drück eine Taste"

        pauseButton =
            roundedButton t Pause (materialIcon Icons.pause) 'p'

        keyboardButton =
            roundedButton t ToggleKeyboard (materialIcon Icons.keyboard) 'k'

        restartButton =
            roundedButton t Restart (materialIcon Icons.refresh) 'r'

        hideKeyboardButton =
            roundedButton t ToggleKeyboard (materialIcon Icons.keyboard_hide) 'k'

        playButton =
            roundedButton t Play (materialIcon Icons.play_arrow) 'p'

        doneCount =
            String.length dictation.prev

        restCount =
            String.length dictation.next + 1

        leftBorder =
            Border.roundEach { topLeft = t.rounding, bottomLeft = t.rounding, topRight = 0, bottomRight = 0 }

        rightBorder =
            Border.roundEach { topRight = t.rounding, bottomRight = t.rounding, topLeft = 0, bottomLeft = 0 }

        progressBar =
            let
                factor =
                    100

                offset =
                    textOffset * factor |> floor

                countFactor =
                    19.2 * factor |> floor

                ( a, b ) =
                    ( doneCount * countFactor - offset, restCount * countFactor + offset )
            in
            row [ spacing 8, width fill ]
                [ row [ width fill, height (px 22) ]
                    [ el (itemBorder t ++ [ width (fillPortion a), height fill, Background.color <| black t, leftBorder ]) <| none
                    , el (itemBorder t ++ [ width (fillPortion b), height fill, rightBorder ]) <| none
                    ]
                , text <| printTime duration
                ]

        hiddenInput =
            ifMobile device
                -- html <|
                -- Html.input
                --     [ Html.Attributes.id hiddenInputId
                --     , Html.Events.onInput KeyDownBatch
                --     , Html.Attributes.value ""
                --     , Html.Attributes.style "backgroun" ""
                --     , Html.Attributes.placeholder "click here"
                --     ]
                --     []
                (el [ width fill, padding 32, moveDown 4, Background.color <| primary t, Border.rounded <| t.rounding * 4 ] <|
                    Input.text
                        ([ htmlAttribute <| Html.Attributes.id hiddenInputId
                         , htmlAttribute <| Html.Attributes.spellcheck False
                         , htmlAttribute <| Html.Attributes.autocomplete False
                         , htmlAttribute <| Html.Attributes.autofocus True
                         , htmlAttribute <| Html.Attributes.type_ "password"
                         ]
                            ++ inputStyle t
                        )
                        { onChange = KeyDownBatch, text = "", placeholder = Just <| Input.placeholder [] <| text "Tastatur öffnen", label = Input.labelHidden "hidden-input" }
                )
                (el [ width (px 0), height (px 0), clip ] <|
                    html <|
                        Html.input [ Html.Attributes.id hiddenInputId, Html.Events.onInput KeyDownBatch, Html.Attributes.value "" ] []
                )

        layer =
            case ( mods.shift, mods.mod3, mods.mod4 ) of
                ( _, False, True ) ->
                    4

                ( True, False, False ) ->
                    2

                ( False, True, False ) ->
                    3

                ( True, True, False ) ->
                    5

                ( False, True, True ) ->
                    6

                _ ->
                    1
    in
    column
        [ spacing 48
        , width fill
        , topLeftBar device
            [ backButton t Exit
            , if paused then
                playButton

              else
                pauseButton
            , ifMobile device
                none
                (if showKeyboard then
                    hideKeyboardButton

                 else
                    keyboardButton
                )
            , if String.isEmpty dictation.prev && paused then
                none

              else
                restartButton
            ]
        , inFront hiddenInput
        ]
        [ el (ifMobile device [ paddingXY 0 32, width fill, clip ] [ paddingXY 64 8 ]) <|
            if paused then
                pausedEl

            else
                typewriter
        , ifMobile device
            none
            (if showKeyboard then
                --image [ width fill ] { src = layerUrl settings.layout layer, description = "" }
                sizedImage 535 183 [ width fill ] { src = layerUrl settings.layout layer, description = "" }

             else
                none
            )
        , progressBar
        ]



-- Sub


subscriptions : TypingModel -> Sub TypingMsg
subscriptions model =
    Sub.batch
        [ keyDecoder |> Decode.map (ignoreCharacters << toKey) |> Browser.Events.onKeyDown
        , keyDecoder |> Decode.map (KeyUp << toKey) |> Browser.Events.onKeyUp
        , if model.paused then
            Sub.none

          else
            Time.every (ticksPerSecond * 1000) (\_ -> TickTypingTime)
        , Browser.Events.onAnimationFrameDelta AnimationFrameDelta
        ]


keyDecoder : Decoder ( String, String )
keyDecoder =
    Decode.map2
        (\a b -> ( a, b ))
        (Decode.field "key" Decode.decoderString)
        (Decode.field "code" Decode.decoderString)


toKey : ( String, String ) -> KeyboardKey
toKey ( key, code ) =
    case String.uncons key of
        Just ( char, "" ) ->
            Character char

        _ ->
            case key of
                -- because of https://github.com/janekx21/NeoTrain/issues/8
                "Enter" ->
                    Control "Enter"

                "Tab" ->
                    Control "Tab"

                _ ->
                    Control code


ignoreCharacters : KeyboardKey -> TypingMsg
ignoreCharacters key =
    case key of
        Character _ ->
            NoOp

        Control _ ->
            KeyDown key


ticksPerSecond =
    0.1
