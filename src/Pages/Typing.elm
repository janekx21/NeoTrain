module Pages.Typing exposing (..)

import Browser.Events
import Common exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Lamdera.Json as Decode exposing (Decoder)
import Material.Icons as Icons
import Task
import Time
import Types exposing (..)



-- Init


init : Lesson -> TypingModel
init lesson =
    { dictation = bookToDictation lesson
    , madeError = False
    , errors = []
    , layer = 1
    , lesson = lesson
    , duration = 0
    , paused = True
    , showKeyboard = True
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
    case typingMsg of
        KeyDown keyboardKey ->
            case updateDictation keyboardKey settings model of
                Just next ->
                    ( Just next, Cmd.none )

                Nothing ->
                    ( Just model, Time.now |> Task.perform (FinishedDictation model.errors model.lesson model.duration) )

        KeyUp keyboardKey ->
            let
                layer =
                    case keyboardKey of
                        Control key ->
                            case key of
                                -- Shift
                                "CapsLock" ->
                                    1

                                "AltGraph" ->
                                    1

                                -- ShiftLevel5
                                "Unidentified" ->
                                    1

                                _ ->
                                    model.layer

                        _ ->
                            model.layer
            in
            ( Just { model | layer = layer }, Cmd.none )

        TickTypingTime ->
            ( Just { model | duration = model.duration + ticksPerSecond }, Cmd.none )

        Pause ->
            ( Just { model | paused = True }, Cmd.none )

        Play ->
            ( Just { model | paused = False }, Cmd.none )

        ToggleKeyboard ->
            ( Just { model | showKeyboard = not model.showKeyboard }, Cmd.none )

        Exit ->
            --change page
            ( Nothing, Cmd.none )


updateDictation : KeyboardKey -> Settings -> TypingModel -> Maybe TypingModel
updateDictation keyboardKey settings typing =
    let
        decodedKey =
            case keyboardKey of
                Control key ->
                    case key of
                        "Enter" ->
                            Character '\n'

                        _ ->
                            Control key

                Character char ->
                    Character char
    in
    case decodedKey of
        Control key ->
            case key of
                "Shift" ->
                    Just { typing | layer = 2 }

                "AltGraph" ->
                    Just { typing | layer = 3 }

                "ShiftLevel5" ->
                    Just { typing | layer = 4 }

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
                    |> Maybe.map (\dictation -> { typing | dictation = dictation, madeError = False })


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


view : Theme -> TypingModel -> Settings -> Element TypingMsg
view t { dictation, layer, madeError, paused, showKeyboard, duration } settings =
    let
        color =
            if madeError then
                primary

            else
                secondary

        prev =
            dictation.prev
                |> String.right settings.paddingLeft
                |> String.padLeft settings.paddingLeft '\u{0000}'
                |> String.toList

        next =
            dictation.next
                |> String.left settings.paddingRight
                |> String.padRight settings.paddingRight '\u{0000}'
                |> String.toList

        typewriter =
            row [ monospace, Font.size 32 ]
                [ row [] (List.map viewChar prev)
                , el [ Background.color <| color t, Font.color <| wheat t ] <| viewChar dictation.current
                , row [ Font.color <| secondary t ] (List.map viewChar next)
                ]

        pausedEl =
            el [ centerX, monospace, Font.size 32 ] <|
                text <|
                    String.pad (settings.paddingLeft + settings.paddingRight + 1) ' ' <|
                        "Paused. Press Any Key"

        pauseButton =
            roundedButton t Pause (materialIcon Icons.pause) 'p'

        keyboardButton =
            roundedButton t ToggleKeyboard (materialIcon Icons.keyboard) 'k'

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
            row [ spacing 8, width fill ]
                [ row [ width fill, height (px 22) ]
                    [ el (itemBorder t ++ [ width (fillPortion doneCount), height fill, Background.color <| black t, leftBorder ]) <| none
                    , el (itemBorder t ++ [ width (fillPortion restCount), height fill, rightBorder ]) <| none
                    ]
                , text <| printTime duration
                ]
    in
    column
        [ spacing 48
        , topLeftBar
            [ backButton t Exit
            , if paused then
                playButton

              else
                pauseButton
            , if showKeyboard then
                hideKeyboardButton

              else
                keyboardButton
            ]
        ]
        [ if paused then
            pausedEl

          else
            typewriter
        , if showKeyboard then
            --image [ width fill ] { src = layerUrl settings.layout layer, description = "" }
            sizedImage 535 183 [ width fill ] { src = layerUrl settings.layout layer, description = "" }

          else
            none
        , progressBar
        ]



-- Sub


subscriptions : TypingModel -> Sub TypingMsg
subscriptions model =
    Sub.batch
        [ keyDecoder |> Decode.map (KeyDown << toKey) |> Browser.Events.onKeyDown
        , keyDecoder |> Decode.map (KeyUp << toKey) |> Browser.Events.onKeyUp
        , if model.paused then
            Sub.none

          else
            Time.every (ticksPerSecond * 1000) (\_ -> TickTypingTime)
        ]


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.decoderString


toKey : String -> KeyboardKey
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


ticksPerSecond =
    0.1
