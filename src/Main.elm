port module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Color exposing (..)
import Dict exposing (Dict, toList)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Set exposing (Set)
import Tone exposing (Tone(..), tone)
import ToneButton
import ToneSet exposing (ToneSet)
import Tuple exposing (first)
import TypedSvg exposing (circle, svg)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (Fill(..), Length, StrokeLinejoin(..), Transform(..), px)


port playTones : Array Bool -> Cmd msg


type KeyAction
    = KeyTone Int
    | Clear


keyDecoder : Decode.Decoder KeyAction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> KeyAction
toDirection string =
    case string of
        "q" ->
            KeyTone 0

        "2" ->
            KeyTone 1

        "w" ->
            KeyTone 2

        "3" ->
            KeyTone 3

        "e" ->
            KeyTone 4

        "r" ->
            KeyTone 5

        "5" ->
            KeyTone 6

        "t" ->
            KeyTone 7

        "6" ->
            KeyTone 8

        "y" ->
            KeyTone 9

        "7" ->
            KeyTone 10

        "u" ->
            KeyTone 11

        _ ->
            Clear


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ButtonMsg Tone ButtonAction
    | SetMsg (List Tone) ButtonAction -- Switch to ToneSet in elm 0.19.1
    | KeyMsg KeyAction


type ButtonAction
    = Click
    | MouseOver
    | MouseOut


type alias Model =
    { selectedTones : ToneSet
    , preview : ToneSet -> ToneSet
    }


startingModel : Model
startingModel =
    { selectedTones = ToneSet.empty
    , preview = identity
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( startingModel
    , Cmd.none
    )


updateModel : Model -> ( Model, Cmd msg )
updateModel newModel =
    ( newModel, playTones (ToneSet.toBoolArray newModel.selectedTones) )


update : Msg -> Model -> ( Model, Cmd msg )
update msg curModel =
    case msg of
        ButtonMsg pc action ->
            let
                idx =
                    Tone.int pc
            in
            case action of
                MouseOver ->
                    ( { curModel | preview = ToneSet.flip pc }, Cmd.none )

                MouseOut ->
                    ( { curModel | preview = identity }, Cmd.none )

                Click ->
                    updateModel
                        { curModel | selectedTones = ToneSet.flip pc curModel.selectedTones }

        SetMsg toneList action ->
            case action of
                MouseOver ->
                    ( { curModel | preview = always <| ToneSet.fromToneList toneList }, Cmd.none )

                MouseOut ->
                    ( { curModel | preview = identity }, Cmd.none )

                Click ->
                    updateModel { curModel | selectedTones = ToneSet.fromToneList toneList }

        KeyMsg action ->
            case action of
                Clear ->
                    updateModel { curModel | selectedTones = ToneSet.empty }

                KeyTone k ->
                    updateModel { curModel | selectedTones = ToneSet.flip (tone k) curModel.selectedTones }


updateAtIndex : Int -> (a -> a) -> Array a -> Array a
updateAtIndex idx fn array =
    case Array.get idx array of
        Nothing ->
            array

        Just a ->
            Array.set idx (fn a) array


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyPress (Decode.map KeyMsg keyDecoder)
        ]



--tonnetzSquare : Model -> Tone -> Html Msg
--tonnetzSquare model basePC =
{-
   tonnetzRow : Model -> Tone -> Html Msg
   tonnetzRow model basePC =
       div [] <|
           List.map
               ((\pc ->
                   ToneButton.viewCircle
                       { tone = pc
                       , selectedTones = model.selectedTones
                       , previewTones = model.previewTones
                       , onClick = ButtonMsg pc Click
                       , onMouseOver = ButtonMsg pc MouseOver
                       , onMouseOut = ButtonMsg pc MouseOut
                       }
                )
                   << Tone.add basePC
                   << tone
               )
               [ 0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70, 77 ]
-}


tonnetz : ( Int, Int ) -> Tone
tonnetz ( x, y ) =
    tone <| x * 7 + y * 4


tonnetzWidth : Int
tonnetzWidth =
    12


tonnetzHeight : Int
tonnetzHeight =
    9


tonnetzIndices : List ( Int, Int )
tonnetzIndices =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) (List.range 0 (tonnetzHeight - 1)))
        (List.range 0 (tonnetzWidth - 1))


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewTonePie model ]
        , div []
            [ svg [ width (px 800), height (px 600) ] <|
                List.map
                    (\pos ->
                        let
                            tone =
                                tonnetz pos
                        in
                        ToneButton.viewTriangle
                            { position = pos
                            , tone = tone
                            , selectedTones = model.selectedTones
                            , previewTones = model.preview model.selectedTones
                            , onClick = \x -> SetMsg (ToneSet.toToneList x) Click
                            , onMouseOver = \x -> SetMsg (ToneSet.toToneList x) MouseOver
                            , onMouseOut = \x -> SetMsg (ToneSet.toToneList x) MouseOut
                            }
                    )
                    tonnetzIndices
                    ++ List.map
                        (\pos ->
                            let
                                tone =
                                    tonnetz pos
                            in
                            ToneButton.viewCircle
                                { position = pos
                                , tone = tone
                                , selectedTones = model.selectedTones
                                , previewTones = model.preview model.selectedTones
                                , onClick = ButtonMsg tone Click
                                , onMouseOver = ButtonMsg tone MouseOver
                                , onMouseOut = ButtonMsg tone MouseOut
                                }
                        )
                        tonnetzIndices
            ]
        , div []
            []
        ]


viewTonePie : Model -> Html Msg
viewTonePie model =
    let
        toSvg : Tone -> Html Msg
        toSvg tone =
            ToneButton.viewSlice
                { selectedTones = model.selectedTones
                , previewTones = model.preview model.selectedTones
                , rotation = 360 * toFloat (Tone.int tone) / 12
                , onClick = ButtonMsg tone Click
                , onMouseOver = ButtonMsg tone MouseOver
                , onMouseOut = ButtonMsg tone MouseOut
                , tone = tone
                }
    in
    svg [ width (px 300), height (px 300) ]
        (List.map toSvg
            << List.sortBy
                (\(Tone i) ->
                    i
                        + (case
                            ( ToneSet.member (Tone i) model.selectedTones, ToneSet.member (Tone i) (model.preview model.selectedTones) )
                           of
                            ( True, True ) ->
                                300

                            ( False, True ) ->
                                200

                            ( True, False ) ->
                                100

                            ( False, False ) ->
                                0
                          )
                )
         <|
            List.map tone [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ]
        )
