port module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Dict exposing (Dict, toList)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import PitchClass exposing (PitchClass(..), pitchClass)
import PitchClassButton
import Tuple exposing (first)
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (Fill(..), Length, StrokeLinejoin(..), Transform(..), px)


port audioControl : ( Int, Bool ) -> Cmd msg


keyDecoder : Decode.Decoder (Maybe Int)
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Maybe Int
toDirection string =
    case string of
        "q" ->
            Just 0

        "2" ->
            Just 1

        "w" ->
            Just 2

        "3" ->
            Just 3

        "e" ->
            Just 4

        "r" ->
            Just 5

        "5" ->
            Just 6

        "t" ->
            Just 7

        "6" ->
            Just 8

        "y" ->
            Just 9

        "7" ->
            Just 10

        "u" ->
            Just 11

        _ ->
            Nothing


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ButtonMsg PitchClass ButtonAction
    | KeyPress (Maybe Int)


type ButtonAction
    = Click
    | MouseUp
    | MouseDown
    | MouseOver
    | MouseOut


type alias Model =
    { selected : Array Bool
    }


startingModel : Model
startingModel =
    { selected = Array.fromList (List.repeat 12 False)
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( startingModel
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg curModel =
    case msg of
        ButtonMsg pc action ->
            let
                idx =
                    PitchClass.int pc
            in
            case action of
                MouseDown ->
                    ( curModel, Cmd.none )

                MouseOver ->
                    ( curModel, Cmd.none )

                MouseUp ->
                    ( curModel, Cmd.none )

                MouseOut ->
                    ( curModel, Cmd.none )

                Click ->
                    case Array.get idx curModel.selected of
                        Just btnSelected ->
                            ( { curModel | selected = Array.set idx (not btnSelected) curModel.selected }
                            , audioControl ( idx, not btnSelected )
                            )

                        _ ->
                            ( curModel, Cmd.none )

        KeyPress key ->
            case key of
                Nothing ->
                    ( curModel, Cmd.none )

                Just pc ->
                    case Array.get pc curModel.selected of
                        Just btnSelected ->
                            ( { curModel | selected = Array.set pc (not btnSelected) curModel.selected }
                            , audioControl ( pc, not btnSelected )
                            )

                        _ ->
                            ( curModel, Cmd.none )


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
        [ onKeyPress (Decode.map KeyPress keyDecoder)
        ]


tonnetzRow : Model -> PitchClass -> Html Msg
tonnetzRow model basePC =
    div [] <|
        List.map
            ((\pc ->
                PitchClassButton.viewCircle
                    { onClick = ButtonMsg pc Click, pitchClass = pc, selected = model.selected }
             )
                << PitchClass.add basePC
                << pitchClass
            )
            [ 0, 7, 14, 21, 28, 35, 42, 49, 56, 63, 70 ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ viewPitchClassPie model ]
        , div []
            [ tonnetzRow model (PitchClass 0)
            , tonnetzRow model (PitchClass 4)
            , tonnetzRow model (PitchClass 8)
            , tonnetzRow model (PitchClass 0)
            , tonnetzRow model (PitchClass 4)
            , tonnetzRow model (PitchClass 8)
            , tonnetzRow model (PitchClass 0)
            ]
        ]


viewPitchClassPie : Model -> Html Msg
viewPitchClassPie model =
    let
        toSvg : ( Int, Bool ) -> Html Msg
        toSvg ( idx, selected ) =
            let
                pc =
                    pitchClass idx
            in
            PitchClassButton.view
                { highlighted = False
                , selected = selected
                , rotation = 360 * toFloat idx / 12
                , onClick = ButtonMsg pc Click
                , onMouseDown = ButtonMsg pc MouseDown
                , onMouseOver = ButtonMsg pc MouseOver
                , onMouseOut = ButtonMsg pc MouseOut
                , onMouseUp = ButtonMsg pc MouseUp
                , pitchClass = pitchClass idx
                }
    in
    svg [ width (px 300), height (px 300) ]
        (List.map toSvg
            << List.sortBy
                (\( idx, _ ) ->
                    idx
                        + (if Array.get idx model.selected == Just True then
                            100

                           else
                            0
                          )
                )
         <|
            Array.toIndexedList model.selected
        )



{-
   examples for svg text

      sample =
          svg [ width "210mm", height "297mm", viewBox "0 0 210 297", version "1.1", id "svg8" ]
              [ defs [ id "defs2" ] []
              , metadata [ id "metadata5" ] []
              , g [ id "layer1" ]
                  [ rect [ id "rect14", width "71.815475", height "46.869045", x "52.916668", y "104.23215", style "fill:#ff0000;fill-opacity:1;stroke-width:0.26458332" ] []
                  , text_ [ style "font-style:normal;font-weight:normal;font-size:10.58333302px;line-height:1.25;font-family:sans-serif;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;stroke-width:0.26458332", x "71.815483", y "129.93452", id "text12" ]
                      [ tspan [ id "tspan10", x "71.815483", y "129.93452", style "stroke-width:0.26458332" ]
                          [ text "asdf" ]
                      ]
                  ]
              ]
-}
