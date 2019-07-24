port module Main exposing (Msg(..), main, update, view)

import Array exposing (Array)
import Browser
import Dict exposing (Dict, toList)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import PitchClass exposing (PitchClass, pitchClass)
import PitchClassButton
import Tuple exposing (first)
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (Fill(..), Length, StrokeLinejoin(..), Transform(..), px)


port audioControl : ( Int, Bool ) -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = ButtonMsg Int ButtonAction


type ButtonAction
    = Click
    | MouseUp
    | MouseDown
    | MouseOver
    | MouseOut


type alias Model =
    { buttons : Array Bool
    , selectedButtonIdx : Maybe Int
    , hoveredButtonIdx : Maybe Int
    }


startingModel : Model
startingModel =
    { buttons = Array.fromList (List.repeat 12 False)
    , selectedButtonIdx = Nothing
    , hoveredButtonIdx = Nothing
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( startingModel
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd msg )
update msg curModel =
    case msg of
        ButtonMsg idx action ->
            case action of
                MouseDown ->
                    ( { curModel | selectedButtonIdx = Just idx }, Cmd.none )

                MouseOver ->
                    ( { curModel | hoveredButtonIdx = Just idx }, Cmd.none )

                MouseUp ->
                    ( curModel, Cmd.none )

                MouseOut ->
                    ( curModel, Cmd.none )

                Click ->
                    case Array.get idx curModel.buttons of
                        Just btnSelected ->
                            ( { curModel | buttons = Array.set idx (not btnSelected) curModel.buttons }
                            , audioControl ( idx, not btnSelected )
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


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ viewPitchClassPie model ]


viewPitchClassPie : Model -> Html Msg
viewPitchClassPie model =
    let
        toSvg : ( Int, Bool ) -> Html Msg
        toSvg ( idx, selected ) =
            PitchClassButton.view
                { highlighted = False
                , selected = selected
                , rotation = 360 * toFloat idx / 12
                , onClick = ButtonMsg idx Click
                , onMouseDown = ButtonMsg idx MouseDown
                , onMouseOver = ButtonMsg idx MouseOver
                , onMouseOut = ButtonMsg idx MouseOut
                , onMouseUp = ButtonMsg idx MouseUp
                , pitchClass = pitchClass idx
                }
    in
    svg [ width (px 300), height (px 300) ]
        (List.map toSvg (Array.toIndexedList model.buttons))



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
