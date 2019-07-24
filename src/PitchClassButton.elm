module PitchClassButton exposing (view)

import Color exposing (..)
import Html exposing (Html)
import PitchClass exposing (PitchClass, noteName, pitchClass, pitchClassName)
import String exposing (fromFloat)
import TypedSvg exposing (circle, polygon, rect, svg, text_)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (text)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Fill(..), Length, StrokeLinejoin(..), Transform(..), px)


type alias Config msg =
    { highlighted : Bool
    , selected : Bool
    , rotation : Float
    , pitchClass : PitchClass
    , onClick : msg
    , onMouseDown : msg
    , onMouseOver : msg
    , onMouseOut : msg
    , onMouseUp : msg
    }



{-

   update : Msg -> Model -> Model
   update msg model =
       case msg of
           MouseOver ->
               { model | highlighted = True }

           MouseOut ->
               { model | highlighted = False }

           MouseDown ->
               { model | highlighted = True }

           MouseUp ->
               { model | highlighted = True }

           Click ->
               { model
                   | highlighted = True
                   , selected = not model.selected
               }
-}


grey : Int -> Color
grey x =
    rgb255 x x x


getFill : Config msg -> Fill
getFill config =
    Fill
        (case ( config.selected, config.highlighted ) of
            ( True, True ) ->
                grey 120

            ( True, False ) ->
                grey 90

            ( False, True ) ->
                grey 200

            ( False, False ) ->
                grey 170
        )


view : Config msg -> Html msg
view config =
    let
        outerSideLength =
            26

        sideRatio =
            1.93185

        pointX =
            sideRatio * outerSideLength * cos (degrees 75)

        pointY =
            -sideRatio * outerSideLength * sin (degrees 75)
    in
    svg
        [ width (px 300)
        , height (px 300)
        , viewBox -50 -50 100 100
        , onMouseOver config.onMouseOver
        , onMouseDown config.onMouseDown
        , onMouseOut config.onMouseOut
        , onMouseUp config.onMouseUp
        , onClick config.onClick
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( -pointX, pointY ), ( pointX, pointY ) ]
            , stroke (grey 0)
            , strokeWidth (px 1.2)
            , strokeLinejoin StrokeLinejoinRound
            , fill (getFill config)
            , transform [ Rotate config.rotation 0 0 ]
            , class [ "outlined" ]
            ]
            []
        , rotatedText config 39 (noteName config.pitchClass)
        , rotatedText config 26 (pitchClassName config.pitchClass)
        ]


rotatedText : Config msg -> Float -> String -> Html msg
rotatedText config distance str =
    text_
        [ x (px (distance * cos (degrees (config.rotation - 90))))
        , y (px (distance * sin (degrees (config.rotation - 90))))
        , class [ "text-style" ]
        , textAnchor AnchorMiddle
        , dominantBaseline DominantBaselineMiddle
        , fill <|
            Fill <|
                if config.selected then
                    grey 255

                else
                    grey 0
        ]
        [ text <| str ]
