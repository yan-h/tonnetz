module PitchClassButton exposing (view, viewCircle)

import Array exposing (Array)
import Color exposing (..)
import HSLColor exposing (..)
import Html exposing (Html)
import PitchClass exposing (PitchClass(..), noteName, pitchClass, pitchClassName)
import String exposing (fromFloat)
import TypedSvg exposing (circle, polygon, rect, svg, text_)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (text)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Fill(..), Length, StrokeLinejoin(..), Transform(..), px)


type alias SliceConfig msg =
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


type alias CircleConfig msg =
    { onClick : msg
    , pitchClass : PitchClass
    , selected : Array Bool
    }


grey : Int -> Color
grey x =
    rgb255 x x x


getFill : SliceConfig msg -> Fill
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


getBackgroundFill : Bool -> Fill
getBackgroundFill selected =
    Fill <|
        case selected of
            True ->
                grey 90

            False ->
                grey 170


getTextFill : Bool -> Fill
getTextFill selected =
    Fill <|
        case selected of
            True ->
                grey 255

            False ->
                grey 0


isSelected : PitchClass -> Array Bool -> Bool
isSelected (PitchClass i) selected =
    Array.get i selected == Just True


viewCircle : CircleConfig msg -> Html msg
viewCircle config =
    let
        selected =
            isSelected config.pitchClass config.selected

        colorMod =
            if selected then
                modifyLuminance (\x -> x - 0.4) << modifySaturation (\x -> x + 0.4)

            else
                identity
    in
    svg [ width (px 60), height (px 60) ]
        [ circle
            [ cx (px 30)
            , cy (px 30)
            , r (px 24)
            , stroke << convert <| PitchClass.darkColor config.pitchClass
            , fill << Fill << convert <| PitchClass.hslColor config.pitchClass
            , onClick config.onClick
            , strokeWidth <|
                if selected then
                    px 5

                else
                    px 0
            ]
            []
        , text_
            [ x (px 30)
            , y (px 30)
            , class [ "text-style" ]
            , textAnchor AnchorMiddle
            , dominantBaseline DominantBaselineMiddle
            , fill << Fill << convert <| PitchClass.darkColor config.pitchClass
            , pointerEvents "none"
            ]
            [ text <| pitchClassName config.pitchClass ]
        ]


view : SliceConfig msg -> Html msg
view config =
    let
        outerSideLength =
            75

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
        , viewBox -150 -150 300 300
        , onMouseOver config.onMouseOver
        , onMouseDown config.onMouseDown
        , onMouseOut config.onMouseOut
        , onMouseUp config.onMouseUp
        , onClick config.onClick
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( -pointX, pointY ), ( pointX, pointY ) ]
            , stroke <|
                if config.selected then
                    convert <| PitchClass.darkColor config.pitchClass

                else
                    grey 255
            , strokeWidth <|
                if config.selected then
                    px 5

                else
                    px 0
            , strokeLinejoin StrokeLinejoinRound
            , fill << Fill << convert <| PitchClass.hslColor config.pitchClass
            , transform [ Rotate config.rotation 0 0 ]
            , class [ "outlined" ]
            ]
            []
        , rotatedText config 110 (noteName config.pitchClass)
        , rotatedText config 80 (pitchClassName config.pitchClass)
        ]


rotatedText : SliceConfig msg -> Float -> String -> Html msg
rotatedText config distance str =
    text_
        [ x (px (distance * cos (degrees (config.rotation - 90))))
        , y (px (distance * sin (degrees (config.rotation - 90))))
        , class [ "text-style" ]
        , textAnchor AnchorMiddle
        , dominantBaseline DominantBaselineMiddle
        , fill << Fill << convert <| PitchClass.darkColor config.pitchClass
        ]
        [ text <| str ]
