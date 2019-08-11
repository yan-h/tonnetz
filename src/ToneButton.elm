module ToneButton exposing (viewCircle, viewSlice, viewTriangle)

import Array exposing (Array)
import Color exposing (..)
import HSLColor exposing (..)
import Html exposing (Html)
import String exposing (fromFloat)
import Tone exposing (Tone(..), tone)
import ToneSet exposing (ToneSet)
import TypedSvg exposing (circle, polygon, rect, svg, text_)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Core exposing (text)
import TypedSvg.Events exposing (..)
import TypedSvg.Types exposing (..)


type alias SliceConfig msg =
    { rotation : Float
    , tone : Tone
    , selectedTones : ToneSet
    , previewTones : ToneSet
    , onClick : msg
    , onMouseOver : msg
    , onMouseOut : msg
    }


type alias CircleConfig msg =
    { position : ( Int, Int )
    , tone : Tone
    , selectedTones : ToneSet
    , previewTones : ToneSet
    , onClick : msg
    , onMouseOver : msg
    , onMouseOut : msg
    }


type alias TriangleConfig msg =
    { position : ( Int, Int )
    , tone : Tone
    , selectedTones : ToneSet
    , previewTones : ToneSet
    , onClick : ToneSet -> msg
    , onMouseOver : ToneSet -> msg
    , onMouseOut : ToneSet -> msg
    }


grey : Int -> Color
grey x =
    rgb255 x x x


isSelected : Tone -> Array Bool -> Bool
isSelected (Tone i) selectedTones =
    Array.get i selectedTones == Just True


viewTriangle : TriangleConfig msg -> Html msg
viewTriangle config =
    let
        xPos =
            Tuple.first config.position

        yPos =
            Tuple.second config.position

        majorToneSet =
            ToneSet.fromToneList
                [ Tone.add config.tone (tone 1)
                , Tone.add config.tone (tone 5)
                , Tone.add config.tone (tone 8)
                ]

        minorToneSet =
            ToneSet.fromToneList
                [ Tone.add config.tone (tone 0)
                , Tone.add config.tone (tone 5)
                , Tone.add config.tone (tone 8)
                ]
    in
    if xPos == 0 || yPos == 0 then
        svg [] []

    else
        svg
            [ x << px <| (toFloat <| xPos * 64) - 32
            , y << px <| (toFloat <| yPos * 64) - 32
            ]
            [ polygon
                [ points <|
                    [ ( 0, 64 ), ( 0, 0 ), ( 64, 0 ) ]
                , fill
                    << Fill
                  <|
                    grey 240
                , onClick <| config.onClick majorToneSet
                , onMouseOver <| config.onMouseOver majorToneSet
                , onMouseOut <| config.onMouseOut majorToneSet
                ]
                []
            , polygon
                [ points <|
                    [ ( 0, 64 ), ( 64, 64 ), ( 64, 0 ) ]
                , fill
                    << Fill
                  <|
                    grey 220
                , onClick <| config.onClick minorToneSet
                , onMouseOver <| config.onMouseOver minorToneSet
                , onMouseOut <| config.onMouseOver minorToneSet
                ]
                []
            ]


viewCircle : CircleConfig msg -> Html msg
viewCircle config =
    let
        toneSelected =
            ToneSet.member config.tone config.selectedTones

        previewSelected =
            ToneSet.member config.tone config.previewTones

        ( frontColor, backColor ) =
            if toneSelected then
                ( Tone.hslColor, Tone.darkColor )

            else
                ( Tone.darkColor, Tone.hslColor )

        frontFill =
            Fill << convert << frontColor <| config.tone

        backFill =
            Fill << convert << backColor <| config.tone
    in
    svg
        [ x << px << toFloat <| Tuple.first config.position * 64
        , y << px << toFloat <| Tuple.second config.position * 64
        ]
        [ circle
            [ cx (px 32)
            , cy (px 32)
            , r (px 24)
            , stroke << convert <| Tone.darkerColor config.tone
            , fill backFill
            , onClick config.onClick
            , onMouseOver config.onMouseOver
            , onMouseOut config.onMouseOut
            , strokeWidth
                << px
              <|
                case ( toneSelected, previewSelected ) of
                    ( _, True ) ->
                        5

                    _ ->
                        0
            , strokeLinecap StrokeLinecapRound
            ]
            []
        , text_
            [ x (px 32)
            , y (px 33)
            , class [ "text-style" ]
            , textAnchor AnchorMiddle
            , dominantBaseline DominantBaselineMiddle
            , fill frontFill
            , pointerEvents "none"
            ]
            [ text <| Tone.letterName config.tone ]
        ]


viewSlice : SliceConfig msg -> Html msg
viewSlice config =
    let
        outerSideLength =
            75

        sideRatio =
            1.93185

        pointX =
            sideRatio * outerSideLength * cos (degrees 75)

        pointY =
            -sideRatio * outerSideLength * sin (degrees 75)

        toneSelected =
            ToneSet.member config.tone config.selectedTones

        previewSelected =
            ToneSet.member config.tone config.previewTones

        ( frontColor, backColor ) =
            if toneSelected then
                ( Tone.hslColor, Tone.darkColor )

            else
                ( Tone.darkColor, Tone.hslColor )

        frontFill =
            Fill << convert << frontColor <| config.tone

        backFill =
            Fill << convert << backColor <| config.tone
    in
    svg
        [ width (px 300)
        , height (px 300)
        , viewBox -150 -150 300 300
        , onMouseOver config.onMouseOver
        , onMouseOut config.onMouseOut
        , onClick config.onClick
        ]
        [ polygon
            [ points [ ( 0, 0 ), ( -pointX, pointY ), ( pointX, pointY ) ]
            , stroke << convert <| Tone.darkerColor config.tone
            , strokeWidth
                << px
              <|
                case ( toneSelected, previewSelected ) of
                    ( _, True ) ->
                        5

                    _ ->
                        0
            , strokeLinejoin StrokeLinejoinRound
            , fill backFill
            , transform [ Rotate config.rotation 0 0 ]
            , class [ "outlined" ]
            , strokeLinecap StrokeLinecapRound
            ]
            []
        , rotatedText config 110 (Tone.letterName config.tone) frontFill
        , rotatedText config 80 (Tone.name config.tone) frontFill
        ]


rotatedText : SliceConfig msg -> Float -> String -> Fill -> Html msg
rotatedText config distance str frontFill =
    text_
        [ x (px (distance * cos (degrees (config.rotation - 90))))
        , y (px (distance * sin (degrees (config.rotation - 90))))
        , class [ "text-style" ]
        , textAnchor AnchorMiddle
        , dominantBaseline DominantBaselineMiddle
        , fill frontFill
        ]
        [ text <| str ]
