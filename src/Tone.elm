module Tone exposing
    ( Tone(..)
    , add
    , darkColor
    , hslColor
    , int
    , letterName
    , map
    , name
    , tone
    )

import Array exposing (Array)
import Color exposing (..)
import HSLColor exposing (HSLColor)


type Tone
    = Tone Int


add : Tone -> Tone -> Tone
add (Tone p1) (Tone p2) =
    tone (p1 + p2)


int : Tone -> Int
int (Tone i) =
    i


map : (Int -> Int) -> Tone -> Tone
map f (Tone i) =
    tone (f i)


tone : Int -> Tone
tone i =
    Tone <| modBy 12 i


name : Tone -> String
name (Tone i) =
    if i < 10 then
        String.fromInt i

    else if i == 10 then
        "t"

    else if i == 11 then
        "e"

    else
        "?"


type alias RGB =
    { r : Float
    , g : Float
    , b : Float
    }


reify : RGB -> Color
reify rgb =
    Color.rgb rgb.r rgb.g rgb.b


darkColor : Tone -> HSLColor
darkColor =
    HSLColor.modifyLuminance (\x -> x * 0.3) << hslColor


hslColor : Tone -> HSLColor
hslColor (Tone i) =
    let
        f h =
            HSLColor h 0.6 0.75
    in
    f <|
        case i of
            0 ->
                0

            1 ->
                0.0833

            2 ->
                0.1667

            3 ->
                0.25

            4 ->
                0.3333

            5 ->
                0.4167

            6 ->
                0.5

            7 ->
                0.5833

            8 ->
                0.6667

            9 ->
                0.75

            10 ->
                0.8333

            11 ->
                0.9167

            _ ->
                0


letterName : Tone -> String
letterName (Tone i) =
    case i of
        0 ->
            "C"

        1 ->
            "D♭"

        2 ->
            "D"

        3 ->
            "E♭"

        4 ->
            "E"

        5 ->
            "F"

        6 ->
            "G♭"

        7 ->
            "G"

        8 ->
            "A♭"

        9 ->
            "A"

        10 ->
            "B♭"

        11 ->
            "B"

        _ ->
            "?"
