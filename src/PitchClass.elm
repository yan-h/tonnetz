module PitchClass exposing
    ( PitchClass(..)
    , PitchClassSet
    , add
    , darkColor
    , hslColor
    , int
    , map
    , noteName
    , pitchClass
    , pitchClassList
    , pitchClassName
    , pitchClassSet
    )

import Array exposing (Array)
import Color exposing (..)
import HSLColor exposing (HSLColor)


type PitchClass
    = PitchClass Int


add : PitchClass -> PitchClass -> PitchClass
add (PitchClass p1) (PitchClass p2) =
    pitchClass (p1 + p2)


int : PitchClass -> Int
int (PitchClass i) =
    i


map : (Int -> Int) -> PitchClass -> PitchClass
map f (PitchClass i) =
    pitchClass (f i)


pitchClass : Int -> PitchClass
pitchClass i =
    PitchClass <| modBy 12 i


pitchClassName : PitchClass -> String
pitchClassName (PitchClass i) =
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


darkColor : PitchClass -> HSLColor
darkColor =
    HSLColor.modifyLuminance (\x -> x * 0.3) << hslColor


hslColor : PitchClass -> HSLColor
hslColor (PitchClass i) =
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


noteName : PitchClass -> String
noteName (PitchClass i) =
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


type PitchClassSet
    = PitchClassSet (Array Bool)


pitchClassSet : Array Bool -> PitchClassSet
pitchClassSet arr =
    PitchClassSet <|
        if Array.length arr < 12 then
            Array.append arr (Array.repeat (12 - Array.length arr) False)

        else if Array.length arr > 12 then
            Array.slice 0 12 arr

        else
            arr


pitchClassList : List Int -> PitchClassSet
pitchClassList pcs =
    PitchClassSet <|
        List.foldl (\pc arr -> Array.set (modBy 12 pc) True arr)
            (Array.repeat 12 False)
            pcs
