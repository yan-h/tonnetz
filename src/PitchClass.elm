module PitchClass exposing (PitchClass, PitchClassSet, noteName, pitchClass, pitchClassList, pitchClassName, pitchClassSet)

import Array exposing (Array)


type PitchClass
    = PitchClass Int


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
