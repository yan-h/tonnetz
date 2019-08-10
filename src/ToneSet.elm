module ToneSet exposing
    ( ToneSet
    , empty
    , equals
    , flip
    , fromBoolArray
    , fromIntList
    , fromToneList
    , member
    , set
    , toBoolArray
    , toToneList
    )

import Array exposing (Array)
import Tone exposing (Tone(..))
import Tuple exposing (first)


type ToneSet
    = ToneSet (Array Bool)


equals : ToneSet -> ToneSet -> Bool
equals (ToneSet s1) (ToneSet s2) =
    s1 == s2


empty : ToneSet
empty =
    ToneSet <| Array.repeat 12 False


member : Tone -> ToneSet -> Bool
member (Tone i) (ToneSet arr) =
    if i >= 0 && i <= 11 then
        Array.get i arr == Just True

    else
        False


flip : Tone -> ToneSet -> ToneSet
flip (Tone i) (ToneSet arr) =
    let
        cur =
            case Array.get i arr of
                Just x ->
                    x

                Nothing ->
                    False
    in
    ToneSet (Array.set i (not cur) arr)


set : Tone -> Bool -> ToneSet -> ToneSet
set (Tone i) b (ToneSet arr) =
    ToneSet (Array.set i b arr)


toToneList : ToneSet -> List Tone
toToneList (ToneSet bools) =
    first <|
        Array.foldl
            (\b ( lst, idx ) ->
                if b then
                    ( Tone idx :: lst, idx + 1 )

                else
                    ( lst, idx + 1 )
            )
            ( [], 0 )
            bools


toBoolArray : ToneSet -> Array Bool
toBoolArray (ToneSet bools) =
    bools


fromBoolArray : Array Bool -> ToneSet
fromBoolArray arr =
    ToneSet <|
        if Array.length arr < 12 then
            Array.append arr (Array.repeat (12 - Array.length arr) False)

        else if Array.length arr > 12 then
            Array.slice 0 12 arr

        else
            arr


fromToneList : List Tone -> ToneSet
fromToneList toneList =
    ToneSet <|
        List.foldl (\(Tone pc) arr -> Array.set pc True arr) (Array.repeat 12 False) toneList


fromIntList : List Int -> ToneSet
fromIntList intList =
    ToneSet <|
        List.foldl (\pc arr -> Array.set (modBy 12 pc) True arr)
            (Array.repeat 12 False)
            intList
