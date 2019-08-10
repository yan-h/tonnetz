module HSLColor exposing (HSLColor, average, convert, modifyLuminance, modifySaturation)

import Color exposing (Color)


type alias HSLColor =
    { h : Float
    , s : Float
    , l : Float
    }


average : List HSLColor -> HSLColor
average hslColors =
    let
        len =
            List.length hslColors
    in
    { h = List.sum (List.map (\x -> x.h) hslColors) / toFloat len
    , s = List.sum (List.map (\x -> x.s) hslColors) / toFloat len
    , l = List.sum (List.map (\x -> x.l) hslColors) / toFloat len
    }


convert : HSLColor -> Color
convert hslColor =
    Color.hsl hslColor.h hslColor.s hslColor.l


modifySaturation : (Float -> Float) -> HSLColor -> HSLColor
modifySaturation f hslColor =
    { hslColor | s = f hslColor.s }


modifyLuminance : (Float -> Float) -> HSLColor -> HSLColor
modifyLuminance f hslColor =
    { hslColor | l = f hslColor.l }
