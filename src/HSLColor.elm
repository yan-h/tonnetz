module HSLColor exposing (HSLColor, convert, modifyLuminance, modifySaturation)

import Color exposing (Color)


type alias HSLColor =
    { h : Float
    , s : Float
    , l : Float
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
