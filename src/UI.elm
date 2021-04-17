module UI exposing (color, size)

import Element exposing (Color, rgb255, rgba255)


size : { large : number, xlarge : number }
size =
    { large = 32
    , xlarge = 64
    }


color : { darkBlue : Color, mediumBlue : Color, green : Color, grey : Color, white : Color, red : Color }
color =
    { darkBlue = rgb255 1 23 47
    , mediumBlue = rgb255 21 59 80
    , green = rgb255 62 168 62
    , grey = rgb255 130 138 149
    , white = rgb255 255 255 255
    , red = rgba255 230 15 15 0.596
    }
