module Theme exposing (..)

import Element as UI
import Element.Font as Font


blue : UI.Color
blue =
    UI.rgb255 51 102 255


pink : UI.Color
pink =
    UI.rgb255 150 50 150


black =
    UI.rgb255 0 0 0


defaultFonts =
    Font.family
        [ Font.typeface "Roboto"
        , Font.monospace
        ]


sizeLimits min max =
    UI.fill
        |> UI.maximum max
        |> UI.minimum min
