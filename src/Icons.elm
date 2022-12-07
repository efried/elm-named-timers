module Icons exposing
    ( bell
    , bellOff
    )

import Element
import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


bell : Element.Element msg
bell =
    svgFeatherIcon "bell"
        [ Svg.path [ d "M18 8A6 6 0 0 0 6 8c0 7-3 9-3 9h18s-3-2-3-9" ] []
        , Svg.path [ d "M13.73 21a2 2 0 0 1-3.46 0" ] []
        ]
        |> Element.html


bellOff : Element.Element msg
bellOff =
    svgFeatherIcon "bell-off"
        [ Svg.path [ d "M13.73 21a2 2 0 0 1-3.46 0" ] []
        , Svg.path [ d "M18.63 13A17.89 17.89 0 0 1 18 8" ] []
        , Svg.path [ d "M6.26 6.26A5.86 5.86 0 0 0 6 8c0 7-3 9-3 9h14" ] []
        , Svg.path [ d "M18 8a6 6 0 0 0-9.33-5" ] []
        , Svg.line [ x1 "1", y1 "1", x2 "23", y2 "23" ] []
        ]
        |> Element.html
