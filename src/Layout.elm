module Layout exposing (directions, hex2pixel)

import Animation
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Html exposing (Html, a, button, div, hr, node, text)
import Html.Attributes exposing (class, classList, href, id, style, tabindex, type_)
import Html.Events exposing (on, onClick)


hex2pixel pos =
    hexToPoint layout pos


directions =
    [ W, SE, NE, E, NW, SW ]


orientationLayoutFlatTop : Orientation
orientationLayoutFlatTop =
    { forward_matrix =
        { f0 = 3.0 / 2.0
        , f1 = 0.0
        , f2 = sqrt 3.0 / 2.0
        , f3 = sqrt 3.0
        }
    , inverse_matrix =
        { f0 = 2.0 / 3.0
        , f1 = 0.0
        , f2 = -1.0 / 3.0
        , f3 = sqrt 3.0 / 3.0
        }
    , start_angle = 0.0
    }


layout =
    { orientation = orientationLayoutFlatTop
    , origin = ( 0, 0 )
    , size = ( 50, 50 )
    }
