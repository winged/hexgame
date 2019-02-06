module Player exposing (Player)

import Animation
import Cell
import Dict exposing (Dict)
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Item exposing (BagOfItems)
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Layout exposing (directions)
import List exposing (map)


type alias Player =
    { pos : Hex
    , name : String
    , items : BagOfItems
    }


neighboringPositions : Player -> List ( Hex, Direction )
neighboringPositions player =
    map (\dir -> ( neighbor player.pos dir, dir )) directions
