module Cell exposing (Cell, CellState, isNavigable, makeCell, viewCell)

import Hexagons.Hex exposing (..)
import Html exposing (Html, a, button, div, hr, node, text)
import Html.Attributes exposing (classList, style)
import Item exposing (BagOfItems, Item(..))
import Layout exposing (hex2pixel)
import Random


type CellState
    = Normal
    | Hole
    | Block


type alias Cell =
    { pos : Hex
    , state : CellState
    , items : BagOfItems
    }


cellState : Random.Seed -> ( CellState, Random.Seed )
cellState seed =
    let
        ( stateChoice, nseed ) =
            Random.step (Random.int 0 7) seed
    in
    case stateChoice of
        0 ->
            ( Block, nseed )

        1 ->
            ( Block, nseed )

        2 ->
            ( Hole, nseed )

        _ ->
            ( Normal, nseed )


cellItems : Random.Seed -> ( Item.BagOfItems, Random.Seed )
cellItems seed =
    let
        ( choice, nseed ) =
            Random.step (Random.int 0 10) seed

        ( item, nseed2 ) =
            Item.randomItem nseed
    in
    case choice of
        0 ->
            ( [ ( item, 1 ) ], nseed2 )

        1 ->
            let
                ( item2, nseed3 ) =
                    Item.randomItem nseed2
            in
            ( [ ( item2, 1 ), ( item, 1 ) ], nseed3 )

        _ ->
            ( [], nseed2 )


makeCell : Hex -> Cell
makeCell pos =
    let
        seed =
            Random.initialSeed ((intQ pos - 31337) * (intR pos + 1337))

        ( newState, nseed ) =
            cellState seed

        ( items, _ ) =
            cellItems nseed
    in
    { pos = pos
    , state = newState
    , items = items
    }


isNavigable : Cell -> Bool
isNavigable field =
    field.state == Normal


viewCell : Hex -> Cell -> Html msg
viewCell playerPos field =
    let
        posStyle =
            let
                ( pxX, pxY ) =
                    hex2pixel field.pos
            in
            [ style "left" (String.fromFloat pxX ++ "px")
            , style "top" (String.fromFloat pxY ++ "px")
            ]

        events =
            -- neighborEvents field.pos playerPos
            []

        classes =
            classList
                [ ( "hexagon", True )
                , ( "clickable", not (List.isEmpty events) )
                , ( "has-player", eq playerPos field.pos )
                , ( "block", field.state == Block )
                , ( "hole", field.state == Hole )
                ]
    in
    div (classes :: posStyle ++ events) []
