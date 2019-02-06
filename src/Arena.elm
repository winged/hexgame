module Arena exposing (Arena, cellAt, extendArena, renderArena)

import Animation
import Cell
import Dict exposing (Dict)
import Hexagons.Hex exposing (..)
import Html exposing (div)
import Html.Attributes exposing (class)
import List exposing (concat, filter, map, range)
import Random


type alias Arena =
    Dict ( Int, Int ) Cell.Cell


cellAt pos arena =
    let
        pq =
            intQ pos

        pr =
            intR pos
    in
    case Dict.get ( pq, pr ) arena of
        Just cell ->
            cell

        Nothing ->
            Cell.makeCell pos


akey : Hex -> ( Int, Int )
akey pos =
    ( intR pos
    , intQ pos
    )


extendArena : Arena -> Hex -> Arena
extendArena arena pos =
    let
        ( pr, pq ) =
            akey pos

        rangeX =
            range (pq - 25) (pq + 25)

        rangeY =
            range (pr - 25) (pr + 25)

        elements =
            concat (map (\x -> map (\y -> AxialHex ( x, y )) rangeY) rangeX)

        missingPos =
            filter (\fpos -> Dict.member (akey fpos) arena) elements

        missingCells =
            Dict.fromList (map (\fpos -> ( akey fpos, Cell.makeCell fpos )) missingPos)
    in
    Dict.union arena missingCells


surroundings : Hex -> Arena -> List Cell.Cell
surroundings center arena =
    let
        ( pr, pq ) =
            ( intR center
            , intQ center
            )

        rangeX =
            List.range (pq - 16) (pq + 13)

        rangeY =
            List.range (pr - 13) (pr + 13)

        elements =
            List.concat (List.map (\x -> List.map (\y -> AxialHex ( x, y )) rangeY) rangeX)
    in
    List.map (\pos -> cellAt pos arena) elements


renderArena mdl =
    let
        fields =
            map (Cell.viewCell mdl.player.pos) (surroundings mdl.player.pos mdl.arena)
    in
    div [ class "board" ]
        [ div (class "field" :: Animation.render mdl.centerPlayer) fields
        ]
