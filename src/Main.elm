
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text, node)
import Html.Events exposing (onClick)
import Html.Attributes exposing (type_, class, style)

import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)

type Item = Bomb
          | Food
          | Coin

main =
  Browser.sandbox {
          init = {
                  player = {
                        pos   = AxialHex(0, 0),
                        name  = "Dave",
                        items = [(Coin, 10), (Food, 15)] },
                  arena = Dict.empty },
          view = view,
          update = update
  }


type alias BagItem = (Item, Int)

type alias BagOfItems = List BagItem

type alias Player = {
        pos: Hex,
        name: String,
        items: BagOfItems }

type FieldState = Normal
                | Hole
                | Block

type alias Field = {
        pos: Hex,
        state: FieldState,
        items: BagOfItems }

type alias Arena = Dict (Int, Int) Field

emptyField: Hex -> Field
emptyField pos = {
        pos   = pos,
        state = Normal,
        items = [] }


fieldAt: Hex -> Arena -> Field
fieldAt pos arena =
        let qr = (intQ pos, intR pos)
        in
           case Dict.get qr arena of
                Just field -> field
                Nothing -> emptyField pos



type alias Model = {
        player: Player,
        arena: Arena }


type Msg = Move Direction
         | Noop


move: Player -> Direction -> Player
move player dir =
        { player | pos = neighbor player.pos dir}


surroundings: Model -> List Field
surroundings {player, arena} =

        let pq = intQ player.pos
            pr = intR player.pos
            -- rangeX = List.range (pq - 20) (pq + 20)
            -- rangeY = List.range (pr - 20) (pr + 20)
            rangeX = List.range (-5) (20)
            rangeY = List.range (-5) (20)
            elements = List.concat (List.map (\x -> List.map (\y -> AxialHex (x, y)) rangeY) rangeX)
        in
           List.map (\pos -> fieldAt pos arena) elements


update msg mdl =
  case msg of
    Move dir -> {mdl | player = move mdl.player dir }
    Noop -> mdl


controls = [
        button [ onClick (Move W)  ] [ text "west" ],
        button [ onClick (Move SE) ] [ text "south-east" ],
        button [ onClick (Move NE) ] [ text "north-east" ],
        node "br" [] [],
        button [ onClick (Move E)  ] [ text "east" ],
        button [ onClick (Move NW) ] [ text "north-west" ],
        button [ onClick (Move SW) ] [ text "south-west" ] ]

layout = {
        orientation = orientationLayoutPointy,
        origin = (0, 0),
        size = (50,50)
  }


hex2pixel pos =
        hexToPoint layout pos


viewField: Player -> Field -> Html msg
viewField player field =
        let (pxX, pxY) = hex2pixel field.pos
            posStyle = [style "left" (String.fromFloat pxX ++ "px"),
                        style "top"  (String.fromFloat pxY ++ "px")]
        in
           if eq player.pos field.pos
              then div ((class "hexagon has-player") :: posStyle)  []
              else div ((class "hexagon") :: posStyle)  []

-- view: Model -> Html Msg
view mdl =
        let fields = List.map (viewField mdl.player) (surroundings mdl)
            info = [
                     div [class "position"] [ text ("Q = " ++ String.fromInt (intQ mdl.player.pos)) ],
                     div [class "position"] [ text ("R = " ++ String.fromInt (intR mdl.player.pos)) ] ]
        in
        div [] (controls ++ [
                node "style" [type_ "text/css"] [ text "@import url(assets/grid.css);" ],
                div [] [],
                div [class "field"] (fields)
         ] ++ info)
