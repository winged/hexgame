
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
                        pos   = AxialHex(3, 3),
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
            rangeX = List.range (pq - 15) (pq + 17)
            rangeY = List.range (pr - 15) (pr + 17)
            elements = List.concat (List.map (\x -> List.map (\y -> AxialHex (x, y)) rangeY) rangeX)
        in
           List.map (\pos -> fieldAt pos arena) elements


update msg mdl =
  case msg of
    Move dir -> {mdl | player = move mdl.player dir }
    Noop -> mdl


layout = {
        orientation = orientationLayoutPointy,
        origin = (0, 0),
        size = (50,50)
  }


directions = [W, SE, NE, E , NW, SW]



hex2pixel pos =
        hexToPoint layout pos

clickableIfNeighbor fpos ppos =
        let clickableDirs = List.filter (\dir -> eq fpos (neighbor ppos dir)) directions
        in
            List.map (\dir -> onClick (Move dir)) clickableDirs


makeClass hasPlayer isClickable =
        case (hasPlayer, isClickable) of
                (False, False) -> class "hexagon"
                (False, True)  -> class "hexagon clickable"
                (True, False)  -> class "hexagon has-player"
                (True, True)   -> class "hexagon has-player" -- fail case, just for completeness


viewField: Player -> Field -> Html Msg
viewField player field =
        let (pxX, pxY) = hex2pixel field.pos
            posStyle = [style "left" (String.fromFloat pxX ++ "px"),
                        style "top"  (String.fromFloat pxY ++ "px")]
            clickableEvents = clickableIfNeighbor field.pos player.pos
            isClickable = not (List.isEmpty clickableEvents)
            hasPlayer = eq player.pos field.pos
            classes = makeClass hasPlayer isClickable
        in
            div (classes :: posStyle ++ clickableEvents)  []


view: Model -> Html Msg
view mdl =
        let fields = List.map (viewField mdl.player) (surroundings mdl)
            info = [
                     div [class "position"] [ text ("Q = " ++ String.fromInt (intQ mdl.player.pos)) ],
                     div [class "position"] [ text ("R = " ++ String.fromInt (intR mdl.player.pos)) ] ]
        in
        div [] [node "style" [type_ "text/css"] [ text "@import url(assets/grid.css);" ],
                div [] [],
                div [class "field"] (fields) ]
