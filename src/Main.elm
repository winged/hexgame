
import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text, node, a, br)
import Html.Events exposing (onClick)
import Html.Attributes exposing (type_, class, style, href)
import Random

import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)

import Animation exposing (px)

type Item = Bomb
          | Food
          | Coin

main =
  Browser.document {
          init = init,
          view = view,
          update = update,
          subscriptions = subscriptions
  }

init: () -> (Model, Cmd Msg)
init  _ = (initialState, Cmd.none)


initialState = {
        player = {
                pos   = AxialHex(1, 3),
                name  = "Dave",
                items = [(Coin, 10), (Food, 15)] },
        arena = Dict.empty,
        centerPlayer = Animation.style [ Animation.translate (px 400) (px 400 ) ] }


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.centerPlayer ]


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
        arena: Arena,
        centerPlayer: Animation.State }


type Msg = Move Direction
         | Noop
         | Animate Animation.Msg


move: Model -> Direction -> Model
move mdl dir =
        let player = mdl.player
            newPlayer = { player | pos = neighbor player.pos dir}
            (ppx, ppy) = hex2pixel newPlayer.pos
            centerX = 500 - ppx
            centerY = 500 - ppy
        in
           {mdl | player=newPlayer,
                  centerPlayer = Animation.interrupt [
                          Animation.to [ Animation.translate (px centerX) (px centerY)]]
                          mdl.centerPlayer
           }


surroundings: Model -> List Field
surroundings {player, arena} =

        let pq = intQ player.pos
            pr = intR player.pos
            rangeX = List.range (pq - 12) (pq + 13)
            rangeY = List.range (pr - 11) (pr + 13)
            elements = List.concat (List.map (\x -> List.map (\y -> AxialHex (x, y)) rangeY) rangeX)
        in
           List.map (\pos -> fieldAt pos arena) elements


update: Msg -> Model -> (Model, Cmd Msg)
update msg mdl =
  case msg of
    Move dir -> (move mdl dir, Cmd.none)
    Noop -> (mdl, Cmd.none)
    Animate animMsg -> ({mdl|centerPlayer = Animation.update animMsg mdl.centerPlayer}, Cmd.none)  -- TODO implement


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


makeClass hasPlayer isClickable navigable =
        case navigable of
                True -> case (hasPlayer, isClickable) of
                        (False, False) -> class "hexagon"
                        (False, True)  -> class "hexagon clickable"
                        (True, False)  -> class "hexagon has-player"
                        (True, True)   -> class "hexagon has-player" -- fail case, just for completeness
                False -> class "hexagon blocked"


isBlock: Hex -> Bool
isBlock pos =
        let seed      = Random.initialSeed ((intQ pos) * (intR pos))
            (val, _)  = Random.step (Random.int 0 3) seed
        in
           val == 0



fieldPosStyle field =
        let (pxX, pxY) = hex2pixel field.pos
        in
            [style "left" (String.fromFloat pxX ++ "px"),
             style "top"  (String.fromFloat pxY ++ "px")]

viewField: Model -> Field -> Html Msg
viewField mdl field =
        let player = mdl.player
            posStyle = fieldPosStyle field
            navigable = not (isBlock field.pos)
            clickableEvents = if navigable then clickableIfNeighbor field.pos player.pos else []
            isClickable = navigable && not (List.isEmpty clickableEvents)
            hasPlayer = eq player.pos field.pos
            classes = makeClass hasPlayer isClickable navigable
        in
            div (classes :: posStyle ++ clickableEvents)  []


view: Model -> Browser.Document Msg
view mdl =
        let fields = List.map (viewField mdl) (surroundings mdl)
        in
           {
                   title = "Hex Game",
                   body = [
                           div [] [node "style" [type_ "text/css"] [ text "@import url(assets/grid.css);" ],
                           div [] [],
                           div [class "board"] [
                                   div ((class "field") :: Animation.render mdl.centerPlayer) (fields) ] ],

                           div [class "info"] [
                                   text "Copyright: Dave V",
                                   br [] [],
                                   a [href "https://github.com/winged/hexgame"] [text "Source @ Github"] ] ] }
