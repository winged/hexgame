module Main exposing (main)

import Animation exposing (px)
import Arena
import Browser
import Browser.Dom as Dom
import Cell exposing (makeCell)
import Debug
import Dict exposing (Dict)
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Html exposing (Html, a, button, div, hr, node, text)
import Html.Attributes exposing (class, classList, href, id, style, tabindex, type_)
import Html.Events exposing (on, onClick)
import Item
import Json.Decode as Json
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Layout
import Player exposing (..)
import Task


type Msg
    = Move Direction
    | Noop
    | Action Direction
    | Animate Animation.Msg
    | HandleKeyboardEvent KeyboardEvent


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { player : Player
    , arena : Arena.Arena
    , centerPlayer : Animation.State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialState
    , Dom.focus "outermost"
        |> Task.attempt (always Noop)
    )


initialState =
    let
        player =
            { pos = AxialHex ( 0, 0 )
            , name = "Dave"
            , items = Item.initialPlayerItems
            }
    in
    { player = player
    , arena = Arena.extendArena Dict.empty player.pos
    , centerPlayer = Animation.style [ Animation.translate (px 0) (px 0) ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate [ model.centerPlayer ]


tryMove : Model -> Direction -> Model
tryMove mdl dir =
    let
        pos =
            neighbor mdl.player.pos dir

        dest =
            Arena.cellAt pos mdl.arena
    in
    if Cell.isNavigable dest then
        move mdl dir

    else
        mdl


rotateTool : Model -> Model
rotateTool mdl =
    mdl


handleKeyboard : KeyboardEvent -> Model -> ( Model, Cmd Msg )
handleKeyboard evt mdl =
    case evt.key of
        -- "right handed" keys - WASD
        Just "d" ->
            ( tryMove mdl NE, Cmd.none )

        Just "q" ->
            ( tryMove mdl SW, Cmd.none )

        Just "w" ->
            ( tryMove mdl SE, Cmd.none )

        Just "s" ->
            ( tryMove mdl NW, Cmd.none )

        Just "e" ->
            ( tryMove mdl E, Cmd.none )

        Just "a" ->
            ( tryMove mdl W, Cmd.none )

        -- "left handed" keys - IJKL
        Just "l" ->
            ( tryMove mdl NE, Cmd.none )

        Just "u" ->
            ( tryMove mdl SW, Cmd.none )

        Just "i" ->
            ( tryMove mdl SE, Cmd.none )

        Just "k" ->
            ( tryMove mdl NW, Cmd.none )

        Just "o" ->
            ( tryMove mdl E, Cmd.none )

        Just "j" ->
            ( tryMove mdl W, Cmd.none )

        Just " " ->
            ( rotateTool mdl, Cmd.none )

        _ ->
            ( mdl, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        Move dir ->
            ( move mdl dir, Cmd.none )

        Noop ->
            ( mdl, Cmd.none )

        Action _ ->
            ( mdl, Cmd.none )

        Animate animMsg ->
            ( { mdl | centerPlayer = Animation.update animMsg mdl.centerPlayer }, Cmd.none )

        HandleKeyboardEvent evt ->
            handleKeyboard evt mdl


move : Model -> Direction -> Model
move mdl dir =
    let
        player =
            mdl.player

        newPlayer =
            { player | pos = neighbor player.pos dir }

        arena =
            Arena.extendArena mdl.arena newPlayer.pos

        ( ppx, ppy ) =
            Layout.hex2pixel newPlayer.pos

        centerX =
            -ppx

        centerY =
            -ppy
    in
    { mdl
        | player = newPlayer
        , arena = arena
        , centerPlayer =
            Animation.interrupt
                [ Animation.to [ Animation.translate (px centerX) (px centerY) ]
                ]
                mdl.centerPlayer
    }


view : Model -> Browser.Document Msg
view mdl =
    { title = "Hex Game"
    , body =
        [ div
            [ id "outermost"
            , tabindex 0
            , on "keydown" <|
                Json.map HandleKeyboardEvent decodeKeyboardEvent
            ]
            [ div []
                [ node "style" [ type_ "text/css" ] [ text "@import url(assets/grid.css);" ]
                , Arena.renderArena mdl
                ]
            , div [ class "info" ]
                [ text "Copyright: "
                , a [ href "https://winged.ch" ] [ text "Dave V" ]
                , hr [] []
                , text "Use Keys UIOJKL (left handed or QWEASD (right handed) for navigation"
                , hr [] []
                , a [ href "https://github.com/winged/hexgame" ] [ text "Source @ Github" ]
                ]
            ]
        ]
    }
