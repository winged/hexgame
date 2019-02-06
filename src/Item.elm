module Item exposing (BagItem, BagOfItems, Item, initialPlayerItems, randomItem)

import Random


type Item
    = Bomb
    | Hammer
    | Coin
    | Food


type alias BagItem =
    ( Item, Int )


type alias BagOfItems =
    List BagItem


randomItem : Random.Seed -> ( Item, Random.Seed )
randomItem seed =
    let
        ( choice, nseed ) =
            Random.step (Random.int 0 3) seed
    in
    case choice of
        -- todo probability weights
        0 ->
            ( Food, nseed )

        1 ->
            ( Hammer, nseed )

        2 ->
            ( Coin, nseed )

        3 ->
            ( Bomb, nseed )

        _ ->
            ( Food, nseed )


initialPlayerItems =
    [ ( Coin, 10 ), ( Food, 15 ) ]
