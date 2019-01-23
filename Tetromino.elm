module Tetromino exposing (Location, Tetromino)

import Basics exposing (..)
import Block exposing (Block)
import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Html exposing (..)
import List


type alias Location =
    ( Int, Int )


type alias Tetromino =
    { shape : List Location
    , block : Block
    , pivot : { r : Float, c : Float }
    , rows : Int
    , cols : Int
    }


toForm : Tetromino -> Form
toForm { shape, block } =
    let
        form =
            Block.toForm block

        translate ( row, col ) =
            move ( toFloat col * Block.size, toFloat row * Block.size ) form

        forms =
            List.map translate shape
    in
    group forms


i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , block = Block Color.lightBlue
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 4
    , cols = 1
    }


j : Tetromino
j =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, -1 )
        ]
    , block = Block Color.lightBlue
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


drawPivot : Tetromino -> Form
drawPivot { pivot } =
    let
        dot =
            circle 5 |> filled Color.black

        translate =
            move ( pivot.c * Block.size, pivot.r * Block.size )
    in
    translate dot


rotateLocation : { r : Float, c : Float } -> Float -> Location -> Location
rotateLocation pivot angle ( row, col ) =
    let
        rowOrigin =
            toFloat row - pivot.r

        colOrigin =
            toFloat col - pivot.c

        ( s, c ) =
            ( sin angle, cos angle )

        rowRotated =
            rowOrigin * c - colOrigin * s

        colRotated =
            rowOrigin * s + colOrigin * c
    in
    ( rowRotated + pivot.r |> round, colRotated + pivot.c |> round )


rotate : Tetromino -> Tetromino
rotate tetromino =
    let
        rotateHelper =
            rotateLocation tetromino.pivot (degrees 90)

        newShape =
            tetromino.shape |> List.map rotateHelper
    in
    { tetromino
        | shape = newShape
        , rows = tetromino.cols
        , cols = tetromino.rows
    }


tetromino =
    j |> rotate |> rotate


main : Html msg
main =
    collage 400 400 [ toForm tetromino, drawPivot tetromino ] |> Element.toHtml
