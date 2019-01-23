module Block exposing (Block, main, size, toForm)

import Basics exposing (..)
import Collage exposing (..)
import Color exposing (Color)
import Element exposing (..)
import Html exposing (..)


type alias Block =
    { color : Color }


size : Float
size =
    25


toForm : Block -> Form
toForm block =
    let
        shape =
            square size

        border =
            outlined (solid Color.black) shape
    in
    group [ filled block.color shape, border ]


el : Html msg
el =
    show 42 |> Element.toHtml


main : Html msg
main =
    [ Block Color.blue |> toForm ]
        |> collage 400 400
        |> Element.toHtml
