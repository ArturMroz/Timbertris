module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Pallette =
    { red : String
    , orange : String
    , yellow : String
    , green : String
    , purple : String
    , lightblue : String
    , blue : String
    , darkgray : String
    , lightgray : String
    }


nord : Pallette
nord =
    { red = "#bf616a"
    , orange = "#d08770"
    , yellow = "#ebcb8b"
    , green = "#a3be8c"
    , purple = "#b48ead"
    , lightblue = "#88c0d0"
    , blue = "#5e81ac"
    , darkgray = "#3b4252"

    -- , darkgray = "#2e3440"
    , lightgray = "#d8dee9"
    }


pallete =
    nord


size_ : Int
size_ =
    35


size =
    String.fromInt size_


board children =
    svg
        [ width <| String.fromInt <| (size_ * 10)
        , height <| String.fromInt <| (size_ * 20)
        ]
        ([ rect
            [ width <| String.fromInt <| (size_ * 10)
            , height <| String.fromInt <| (size_ * 20)
            , fill pallete.darkgray
            ]
            []
         ]
            ++ children
        )


type alias Pos =
    ( Int, Int )


type alias Tetromino =
    { shape : List Pos
    , color : String
    , pivot : { r : Float, c : Float }
    , rows : Int
    , cols : Int
    }


toForm : Tetromino -> List (Svg msg)
toForm { shape, color } =
    let
        basicB =
            [ width size, height size, fill color ]

        pos location =
            String.fromInt ((location + 1) * size_)

        translate ( x_, y_ ) =
            rect (basicB ++ [ x (pos x_), y (pos y_) ]) []

        forms =
            List.map translate shape
    in
    forms


rotateLocation : { r : Float, c : Float } -> Float -> Pos -> Pos
rotateLocation pivot angle ( row, col ) =
    let
        rowOrigin =
            toFloat row - pivot.r

        colOrigin =
            toFloat col - pivot.c

        ( sn, c ) =
            ( sin angle, cos angle )

        rowRotated =
            rowOrigin * c - colOrigin * sn

        colRotated =
            rowOrigin * sn + colOrigin * c
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


shift : Pos -> Tetromino -> Tetromino
shift ( row, col ) tetromino =
    let
        shiftHelper ( x, y ) =
            ( x + row, y + col )

        newShape =
            List.map shiftHelper tetromino.shape
    in
    { tetromino | shape = newShape }


z : Tetromino
z =
    { shape =
        [ ( 1, -1 )
        , ( 1, 0 )
        , ( 0, 0 )
        , ( 0, 1 )
        ]
    , color = nord.red
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 2
    , cols = 3
    }


s : Tetromino
s =
    { shape =
        [ ( -1, -1 )
        , ( -1, 0 )
        , ( 0, 0 )
        , ( 0, 1 )
        ]
    , color = nord.lightblue
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 2
    , cols = 3
    }


t : Tetromino
t =
    { shape =
        [ ( 0, -1 )
        , ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 0 )
        ]
    , color = pallete.purple
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 2
    , cols = 3
    }


i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , color = nord.green
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 4
    , cols = 1
    }


l : Tetromino
l =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
    , color = nord.blue
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


j : Tetromino
j =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, -1 )
        ]
    , color = pallete.orange
    , pivot = { r = 0.0, c = 0.0 }
    , rows = 3
    , cols = 2
    }


o : Tetromino
o =
    { shape =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 1 )
        , ( -1, 0 )
        ]
    , color = pallete.yellow
    , pivot = { r = -0.5, c = 0.5 }
    , rows = 2
    , cols = 3
    }


type alias Model =
    { content : String }


init : Model
init =
    { content = "44" }


type Msg
    = Rotate
    | Shift ( Int, Int )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Shift ( x, y ) ->
            { model | content = " change" }

        Rotate ->
            { model | content = " change" }


view model =
    div []
        [ board (z |> rotate |> shift ( 4, 6 ) |> toForm)
        , div []
            [ button [ onClick (Shift ( -1, 0 )) ] [ Html.text "Left" ]
            , button [ onClick (Shift ( 1, 0 )) ] [ Html.text "Right" ]
            , button [ onClick (Shift ( 0, -1 )) ] [ Html.text "Down" ]
            , button [ onClick Rotate ] [ Html.text "Rotate" ]
            ]
        ]
