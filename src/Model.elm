module Model exposing
    ( Block
    , Model
    , Palette
    , Pos
    , State
    , Tetromino
    ,  allTetrominos
       -- , clearLines

    , emptyShape
    , findFirstFullRow
    , hardDropped
    , i
    , isInBounds
    , isIntersecting
    , isValid
    , j
    , l
    , o
    , palette
    , rotate
    , rotateLocation
    ,  s
       -- , scoreLines

    , shift
    , size
    , t
    , totalCols
    , totalRows
    , z
    )

import Array exposing (Array)
import Messages exposing (Msg(..))
import Tuple exposing (first, second)


type alias Model =
    { active : Tetromino
    , ghost : Tetromino
    , next : Tetromino
    , pile : List Block
    , lines : Int
    , score : Int
    , level : Int

    -- , state : State
    }



-- type State1
--     = Playdsing
--     | Pausefdd
--     | Stoppesd


type State
    = Playing
    | Paused
    | Stopped


type alias Pos =
    ( Int, Int )


type alias Block =
    { pos : Pos
    , color : String
    }


type alias Tetromino =
    { shape : List Pos
    , color : String
    , pivot : { r : Float, c : Float }
    }


type alias Palette =
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


nord : Palette
nord =
    { red = "#bf616a"
    , orange = "#d08770"
    , yellow = "#ebcb8b"
    , green = "#a3be8c"
    , purple = "#b48ead"
    , lightblue = "#88c0d0"
    , blue = "#5e81ac"
    , darkgray = "#3b4252"
    , lightgray = "#d8dee9"
    }


palette : Palette
palette =
    nord


z : Tetromino
z =
    { shape = [ ( -1, 0 ), ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]
    , color = palette.red
    , pivot = { r = 0.0, c = 0.0 }
    }


s : Tetromino
s =
    { shape = [ ( -1, 1 ), ( 0, 1 ), ( 0, 0 ), ( 1, 0 ) ]
    , color = palette.green
    , pivot = { r = 0.0, c = 0.0 }
    }


t : Tetromino
t =
    { shape = [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ), ( -1, 1 ) ]
    , color = palette.purple
    , pivot = { r = 0.0, c = 1.0 }
    }


i : Tetromino
i =
    { shape = [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( -1, 0 ) ]
    , color = palette.lightblue
    , pivot = { r = 1.0, c = 0.0 }
    }


l : Tetromino
l =
    { shape = [ ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( 1, 0 ) ]
    , color = palette.blue
    , pivot = { r = 0.0, c = 1.0 }
    }


j : Tetromino
j =
    { shape = [ ( 1, 1 ), ( 0, 1 ), ( -1, 1 ), ( -1, 0 ) ]
    , color = palette.orange
    , pivot = { r = 0.0, c = 1.0 }
    }


o : Tetromino
o =
    { shape = [ ( 1, 0 ), ( 1, 1 ), ( 0, 1 ), ( 0, 0 ) ]
    , color = palette.yellow
    , pivot = { r = 0.5, c = 0.5 }
    }


emptyShape : Tetromino
emptyShape =
    { shape = []
    , color = "red"
    , pivot = { r = 0.0, c = 0.0 }
    }


allTetrominos : Array Tetromino
allTetrominos =
    Array.fromList [ i, j, l, s, z, o, t ]


size : Int
size =
    35


totalRows : Int
totalRows =
    20


totalCols : Int
totalCols =
    10


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
    { tetromino | shape = newShape }


shift : Pos -> Tetromino -> Tetromino
shift ( row, col ) tetromino =
    let
        shiftHelperPivot { r, c } =
            { r = r + toFloat row, c = c + toFloat col }

        shiftHelper ( x, y ) =
            ( x + row, y + col )

        shape_ =
            List.map shiftHelper tetromino.shape

        pivot_ =
            shiftHelperPivot tetromino.pivot
    in
    { tetromino | shape = shape_, pivot = pivot_ }


hardDropped : Tetromino -> List Block -> Tetromino
hardDropped tetromino board =
    let
        lower =
            tetromino |> shift ( 0, 1 )
    in
    if isValid lower board then
        hardDropped lower board

    else
        tetromino


findFirstFullRow : List Block -> Maybe Int
findFirstFullRow board =
    case board of
        [] ->
            Nothing

        block :: _ ->
            let
                lineY =
                    second block.pos

                ( curLine, others ) =
                    -- board |> List.partition (.pos >> second >> (==) lineY)
                    board |> List.partition (\{ pos } -> second pos == lineY)
            in
            if List.length curLine > 9 then
                Just lineY

            else
                findFirstFullRow others


isIntersecting : Tetromino -> List Block -> Bool
isIntersecting { shape } board =
    let
        checkLocation pos_ =
            board |> List.any (.pos >> (==) pos_)
    in
    List.any checkLocation shape


isInBounds : Tetromino -> Bool
isInBounds { shape } =
    let
        checkLocation ( c, r ) =
            c >= 0 && c < totalCols && r < totalRows
    in
    List.all checkLocation shape


isValid : Tetromino -> List Block -> Bool
isValid tetromino board =
    isInBounds tetromino && not (isIntersecting tetromino board)
