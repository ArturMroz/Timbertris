module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes
import Html.Events exposing (keyCode, onClick, onInput)
import Json.Decode as Decode
import Random
import Set
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time
import Tuple exposing (..)



-- import Tuple exposing (Tuple)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


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

    -- , darkgray = "#3b4252"
    , darkgray = "#2e3440"
    , lightgray = "#d8dee9"
    }


palette : Palette
palette =
    nord


size_ : Int
size_ =
    30


size =
    String.fromInt size_


boardWidth =
    String.fromInt <| (size_ * 10)


boardHeight =
    String.fromInt <| (size_ * 20)


board : Model -> Svg Msg
board model =
    svg
        [ width boardWidth
        , height boardHeight
        ]
        ([ rect
            [ width boardWidth
            , height boardHeight
            , fill palette.darkgray
            ]
            []
         ]
            ++ toForm model.active
            ++ toForm_ model.pile
        )


type alias Pos =
    ( Int, Int )


type alias Tetromino =
    { shape : List Pos
    , color : String
    , pivot : { r : Float, c : Float }
    }


z : Tetromino
z =
    { shape =
        [ ( 1, -1 )
        , ( 1, 0 )
        , ( 0, 0 )
        , ( 0, 1 )
        ]
    , color = palette.red
    , pivot = { r = 0.0, c = 0.0 }
    }


s : Tetromino
s =
    { shape =
        [ ( -1, -1 )
        , ( -1, 0 )
        , ( 0, 0 )
        , ( 0, 1 )
        ]
    , color = palette.green
    , pivot = { r = 0.0, c = 0.0 }
    }


t : Tetromino
t =
    { shape =
        [ ( 0, -1 )
        , ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 0 )
        ]
    , color = palette.purple
    , pivot = { r = 0.0, c = 0.0 }
    }


i : Tetromino
i =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -2, 0 )
        ]
    , color = palette.lightblue
    , pivot = { r = -0.5, c = 0.5 }
    }


l : Tetromino
l =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, 1 )
        ]
    , color = palette.blue
    , pivot = { r = 0.0, c = 0.0 }
    }


j : Tetromino
j =
    { shape =
        [ ( 1, 0 )
        , ( 0, 0 )
        , ( -1, 0 )
        , ( -1, -1 )
        ]
    , color = palette.orange
    , pivot = { r = 0.0, c = 0.0 }
    }


o : Tetromino
o =
    { shape =
        [ ( 0, 0 )
        , ( 0, 1 )
        , ( -1, 1 )
        , ( -1, 0 )
        ]
    , color = palette.yellow
    , pivot = { r = -0.5, c = 0.5 }
    }


allTetrominos : Array Tetromino
allTetrominos =
    Array.fromList [ i, j, l, s, z, o, t ]


type alias Model =
    { active : Tetromino
    , pile : List Block
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = z |> shift ( 4, 30 )
      , pile = []
      }
    , Cmd.none
    )


toForm_ : List Block -> List (Svg msg)
toForm_ shape =
    let
        tans location =
            String.fromInt (location * size_)

        translate { pos, color } =
            rect
                [ width size
                , height size
                , x (tans (first pos))
                , y (tans (second pos))
                , fill color
                ]
                []
    in
    List.map translate shape


toForm : Tetromino -> List (Svg msg)
toForm { shape, color } =
    let
        basicB =
            [ width size, height size, fill color ]

        pos location =
            String.fromInt (location * size_)

        animateEl =
            animate
                [ attributeName "fill"
                , from "red"
                , to "blue"
                , dur "15s"
                ]
                []

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

        tetromino_ =
            { tetromino | shape = shape_, pivot = pivot_ }
    in
    if isInBounds tetromino_ then
        tetromino_

    else
        tetromino_



-- UPDATE


type Msg
    = Rotate
    | Shift ( Int, Int )
    | MsgNewTetromino Int
    | GetNewTetromino
    | Tick Time.Posix
    | FillRow Int
    | RemoveFullRow
      -- | HardDrop
    | Nop


cmdRandomNewTetromino : Cmd Msg
cmdRandomNewTetromino =
    Random.generate (\ind -> MsgNewTetromino ind) (Random.int 0 6)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shift ( x, y ) ->
            let
                active_ =
                    model.active |> shift ( x, y )
            in
            if isValid active_ model.pile then
                ( { model | active = active_ }, Cmd.none )

            else
                ( model, Cmd.none )

        Rotate ->
            let
                active_ =
                    rotate model.active
            in
            if isValid active_ model.pile then
                ( { model | active = active_ }, Cmd.none )

            else
                ( model, Cmd.none )

        GetNewTetromino ->
            ( model
            , cmdRandomNewTetromino
            )

        Tick newTime ->
            let
                newOne =
                    model.active |> shift ( 0, 1 )

                lowestPoint =
                    newOne.shape |> List.map second |> List.maximum
            in
            if isValid newOne model.pile |> not then
                ( model, cmdRandomNewTetromino )

            else
                ( { model | active = newOne }
                , Cmd.none
                )

        MsgNewTetromino ind ->
            let
                newTetromino =
                    -- i |> shift ( 4, 0 )
                    Array.get ind allTetrominos
                        |> Maybe.map (shift ( 4, 2 ))
                        |> Maybe.withDefault z

                pileAdd tetromino =
                    tetromino.shape
                        |> List.map (\block -> { pos = block, color = tetromino.color })
            in
            ( { model
                | pile = pileAdd model.active ++ model.pile |> clearLines
                , active = newTetromino
              }
            , Cmd.none
            )

        -- HardDrop ->
        --     let
        --         hardDropped =
        --             model.active
        --     in
        --     ( { model | pile = model.pile ++ hardDropped }, cmdRandomNewTetromino )
        Nop ->
            ( model, Cmd.none )

        FillRow row ->
            let
                newRow y =
                    List.range 0 8
                        |> List.map (\n -> { pos = ( n, y ), color = "plum" })
            in
            ( { model | pile = newRow 19 ++ newRow 17 ++ newRow 18 ++ model.pile }, Cmd.none )

        RemoveFullRow ->
            ( { model | pile = model.pile |> clearLines |> clearGridBellowViewport }
            , Cmd.none
            )


findFirstFullRow : List Block -> Maybe Int
findFirstFullRow board_ =
    case board_ of
        [] ->
            Nothing

        x :: _ ->
            let
                lineY =
                    second x.pos

                ( curLine, others ) =
                    -- board_ |> List.partition (\{ pos } -> second pos == lineY)
                    board_ |> List.partition (.pos >> second >> (==) lineY)

                curLineSet =
                    curLine
                        |> List.map (.pos >> first)
                        |> Set.fromList
            in
            if Set.size curLineSet > 9 then
                Just lineY

            else
                findFirstFullRow others


clearLines : List Block -> List Block
clearLines board_ =
    case findFirstFullRow board_ of
        Nothing ->
            board_

        Just row ->
            let
                clearedPile =
                    board_ |> List.filter (\{ pos } -> second pos /= row)

                -- board_ |> List.filter (.pos >> second >> (/=) row)
                ( above, bellow ) =
                    clearedPile |> List.partition (\{ pos } -> second pos < row)

                -- clearedPile |> List.partition (.pos >> second >> (<) row)
                shiftDown el =
                    { el | pos = ( first el.pos, second el.pos + 1 ) }

                shiftedAbove =
                    above |> List.map shiftDown

                clearedBellow =
                    bellow |> List.filter (\{ pos } -> second pos < 20)
            in
            clearedBellow ++ shiftedAbove |> clearLines


isValid : Tetromino -> List Block -> Bool
isValid tetromino board_ =
    isInBounds tetromino && not (isIntersecting tetromino board_)


isIntersecting : Tetromino -> List Block -> Bool
isIntersecting { shape } board_ =
    let
        checkLocation pos_ =
            board_
                |> List.map .pos
                -- |> List.any (\b -> first b.pos == y_ && second b.pos == x_)
                |> List.member pos_
    in
    List.any checkLocation shape


isInBounds : Tetromino -> Bool
isInBounds { shape } =
    let
        checkLocation ( c, r ) =
            r >= 0 && r < 20 && c >= 0 && c < 10
    in
    List.all checkLocation shape


clearGridBellowViewport : List Block -> List Block
clearGridBellowViewport grid =
    -- grid
    -- grid |> List.filter (.pos >> second >> (<) 100)
    grid |> List.filter (\{ pos } -> second pos < 20)


key : Bool -> Int -> Msg
key on keycode =
    case keycode of
        37 ->
            Shift ( -1, 0 )

        39 ->
            Shift ( 1, 0 )

        40 ->
            Shift ( 0, 1 )

        38 ->
            Rotate

        _ ->
            Nop


type alias Block =
    { pos : Pos
    , color : String
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick
        , onKeyDown (Decode.map (key True) keyCode)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ board model
        , div []
            [ button [ onClick (Shift ( 0, -1 )) ] [ Html.text "up" ]
            , button [ onClick (Shift ( -1, 0 )) ] [ Html.text "left" ]
            , button [ onClick (Shift ( 1, 0 )) ] [ Html.text "right" ]
            , button [ onClick (Shift ( 1, 0 )) ] [ Html.text "right" ]
            , button [ onClick (Shift ( 0, 1 )) ] [ Html.text "down" ]
            , button [ onClick Rotate ] [ Html.text "rotate" ]
            , button [ onClick GetNewTetromino ] [ Html.text "new" ]
            , button [ onClick (FillRow 2) ] [ Html.text "row" ]
            , button [ onClick RemoveFullRow ] [ Html.text "rem row" ]
            , debugDisplay model.active.shape
            , model.pile |> List.map .pos |> debugDisplay
            ]
        ]


debugDisplay el =
    Html.p [] [ Html.text (Debug.toString el) ]
