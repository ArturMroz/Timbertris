module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html exposing (Html, a, button, div, h3, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (keyCode, onClick)
import Json.Decode as Decode
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)
import Task
import Time
import Tuple exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { active : Tetromino
    , ghost : Tetromino
    , next : Tetromino
    , pile : List Block
    , lines : Int
    , score : Int
    , level : Int
    }


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


moveFunction : (Tetromino -> Tetromino) -> Model -> ( Model, Cmd Msg )
moveFunction func model =
    let
        active_ =
            func model.active
    in
    if isValid active_ model.pile then
        ( { model
            | active = active_
            , ghost = hardDropped active_ model.pile
          }
        , Cmd.none
        )

    else
        ( model, Cmd.none )


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


clearLines : ( List Block, Int ) -> ( List Block, Int )
clearLines ( board, linesSoFar ) =
    case findFirstFullRow board of
        Nothing ->
            ( board, linesSoFar )

        Just row ->
            let
                clearedPile =
                    -- board |> List.filter (.pos >> second >> (/=) row)
                    board |> List.filter (\{ pos } -> second pos /= row)

                ( above, bellow ) =
                    -- clearedPile |> List.partition (.pos >> second >> (<) row)
                    clearedPile |> List.partition (\{ pos } -> second pos < row)

                shiftDown el =
                    { el | pos = ( first el.pos, second el.pos + 1 ) }

                shiftedAbove =
                    above |> List.map shiftDown

                clearedBellow =
                    bellow |> List.filter (\{ pos } -> second pos < totalRows)
            in
            ( clearedBellow ++ shiftedAbove, linesSoFar + 1 ) |> clearLines


scoreLines lines { level } =
    case lines of
        1 ->
            40 * level

        2 ->
            100 * level

        3 ->
            300 * level

        4 ->
            800 * level

        _ ->
            0


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



-- UPDATE


init : () -> ( Model, Cmd Msg )
init _ =
    ( { active = z |> shift ( 4, 30 )
      , ghost = z |> shift ( 4, 30 )
      , next = z
      , pile = []
      , lines = 0
      , score = 0
      , level = 1
      }
    , Cmd.none
    )


cmdRandomNewTetromino : Cmd Msg
cmdRandomNewTetromino =
    Random.generate (\ind -> MsgNewTetromino ind) (Random.int 0 6)


type Msg
    = Rotate
    | Shift ( Int, Int )
    | MsgNewTetromino Int
      -- | GetNewTetromino
    | Tick Time.Posix
    | FillRow Int
      -- | RemoveFullRow
    | HardDrop
    | NewGame
    | Nop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shift ( x, y ) ->
            moveFunction (shift ( x, y )) model

        Rotate ->
            moveFunction rotate model

        Tick newTime ->
            let
                active_ =
                    model.active |> shift ( 0, 1 )
            in
            if isValid active_ model.pile then
                ( { model | active = active_ }, Cmd.none )

            else
                ( model, cmdRandomNewTetromino )

        MsgNewTetromino ind ->
            let
                next_ =
                    Array.get ind allTetrominos
                        |> Maybe.withDefault z

                active_ =
                    model.next |> shift ( 4, 0 )

                -- o |> shift ( 4, 1 )
                -- Array.get ind allTetrominos
                --     |> Maybe.map (shift ( 4, 0 ))
                --     |> Maybe.withDefault z
                pileAdd tetromino =
                    tetromino.shape
                        |> List.map (\block -> { pos = block, color = tetromino.color })

                ( pile_, lines_ ) =
                    ( model.pile ++ pileAdd model.active, 0 ) |> clearLines

                level_ =
                    if model.lines + lines_ >= model.level * 10 then
                        model.level + 1

                    else
                        model.level

                score_ =
                    model.score + 4 + scoreLines lines_ model
            in
            if isValid active_ pile_ then
                ( { model
                    | pile = pile_
                    , score = score_
                    , active = active_
                    , next = next_
                    , ghost = hardDropped active_ pile_
                    , lines = model.lines + lines_
                    , level = level_
                  }
                , Cmd.none
                )

            else
                -- ( { model | pile = [] }, Cmd.none )
                update NewGame model

        -- ( model, NewGame )
        HardDrop ->
            ( { model | active = model.ghost }
            , cmdRandomNewTetromino
            )

        NewGame ->
            ( { model
                | pile = []
                , active = emptyShape
                , level = 1
                , score = 1
                , lines = 0
              }
            , cmdRandomNewTetromino
            )

        Nop ->
            ( model, Cmd.none )

        FillRow row ->
            let
                newRow y =
                    List.range 0 8
                        |> List.map (\n -> { pos = ( n, y ), color = "plum" })
            in
            ( { model | pile = newRow 19 ++ newRow 17 ++ newRow 18 ++ model.pile }, Cmd.none )


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

        32 ->
            HardDrop

        _ ->
            Nop


getTickDelta : Model -> Float
getTickDelta model =
    let
        delta =
            800 - toFloat (model.level * 50)
    in
    Basics.max 25 delta


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (getTickDelta model) Tick
        , onKeyDown (Decode.map (key True) keyCode)
        ]



-- VIEW


size : Int
size =
    35


totalRows : Int
totalRows =
    20


totalCols : Int
totalCols =
    10


boardWidth : String
boardWidth =
    String.fromInt (size * totalCols)


boardHeight =
    String.fromInt (size * totalRows)


renderBoard : Model -> Svg Msg
renderBoard { active, pile, ghost } =
    svg
        [ width boardWidth, height boardHeight ]
        ([ rect
            [ width boardWidth, height boardHeight, fill palette.darkgray ]
            []
         ]
            ++ toSvg active False
            ++ toSvg ghost True
            ++ toSvgPile pile
        )


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


nextPreview : Tetromino -> Svg Msg
nextPreview next =
    svg
        [ width (4 * size |> String.fromInt)
        , height (2 * size |> String.fromInt)

        -- , Svg.Attributes.style ("background: " ++ next.color)
        ]
        (toSvgNext next)


toSvgNext : Tetromino -> List (Svg msg)
toSvgNext tetromino =
    let
        tans location =
            String.fromInt (location * size)

        translate pos =
            rect
                [ width (String.fromInt size)
                , height (String.fromInt size)
                , x (tans (first pos))
                , y (tans (second pos))
                , fill tetromino.color

                -- , opacity "0.8"
                -- , stroke palette.darkgray
                -- , strokeWidth "2px"
                ]
                []
    in
    List.map translate tetromino.shape


toSvgPile : List Block -> List (Svg msg)
toSvgPile shape =
    let
        tans location =
            String.fromInt (location * size)

        translate { pos, color } =
            rect
                [ width (String.fromInt size)
                , height (String.fromInt size)
                , x (tans (first pos))
                , y (tans (second pos))
                , fill color

                -- , rx "5"
                -- , ry "5"
                -- , stroke color
                -- , strokeWidth "0.5px"
                -- , class "testy"
                -- , stroke palette.darkgray
                -- , strokeWidth "2px"
                ]
                []
    in
    List.map translate shape


toSvg : Tetromino -> Bool -> List (Svg msg)
toSvg { shape, color, pivot } ghost =
    let
        str location =
            -- String.fromInt (location * size + 2)
            String.fromInt (location * size) ++ "px"

        inset x =
            x - 4 |> String.fromInt

        common ( x_, y_ ) =
            [ width (String.fromInt size)
            , height (String.fromInt size)
            , x (str x_)
            , y (str y_)
            ]

        normal =
            [ fill color, strokeWidth "0.5" ]

        ghostProps =
            [ fill "transparent"
            , stroke color
            , strokeWidth "2px"

            -- , height (inset size)
            -- , width (inset size)
            ]

        -- strF location =
        --     String.fromFloat ((location + 0.5) * toFloat size)
        -- pivotPoint { r, c } =
        --     circle [ cx (strF r), cy (strF c), Svg.Attributes.r "3", fill "black" ] []
        props =
            if ghost then
                ghostProps

            else
                normal

        translate pos_ =
            rect (common pos_ ++ props) []
    in
    List.map translate shape


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ renderBoard model
        , div [ class "nav" ]
            [ button [ onClick Rotate ] [ text "↻" ]
            , button [ onClick (Shift ( -1, 0 )) ] [ text "↼" ]
            , button [ onClick (Shift ( 1, 0 )) ] [ text "⇁" ]
            , button [ onClick (Shift ( 0, 1 )) ] [ text "⇂" ]
            , button [ onClick HardDrop ] [ text "⇓" ]
            ]
        , div [ class "score" ]
            [ div [ class "next" ]
                [ h3 [] [ text "next" ]
                , nextPreview (model.next |> shift ( 1, 0 ))
                ]
            , div []
                [ h3 [] [ text "score" ]
                , span [] [ text (String.fromInt model.score) ]
                ]
            , div []
                [ h3 [] [ text "level" ]
                , span [] [ text (String.fromInt model.level) ]
                , renderProgressBar model
                ]
            , button [ onClick NewGame ] [ text "new game" ]
            ]
        , div [ class "info" ]
            [ a
                [ href "https://github.com/ArturMroz/Timbertris", target "blank_" ]
                [ h3 [] [ text "timbertris 🍂" ]
                , span [] [ text "— by artur" ]
                ]
            ]
        ]


renderProgressBar : Model -> Html Msg
renderProgressBar model =
    let
        percent =
            (model.lines - (model.level - 1) * 10) * 10

        color =
            if percent < 25 then
                palette.orange

            else if percent < 66 then
                palette.yellow

            else
                palette.green
    in
    div [ class "level" ]
        [ div
            [ class "level-bar"
            , style "width" (String.fromInt percent ++ "%")
            , style "background" color
            ]
            []
        ]



-- TODO
-- ** CRITICAL **
-- new game
-- pause
-- score harddrop
-- score back-to-back
-- game over
-- framediff sub
-- rotate by the edge
-- ** NICE TO HAVE **
-- animate full row
-- animate drop after full row clear
-- animate hard drop
-- level up animation
-- sfx
