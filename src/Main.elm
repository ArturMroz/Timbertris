module Main exposing (cmdRandomNewTetromino, init, key, main, moveFunction, subscriptions, update)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Html.Events exposing (keyCode, onClick)
import Json.Decode as Decode
import Messages exposing (..)
import Model exposing (..)
import Random
import Task
import Time
import View exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = View.view
        }



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

      --   , state = Playing
      }
    , Cmd.none
    )


cmdRandomNewTetromino : Cmd Msg
cmdRandomNewTetromino =
    Random.generate (\ind -> MsgNewTetromino ind) (Random.int 0 6)


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
                , score = 0
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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        delta =
            Basics.max 25 (800 - toFloat (model.level * 50))
    in
    Sub.batch
        [ Time.every delta Tick
        , onKeyDown (Decode.map (key True) keyCode)
        ]
