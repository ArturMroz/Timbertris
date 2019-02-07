module Main exposing (cmdRandomNewTetromino, init, key, main, moveFunction, subscriptions, update)

import Array
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Messages exposing (Msg(..))
import Model exposing (Model, Tetromino, allTetrominos, clearLines, emptyShape, hardDropped, isValid, rotate, scoreLines, shift, z)
import Random
import Time
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
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
        Shift pos ->
            moveFunction (shift pos) model

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
                        |> shift ( 1, 0 )

                active_ =
                    model.next |> shift ( 4, 0 )

                -- o |> shift ( 4, 1 )
                -- Array.get ind allTetrominos
                --     |> Maybe.map (shift ( 4, 0 ))
                --     |> Maybe.withDefault z
                pileAdd tetromino =
                    tetromino.shape
                        |> List.map (\pos_ -> { pos = pos_, color = tetromino.color })

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
                update NewGame model

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



-- type Direction
--     = Left
--     | Right
--     | Up
--     | Down
--     | Space
--     | Other
-- keyDecoder : Decode.Decoder Direction
-- keyDecoder =
--     Decode.map toDirection (Decode.field "key" Decode.string)
-- toDirection : String -> Direction
-- toDirection string =
--     case string of
--         "ArrowLeft" ->
--             Left
--         "ArrowRight" ->
--             Right
--         _ ->
--             Other


key : Int -> Msg
key keycode =
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
            Basics.max 25 (800 - toFloat (model.level * 75))
    in
    Sub.batch
        [ Time.every delta Tick
        , onKeyDown (Decode.map key keyCode)
        ]
