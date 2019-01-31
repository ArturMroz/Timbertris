module View exposing (boardHeight, boardWidth, nextPreview, renderBoard, renderProgressBar, toSvg, toSvgNext, toSvgPile, view)

-- import Random

import Html exposing (Html, a, button, div, h3, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (keyCode, onClick)
import Json.Decode as Decode
import Messages exposing (..)
import Model exposing (..)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)
import Tuple exposing (..)


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

                -- , span [] [ text "— by artur" ]
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
-- game over
-- score harddrop
-- score back-to-back
-- framediff sub
-- rotate by the edge
-- ** NICE TO HAVE **
-- animate full row
-- animate drop after full row clear
-- animate hard drop
-- level up animation
-- sfx
