module View exposing
    ( boardHeight
    , boardWidth
    , nextPreview
    , renderBoard
    , renderProgressBar
    , toSvg
    , toSvgNext
    , toSvgPile
    , view
    )

import Html exposing (Html, a, button, div, h3, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Model exposing (Block, Model, Tetromino, palette, size, totalCols, totalRows)
import Svg exposing (Svg, animate, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)
import Tuple exposing (first, second)


boardWidth : String
boardWidth =
    String.fromInt (size * totalCols)


boardHeight : String
boardHeight =
    String.fromInt (size * totalRows)


renderBoard : Model -> Svg Msg
renderBoard { active, pile, ghost } =
    svg
        [ width boardWidth, height boardHeight ]
        (rect
            [ width boardWidth
            , height boardHeight
            , fill palette.darkgray
            ]
            []
            :: toSvg ghost True
            ++ toSvgPile pile
            ++ toSvg active False
        )


nextPreview : Tetromino -> Svg Msg
nextPreview next =
    svg
        [ width (4 * size |> String.fromInt)
        , height (2 * size |> String.fromInt)

        -- , Svg.Attributes.style ("background: " ++ next.color)
        ]
        (toSvgNext next)



-- translate pos color =
--     rect
--         [ width (String.fromInt size)
--         , height (String.fromInt size)
--         , x (tans (first pos))
--         , y (tans (second pos))
--         , fill tetromino.color
--         -- , opacity "0.8"
--         -- , stroke palette.darkgray
--         -- , strokeWidth "2px"
--         ]
--         []


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
                , stroke "white"
                , strokeWidth "2"

                -- , opacity "0.8"
                -- , stroke palette.darkgray
                -- , strokeWidth "2px"
                ]
                [ Svg.animate [] [] ]
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
                , stroke "white"
                , strokeWidth "2px"

                -- , class "testy"
                -- , stroke palette.darkgray
                -- , strokeWidth "2px"
                ]
                []
    in
    List.map translate shape


toSvg : Tetromino -> Bool -> List (Svg msg)
toSvg { shape, color } ghost =
    let
        str location =
            String.fromInt (location * size) ++ "px"

        strGhost location =
            String.fromInt ((location * size) + 2) ++ "px"

        inset x =
            x - 4 |> String.fromInt

        common =
            [ width (String.fromInt size)
            , height (String.fromInt size)
            ]

        normal ( x_, y_ ) =
            [ fill color
            , strokeWidth "2"
            , stroke "white"
            , x (str x_)
            , y (str y_)
            ]

        ghostProps ( x_, y_ ) =
            [ fill "transparent"
            , stroke color
            , strokeWidth "2px"
            , height (inset size)
            , width (inset size)
            , x (strGhost x_)
            , y (strGhost y_)
            ]

        props pos =
            if ghost then
                ghostProps pos

            else
                normal pos

        translate pos_ =
            rect (common ++ props pos_) []
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

            -- , button [ onClick (Shift ( 0, 1 )) ] [ text "⇂" ] →
            -- , button [ onClick HardDrop ] [ text "⇓" ]
            , button [ onClick HardDrop ] [ text "⇂" ]
            ]
        , div [ class "score" ]
            [ div [ class "next" ]
                [ h3 [] [ text "next" ]
                , nextPreview model.next
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
                [ h3 [] [ text "timbertris 🍂" ] ]
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
