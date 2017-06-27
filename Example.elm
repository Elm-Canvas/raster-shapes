module Main exposing (..)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (style)
import RasterShapes as Raster exposing (Size, Position)


main : Html a
main =
    view
        [ rectangle
        , ellipse
        , circle
        , line
        , bezier
        ]


-- VIEW --


view : List (List Position) -> Html a
view =
    List.concat >> List.map pixel >> div []


pixel : Position -> Html a
pixel { x, y } =
    div
        [ style
            [ ("background", "#000000")
            , ("width", "10px")
            , ("height", "10px")
            , ("top", px (y * 10))
            , ("left", px (x * 10))
            , ("position", "absolute")
            ]
        ]
        []


px : Int -> String
px i =
    (toString i) ++ "px"


-- SHAPES --


circle : List Position
circle =
    Raster.circle
        5
        (Position 20 5)


ellipse : List Position
ellipse =
    Raster.ellipse
        (Size 10 14)
        (Position 16 2)


rectangle : List Position
rectangle =
    Raster.rectangle
        (Size 10 10)
        (Position 2 2)


line : List Position
line =
    Raster.line
        (Position 1 23)
        (Position 13 14)


bezier : List Position
bezier =
    Raster.bezier
        20
        (Position 1 25)
        (Position 20 25)
        (Position 1 35)
        (Position 35 35)