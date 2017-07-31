module RasterShapes
    exposing
        ( Position
        , Size
        , line
        , bezier
        , rectangle
        , rectangle2
        , ellipse
        , circle
        )

{-| 

# Basics

@docs Position, Size

# Shape calculators
@docs line, bezier, rectangle, rectangle2, ellipse, circle
-}




{-| `Position` is our basic type for representing the position in a grid. -}
type alias Position =
    { x : Int
    , y : Int 
    }


{-| We use `Size` to store width and height value.
-}
type alias Size =
    { width : Int
    , height : Int
    }


{-| To get a list of `Position` along the edge of a circle of diameter `Int`, use `circle`.

    dot : Position -> List Position
    dot =
        circle 1

-}
circle : Int -> Position -> List Position
circle diameter =
    ellipse (Size diameter diameter)



{-| To get a list of `Position` along the edge of a ellipse of dimensions `Size`, use `ellipse`.

    circle : Int -> Position -> List Position
    circle diameter point =
        ellipse (Size diameter diameter) point
-}
ellipse : Size -> Position -> List Position
ellipse ({ width, height } as size) { x, y } =
    let
        position =
            Position
                (x + width)
                (y + height)

        firstHalf =
            ellipseLoopFirst
                position
                (Position 0 height )
                (2 * (width ^ 2) + (height ^ 2) * (1 - 2 * height))
                size
                []

        secondHalf =
            ellipseLoopSecond
                position
                (Position width 0 )
                (2 * (width ^ 2) + (height ^ 2) * (1 - 2 * width))
                size
                []
    in
        List.append firstHalf secondHalf



ellipseLoopFirst : Position -> Position -> Int -> Size -> List Position -> List Position
ellipseLoopFirst c { x, y } sigma { width, height } positions =
    if x * (height ^ 2) <= y * (width ^ 2) then
        let
            ( dy, dSigma ) =
                if sigma >= 0 then
                    ( 1, (4 * (width ^ 2)) * (1 - y) )
                else
                    ( 0, 0 )
        in
            ellipseLoopFirst
                c
                ( Position (x + 1) (y - dy) )
                (sigma + dSigma + ((height ^ 2) * ((4 * x) + 6)))
                (Size width height)
                (addPoints c.x c.y x y positions)
    else
        positions


ellipseLoopSecond : Position -> Position -> Int -> Size -> List Position -> List Position
ellipseLoopSecond c { x, y } sigma { width, height } positions =
    if y * (width ^ 2) <= x * (height ^ 2) then
        let
            ( dx, dSigma ) =
                if sigma >= 0 then
                    ( 1, (4 * (height ^ 2)) * (1 - x) )
                else
                    ( 0, 0 )
        in
            ellipseLoopSecond
                c
                ( Position (x - dx) (y + 1) )
                (sigma + dSigma + ((width ^ 2) * ((4 * y) + 6)))
                (Size width height)
                (addPoints c.x c.y x y positions)
    else
        positions


addPoints : Int -> Int -> Int -> Int -> List Position -> List Position
addPoints cx cy x y points =
    List.append
        points
        [ Position (cx + x) (cy + y )
        , Position (cx - x) (cy + y )
        , Position (cx + x) (cy - y )
        , Position (cx - x) (cy - y )
        ]


{-| To make a curved line, try this function called `bezier`, named after [the bezier curve](https://en.wikipedia.org/wiki/B%C3%A9zier_curve). It works by approximation, drawing many small straight lines along a curved path. Its first parameter, an `Int`, is the resolution of the curve (resolution=1 will be just a straight line, and higher values will compute a more perfect curve). The remaining parameters are `Position`. The first and last `Position` refer to the starting and ending points of the curve. The middle two are control points, which are where the curve will curve towards from each end point.

    squiggle : Int -> List Position
    squiggle i =
        Raster (i // 2)
            (Position 0 0)
            (Position i 0)
            (Position 0 i)
            (Position i i)

-}
bezier : Int -> Position -> Position -> Position -> Position -> List Position
bezier resolution p0 p1 p2 p3 =
    let
        positions =
            bezierLoop
                resolution
                0
                (toFloat p0.x, toFloat p0.y)
                (toFloat p1.x, toFloat p1.y)
                (toFloat p2.x, toFloat p2.y)
                (toFloat p3.x, toFloat p3.y)
                []
    in
        List.map2 (,) positions (List.drop 1 positions)
            |> List.map applyLine
            |> List.concat


applyLine : ( Position, Position ) -> List Position
applyLine ( p0, p1 ) =
    line p0 p1


bezierLoop : Int -> Int -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> List Position -> List Position
bezierLoop seg i p0 p1 p2 p3 positions =
    let
        positions_ =
            (calcBezierPoint seg i p0 p1 p2 p3) :: positions
    in
        if i < seg then
            bezierLoop seg (i + 1) p0 p1 p2 p3 positions_
        else
            positions_


calcBezierPoint : Int -> Int -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Position
calcBezierPoint seg i ( p0x, p0y ) ( p1x, p1y ) ( p2x, p2y ) ( p3x, p3y ) =
    let
        ( a, b, c, d ) =
            calcBezier seg i
    in
        Position
            (floor (a * p0x + b * p1x + c * p2x + d * p3x))
            (floor (a * p0y + b * p1y + c * p2y + d * p3y))


calcBezier : Int -> Int -> ( Float, Float, Float, Float )
calcBezier seg i =
    let
        a =
            (toFloat i) / (toFloat seg)

        b =
            1 - a
    in
        ( b ^ 3
        , 3 * b ^ 2 * a
        , 3 * a ^ 2 * b
        , a ^ 3
        )



-- RECTANGLE --



{-| To get a list of `Position` along the edge of a rectangle, use `rectangle`.

    rectangle (Size 10 7) (Position 1 1) ==
        [ line (Position 1 1) (Position 10 1)
        , line (Position 1 1) (Position 1 7)
        , line (Position 11 8) (Position 1 8)
        , line (Position 11 8) (Position 11 1)
        ]
-}

rectangle : Size -> Position -> List Position
rectangle { width, height } p =
    case (width, height) of
        (0, 0) ->
            []

        _ ->
            rectangle2
                p
                (Position (p.x + width) (p.y + height))


{-| `Rectangle2` is just like `Rectangle`, except it takes two positions as parameters rather than a position and a size
-}
rectangle2 : Position -> Position -> List Position
rectangle2 p q =
    let
        (xLeft, xRight) =
            if p.x == (min p.x q.x) then
                (p.x, q.x)
            else
                (q.x, p.x)

        (yTop, yBottom) =
            if p.y == (min p.y q.y) then
                (p.y, q.y)
            else
                (q.y, p.y)
    in 
        case (xLeft - xRight, yTop - yBottom) of
            (0, 0) ->
                [ p ]

            (1, 1) ->
                [ Position xLeft yTop
                , Position xLeft yBottom
                , Position xRight yTop
                , Position xRight yBottom
                ]

            (1, _) ->
                line 
                    (Position xLeft yTop) 
                    (Position xLeft yBottom)


            (_ , 1) ->
                line 
                    (Position xLeft yTop) 
                    (Position xRight yTop)


            (width, height) ->
                [ line 
                    (Position xLeft yTop) 
                    (Position (xRight - 1) yTop)
                , line 
                    (Position xRight yTop) 
                    (Position xRight (yBottom - 1))
                , line 
                    (Position xRight yBottom) 
                    (Position (xLeft + 1) (yBottom))
                , line 
                    (Position xLeft yBottom) 
                    (Position xLeft (yTop + 1))
                ]
                    |> List.concat



-- LINE --



type alias LineStatics =
    { fx : Int
    , fy : Int
    , sx : Int
    , sy : Int
    , dx : Float
    , dy : Float
    }

{-| Given a starting and ending `Position`, this function will give you every `Position` along that line.

    (line (Position 0 0) (Position 3 2)) ==
        [ Position 3 2
        , Position 2 1
        , Position 1 1
        , Position 0 0
        ]
-}
line : Position -> Position -> List Position
line p0 p1 =
    let
        ( statics, error ) =
            initLine p1 p0
    in
        lineLoop statics error p0 []



lineLoop : LineStatics -> Float -> Position -> List Position -> List Position
lineLoop statics error ({ x, y } as p) positions =
    if (x == statics.fx) && (y == statics.fy) then
        p :: positions
    else
        let
            ( error_, q ) =
                calcError statics error p
        in
            lineLoop statics error_ q (p :: positions)


calcError : LineStatics -> Float -> Position -> ( Float, Position )
calcError { sx, sy, dx, dy } error p =
    let
        ( errX, x ) =
            if error > -dx then
                ( -dy, sx + p.x )
            else
                ( 0, p.x )

        ( errY, y ) =
            if error < dy then
                ( dx, sy + p.y )
            else
                ( 0, p.y )
    in
        ( error + errX + errY, Position x y )



initLine : Position -> Position -> ( LineStatics, Float )
initLine p q =
    let
        dx =
            (toFloat << abs) (p.x - q.x)

        dy =
            (toFloat << abs) (p.y - q.y)

        sx =
            if p.x > q.x then
                1
            else
                -1

        sy =
            if p.y > q.y then
                1
            else
                -1

        error =
            if dx > dy then
                dx / 2
            else
                -dy / 2
    in
        ( LineStatics p.x p.y sx sy dx dy, error )