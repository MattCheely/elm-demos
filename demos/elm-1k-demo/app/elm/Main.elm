module Main exposing (main)

import Html exposing (Html, text, div, button, a, input)
import Html.Attributes exposing (class, id, href, type_, defaultValue)
import Html.Events exposing (onInput)
import Svg exposing (Svg, svg, circle)
import Svg.Attributes exposing (transform, fill, r)
import Svg.Keyed as Keyed
import Array exposing (Array)
import AnimationFrame
import Time exposing (Time)
import Window
import Task
import Colors


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { points : List Point
    , step : Int
    , layoutIdx : Int
    , count : Int
    , size : Window.Size
    }


type alias Point =
    { id : String
    , x : Float
    , y : Float
    , gx : Float
    , gy : Float
    , wx : Float
    , wy : Float
    , sx : Float
    , sy : Float
    , px : Float
    , py : Float
    , color : String
    }


type Layout
    = Phyllotaxis
    | Grid
    | Wave
    | Spiral


layoutOrder : Array Layout
layoutOrder =
    Array.fromList [ Phyllotaxis, Spiral, Phyllotaxis, Grid, Wave ]


layoutsLength : Int
layoutsLength =
    Array.length layoutOrder


init : ( Model, Cmd Msg )
init =
    ( { points = []
      , step = 0
      , layoutIdx = 0
      , count = 1000
      , size = { width = 800, height = 800 }
      }
    , Task.perform WindowSize Window.size
    )


buildPoints : Window.Size -> Int -> List Point
buildPoints winSize count =
    (List.range 0 (count - 1))
        |> List.map
            (\idx ->
                let
                    ( gx, gy ) =
                        project winSize (grid count idx)

                    ( wx, wy ) =
                        project winSize (wave count idx)

                    ( sx, sy ) =
                        project winSize (spiral count idx)

                    ( px, py ) =
                        project winSize (phyllotaxis count idx)
                in
                    { id = (toString idx)
                    , x = 0
                    , y = 0
                    , gx = gx
                    , gy = gy
                    , wx = wx
                    , wy = wy
                    , sx = sx
                    , sy = sy
                    , px = px
                    , py = py
                    , color = Colors.interpolate ((toFloat idx) / (toFloat count))
                    }
            )


targetPoints : Layout -> Point -> ( Float, Float )
targetPoints layout point =
    case layout of
        Phyllotaxis ->
            ( point.px, point.py )

        Grid ->
            ( point.gx, point.gy )

        Wave ->
            ( point.wx, point.wy )

        Spiral ->
            ( point.sx, point.sy )


project : Window.Size -> ( Float, Float ) -> ( Float, Float )
project size vector =
    let
        wh =
            (toFloat size.height) / 2

        ww =
            (toFloat size.width) / 2
    in
        translate ( ww, wh ) (scale (min wh ww) vector)


translate : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
translate ( tx, ty ) ( x, y ) =
    ( x + tx, y + ty )


scale : Float -> ( Float, Float ) -> ( Float, Float )
scale magnitude ( x, y ) =
    ( x * magnitude, y * magnitude )



-- Update


type Msg
    = Frame Time
    | WindowSize Window.Size
    | SetCount String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize size ->
            ( { model
                | size = size
                , points = buildPoints size model.count
              }
            , Cmd.none
            )

        SetCount count ->
            let
                newCount =
                    String.toInt count
                        |> Result.withDefault 1000
            in
                ( { model
                    | count = newCount
                    , points = buildPoints model.size newCount
                  }
                , Cmd.none
                )

        Frame _ ->
            let
                step =
                    (model.step + 1) % numSteps

                layoutIdx =
                    if (step == 0) then
                        (model.layoutIdx + 1) % layoutsLength
                    else
                        model.layoutIdx

                layout =
                    Array.get layoutIdx layoutOrder
                        |> Maybe.withDefault Grid

                nextLayout =
                    Array.get ((layoutIdx + 1) % layoutsLength) layoutOrder
                        |> Maybe.withDefault Grid

                pct =
                    min 1 ((toFloat step) / ((toFloat numSteps) * 0.8))
            in
                ( { model
                    | layoutIdx = layoutIdx
                    , step = step
                    , points =
                        model.points
                            |> List.map (updatePoint layout nextLayout pct)
                  }
                , Cmd.none
                )


updatePoint : Layout -> Layout -> Float -> Point -> Point
updatePoint prevLayout nextLayout pct point =
    let
        prevPoint =
            targetPoints prevLayout point

        nextPoint =
            targetPoints nextLayout point

        ( x, y ) =
            lerp pct prevPoint nextPoint
    in
        { point | x = x, y = y }


lerp : Float -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
lerp pct ( sx, sy ) ( ex, ey ) =
    ( sx + (ex - sx) * pct
    , sy + (ey - sy) * pct
    )


numSteps : Int
numSteps =
    60 * 2


theta : Float
theta =
    pi * (3 - (sqrt 5))


phyllotaxis : Int -> Int -> ( Float, Float )
phyllotaxis n i =
    let
        iFloat =
            (toFloat i)

        nFloat =
            (toFloat n)

        r =
            sqrt (iFloat / nFloat)

        th =
            iFloat * theta
    in
        ( r * (cos th), r * (sin th) )


grid : Int -> Int -> ( Float, Float )
grid n i =
    let
        rowLength =
            round (sqrt (toFloat n))

        rowLenFloat =
            toFloat rowLength
    in
        ( -0.8 + 1.6 / rowLenFloat * (toFloat (i % rowLength))
        , -0.8 + 1.6 / rowLenFloat * (toFloat (floor ((toFloat i) / rowLenFloat)))
        )


wave : Int -> Int -> ( Float, Float )
wave n i =
    let
        xScale =
            2 / ((toFloat n) - 1)

        x =
            -1 + (toFloat i) * xScale
    in
        ( x
        , (sin (x * pi * 3)) * 0.3
        )


spiral : Int -> Int -> ( Float, Float )
spiral n i =
    let
        t =
            sqrt ((toFloat i) / ((toFloat n) - 1))
    in
        ( t * (cos (t * pi * 10))
        , t * (sin (t * pi * 10))
        )



-- View


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "app-wrapper" ]
            [ svg [ Svg.Attributes.class "demo" ]
                [ Keyed.node "g"
                    []
                    (List.map (vizPoint) model.points)
                ]
            , div [ class "controls" ]
                [ text "# Points "
                , input
                    [ type_ "range"
                    , Html.Attributes.min "10"
                    , Html.Attributes.max "10000"
                    , defaultValue (toString model.count)
                    , onInput SetCount
                    ]
                    []
                , text " "
                , text (toString model.count)
                ]
            , div [ class "about" ]
                [ text "Demo port by "
                , a [ href "#" ] [ text "Matt Cheely" ]
                , text ", based on "
                , a [ href "https://dingoeatingfuzz.github.io/glimmer-1k/" ] [ text "Glimmmer-1k+" ]
                , text " by "
                , a [ href "http://mlange.io" ] [ text "Michael Lange" ]
                ]
            ]
        ]


vizPoint : Point -> ( String, Svg Msg )
vizPoint point =
    ( point.id
    , circle
        [ Svg.Attributes.class "point"
        , r "4"
        , transform ("translate(" ++ (toString point.x) ++ " " ++ (toString point.y) ++ ")")
        , fill point.color
        ]
        []
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Frame
