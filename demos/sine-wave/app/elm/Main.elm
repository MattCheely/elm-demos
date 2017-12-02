module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import AnimationFrame exposing (diffs)
import Window
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { frame : Int
    , barCount : Int
    , barWidth : Float
    , step : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 0 0 1, Task.perform WindowWidth Window.width )


type Msg
    = WindowWidth Int
    | Frame
    | ReverseDirection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowWidth width ->
            let
                barCount =
                    min 200 (width // 15)
            in
                ( { model
                    | barCount = barCount
                    , barWidth = 100 / toFloat barCount
                  }
                , Cmd.none
                )

        ReverseDirection ->
            ( { model | step = model.step * -1 }, Cmd.none )

        Frame ->
            ( { model | frame = model.frame + model.step }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs <| always Frame


barDiv : Model -> Int -> Html msg
barDiv model index =
    let
        frameFloat =
            toFloat model.frame

        indexFloat =
            toFloat index

        translateY =
            (sin (frameFloat / 10 + indexFloat / 5)) * 100 * 0.5

        hue =
            (360 // model.barCount * index - model.frame) % 360

        color =
            "hsl(" ++ toString hue ++ ", 95%, 55%)"

        width =
            toString model.barWidth ++ "%"

        barX =
            toString (model.barWidth * indexFloat) ++ "%"

        rotation =
            (model.frame + index) % 360

        transform =
            "scale(0.8,.5) translateY(" ++ toString translateY ++ "%) rotate(" ++ toString rotation ++ "deg)"
    in
        div
            [ class "bar"
            , style
                [ ( "backgroundColor", color )
                , ( "width", width )
                , ( "left", barX )
                , ( "transform", transform )
                ]
            ]
            []


view : Model -> Html Msg
view model =
    let
        divs =
            List.map (barDiv model) (List.range 0 (model.barCount - 1))
    in
        div [ onClick ReverseDirection ]
            [ div [ class "animated-sin-wave" ] <|
                divs
            , lazy description model.barCount
            ]


description : Int -> Html Msg
description barCount =
    p [ class "animated-sin-wave-description" ]
        [ text "The above animation is "
        , text (toString barCount)
        , text " "
        , code [] [ text "<div>" ]
        , text " tags."
        ]
