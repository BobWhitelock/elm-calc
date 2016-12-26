module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)


type alias Model =
    { partialSum : Maybe PartialSum
    , currentNumber : NumberUnderConstruction
    }


type alias PartialSum =
    { answer : Float
    , operator : Maybe Operator
    }


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide


type alias NumberUnderConstruction =
    { integerPart : List Int
    , fractionalPart : Maybe (List Int)
    }


init : String -> ( Model, Cmd Msg )
init path =
    ( { partialSum = Nothing
      , currentNumber = NumberUnderConstruction [] Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ result model ]
        , div
            []
            [ numberButton 7
            , numberButton 8
            , numberButton 9
            , operatorButton Divide
            ]
        , div
            []
            [ numberButton 4
            , numberButton 5
            , numberButton 6
            , operatorButton Multiply
            ]
        , div
            []
            [ numberButton 1
            , numberButton 2
            , numberButton 9
            , operatorButton Minus
            ]
        , div
            []
            [ numberButton 0
            , button [] [ text "." ]
            , button [] [ text "=" ]
            , operatorButton Plus
            ]
        ]


result : Model -> Html Msg
result model =
    span [] []


numberButton : Int -> Html Msg
numberButton num =
    button [] [ text (toString num) ]


operatorButton : Operator -> Html Msg
operatorButton operator =
    let
        symbol =
            case operator of
                Plus ->
                    "+"

                Minus ->
                    "−"

                Multiply ->
                    "×"

                Divide ->
                    "÷"
    in
        button [] [ text symbol ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
