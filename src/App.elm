module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { numberOperatorPairs : List NumberOperatorPair
    , currentNumber : Decimal
    }


type alias NumberOperatorPair =
    { number : Decimal
    , operator : Operator
    }


type alias Decimal =
    { integerPart : List Int
    , fractionalPart : Maybe (List Int)
    }


type Operator
    = Plus
    | Minus
    | Multiply
    | Divide


init : String -> ( Model, Cmd Msg )
init path =
    ( { numberOperatorPairs = []
      , currentNumber = newDecimal
      }
    , Cmd.none
    )


newDecimal : Decimal
newDecimal =
    Decimal [] Nothing


type Msg
    = Operator Operator
    | Digit Int
    | DecimalPoint
    | Equals


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Operator operator ->
            handleOperator operator model

        Digit digit ->
            handleDigit digit model

        DecimalPoint ->
            handleDecimalPoint model

        Equals ->
            ( model, Cmd.none )


handleOperator : Operator -> Model -> ( Model, Cmd Msg )
handleOperator operator model =
    let
        newModel =
            if model.currentNumber == newDecimal then
                -- If we've not started entering any of the current number yet,
                -- just replace the last operator.
                replaceLastOperator operator model
            else
                pushNewPair operator model
    in
        ( newModel, Cmd.none )


replaceLastOperator : Operator -> Model -> Model
replaceLastOperator operator model =
    let
        lastPair =
            List.head model.numberOperatorPairs

        lastNumber =
            case lastPair of
                Just pair ->
                    pair.number

                Nothing ->
                    newDecimal

        otherPairs =
            List.tail model.numberOperatorPairs
                |> Maybe.withDefault []

        newLastPair =
            { number = lastNumber
            , operator = operator
            }
    in
        { model | numberOperatorPairs = newLastPair :: otherPairs }


pushNewPair : Operator -> Model -> Model
pushNewPair operator model =
    { numberOperatorPairs =
        { number = model.currentNumber
        , operator = operator
        }
            :: model.numberOperatorPairs
    , currentNumber = newDecimal
    }


handleDigit : Int -> Model -> ( Model, Cmd Msg )
handleDigit digit model =
    let
        currentNumber =
            model.currentNumber
    in
        case currentNumber.fractionalPart of
            Nothing ->
                -- No fractional part => add digit to integer part.
                ( { model
                    | currentNumber =
                        { currentNumber
                            | integerPart = digit :: currentNumber.integerPart
                        }
                  }
                , Cmd.none
                )

            Just fractionalPart ->
                -- We're building a fractional part => add the digit to that.
                ( { model
                    | currentNumber =
                        { currentNumber
                            | fractionalPart = Just (digit :: fractionalPart)
                        }
                  }
                , Cmd.none
                )


handleDecimalPoint : Model -> ( Model, Cmd Msg )
handleDecimalPoint model =
    let
        currentNumber =
            model.currentNumber
    in
        case currentNumber.fractionalPart of
            Nothing ->
                -- We don't have a fractional part already => start building
                -- one.
                ( { model
                    | currentNumber =
                        { currentNumber
                            | fractionalPart = Just []
                        }
                  }
                , Cmd.none
                )

            Just _ ->
                -- We're already building the fractional part => do nothing.
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
            , button [ onClick DecimalPoint ] [ text "." ]
            , button [] [ text "=" ]
            , operatorButton Plus
            ]
        ]


result : Model -> Html Msg
result model =
    span [] [ text (modelToString model) ]


modelToString : Model -> String
modelToString model =
    let
        pairs =
            List.map numberOperatorPairToString model.numberOperatorPairs
                |> List.reverse
                |> String.join ""
    in
        pairs ++ decimalToString model.currentNumber


numberOperatorPairToString : NumberOperatorPair -> String
numberOperatorPairToString pair =
    (decimalToString pair.number) ++ (operatorToString pair.operator)


operatorToString : Operator -> String
operatorToString operator =
    case operator of
        Plus ->
            "+"

        Minus ->
            "−"

        Multiply ->
            "×"

        Divide ->
            "÷"


decimalToString : Decimal -> String
decimalToString currentNumber =
    let
        integerPart =
            decimalPartToString currentNumber.integerPart

        fractionalPart =
            case currentNumber.fractionalPart of
                Just part ->
                    "." ++ decimalPartToString part

                Nothing ->
                    ""
    in
        integerPart ++ fractionalPart


decimalPartToString : List Int -> String
decimalPartToString part =
    List.map toString part
        |> List.reverse
        |> String.concat


numberButton : Int -> Html Msg
numberButton num =
    button [ onClick (Digit num) ] [ text (toString num) ]


operatorButton : Operator -> Html Msg
operatorButton operator =
    button [ onClick (Operator operator) ] [ text (operatorToString operator) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
