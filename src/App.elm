module App exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


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
    = Operator Operator
    | Digit Int
    | DecimalPoint
    | Equals


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Operator operator ->
            ( model, Cmd.none )

        Digit digit ->
            handleDigit digit model

        DecimalPoint ->
            handleDecimalPoint model

        Equals ->
            ( model, Cmd.none )


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
    span [] [ text (currentNumberString model.currentNumber) ]


currentNumberString : NumberUnderConstruction -> String
currentNumberString currentNumber =
    let
        integerPart =
            numberPartsToString currentNumber.integerPart

        fractionalPart =
            case currentNumber.fractionalPart of
                Just parts ->
                    "." ++ numberPartsToString parts

                Nothing ->
                    ""
    in
        integerPart ++ fractionalPart


numberPartsToString : List Int -> String
numberPartsToString parts =
    List.map toString parts
        |> List.reverse
        |> String.concat


numberButton : Int -> Html Msg
numberButton num =
    button [ onClick (Digit num) ] [ text (toString num) ]


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
