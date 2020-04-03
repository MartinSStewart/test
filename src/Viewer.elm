module Viewer exposing (toProgram)

import Browser
import Element exposing (Element)
import Html exposing (Html)
import Random
import Test exposing (Test)
import Test.Expectation exposing (Expectation)
import Test.Runner


type Msg
    = NoOp


type alias Model_ =
    {}


type Model
    = Model Model_


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : List FinishedTest -> Model -> Html Msg
view finishedTests model =
    Element.layout
        []
        Element.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias FinishedTest =
    { ran : List Expectation
    , labels : List String
    }


runnerToFinishedRunner : Test.Runner.Runner -> FinishedTest
runnerToFinishedRunner runner =
    { ran = runner.run ()
    , labels = runner.labels
    }


a =
    case Nothing of
        Just _ ->
            0

        Nothing ->
            0
                |> List.singleton


toProgram : Test -> Platform.Program () Model Msg
toProgram tests =
    let
        results =
            case Test.Runner.fromTest 100 (Random.initialSeed 123123) tests of
                Test.Runner.Plain runners ->
                    runners

                Test.Runner.Only runners ->
                    runners

                Test.Runner.Skipping runners ->
                    runners

                Test.Runner.Invalid _ ->
                    []
                        |> List.map runnerToFinishedRunner
    in
    Browser.element
        { init = init
        , update = update
        , view = view results
        , subscriptions = subscriptions
        }
