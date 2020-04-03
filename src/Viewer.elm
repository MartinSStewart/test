module Viewer exposing (toProgram)

import Browser
import Element exposing (Element)
import Expect
import Html exposing (Html)
import Random
import Test exposing (Test)
import Test.Internal as Internal
import Test.Runner.Failure exposing (Reason(..))


type Msg
    = NoOp


type alias Model_ =
    {}


type Model
    = Model Model_


init : () -> ( Model, Cmd Msg )
init flags =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout
        []
        Element.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toProgram : Test -> Platform.Program () Model Msg
toProgram tests =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
