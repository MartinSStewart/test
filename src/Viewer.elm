module Viewer exposing (Viewer, ViewerMsg, toProgram, viewerTest)

import Browser
import Element exposing (Element)
import Expect
import Html exposing (Html)
import Random
import Test
import Test.Internal as Internal
import Test.Runner.Failure exposing (Reason(..))


type alias Viewer actual model msg =
    { init : actual -> model
    , update : msg -> model -> model
    , view : model -> Element msg
    }


type alias ViewerBuilder model msg =
    { init : model
    , update : msg -> model -> model
    , view : model -> List (Element msg)
    }


type alias TestResult actual =
    { result : actual
    , passed : Bool
    }


type ViewerTest actual
    = UnitTest (() -> TestResult actual)
    | FuzzTest (Random.Seed -> Int -> TestResult actual)


type ViewerMsg nestedMsg msg
    = NestedMsg nestedMsg
    | CurrentMsg msg


init : ViewerBuilder () msg
init =
    { init = ()
    , update = \_ m -> m
    , view = always []
    }


viewerTest :
    Viewer actual model msg
    -> ViewerTest actual
    -> ViewerBuilder nestedModel nestedMsg
    -> ViewerBuilder ( nestedModel, model ) (ViewerMsg nestedMsg msg)
viewerTest viewer test viewerBuilder =
    { init = ( viewerBuilder.init, viewer.init test )
    , update =
        \msg ( nestedModel, model ) ->
            case msg of
                NestedMsg nestedMsg ->
                    ( viewerBuilder.update nestedMsg nestedModel, model )

                CurrentMsg currentMsg ->
                    ( nestedModel, viewer.update currentMsg model )
    , view =
        \( nestedModel, model ) ->
            (viewer.view model |> Element.map CurrentMsg)
                :: (viewerBuilder.view nestedModel |> List.map (Element.map NestedMsg))
    }


type alias Model viewerModel =
    { viewerModel : viewerModel
    }


type Msg viewerMsg
    = NoOp
    | ViewerMsg viewerMsg


update :
    ViewerBuilder model msg
    -> Msg msg
    -> Model model
    -> ( Model model, Cmd (Msg msg) )
update viewers msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ViewerMsg viewerMsg ->
            ( { model | viewerModel = viewers.update viewerMsg model.viewerModel }, Cmd.none )


view : ViewerBuilder model msg -> Model model -> Html (Msg msg)
view viewers model =
    Element.layout
        []
    <|
        Element.column
            [ Element.width Element.fill, Element.height Element.fill ]
            [ Element.text "Test Viewer"
            , Element.row [] (viewers.view model.viewerModel) |> Element.map ViewerMsg
            ]


toProgram : ViewerBuilder model msg -> Platform.Program flags (Model model) (Msg msg)
toProgram viewerBuilder =
    Browser.element
        { init = always ( { viewerModel = viewerBuilder.init }, Cmd.none )
        , update = update viewerBuilder
        , view = view viewerBuilder
        , subscriptions = always Sub.none
        }


a =
    init
        |> viewerTest
            { init = \_ -> ()
            , update = \_ m -> m
            , view = \_ -> Element.text "test"
            }
            (Test.test "" <| \_ -> Expect.equal 5 4)
        |> viewerTest
            { init = \_ -> ()
            , update = \_ m -> m
            , view = \_ -> Element.text "test 2"
            }
            (Test.test "" <| \_ -> Expect.equal 5 5)
