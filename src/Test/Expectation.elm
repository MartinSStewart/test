module Test.Expectation exposing (Expectation(..), Viewer, fail, withGiven)

import Element exposing (Element)
import Test.Runner.Failure exposing (Reason)


type Expectation flags model msg
    = Pass
    | Fail { given : Maybe String, description : String, reason : Reason, viewer : Maybe (Viewer flags model msg) }


type alias Viewer flags model msg =
    { init : flags -> model
    , update : msg -> model -> model
    , view : model -> Element msg
    }


{-| Create a failure without specifying the given.
-}
fail : { description : String, reason : Reason } -> Expectation flags model msg
fail { description, reason } =
    Fail { given = Nothing, description = description, reason = reason, viewer = Nothing }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven : String -> Expectation flags model msg -> Expectation flags model msg
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail { failure | given = Just newGiven }

        Pass ->
            expectation
