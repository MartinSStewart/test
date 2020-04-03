module Test.Expectation exposing (Expectation(..), TestResult(..), fail, result, textViewer, withGiven)

import Element exposing (Element)
import Test.Runner.Failure exposing (Reason)


type Expectation
    = Expectation
        { viewer : TestResult -> Element Never
        , result : TestResult
        }


result : Expectation -> TestResult
result (Expectation expectation) =
    expectation.result


textViewer : TestResult -> Element msg
textViewer result_ =
    case result_ of
        Pass ->
            Element.text "PASSED"

        Fail failure ->
            Element.text failure.description


type TestResult
    = Pass
    | Fail { given : Maybe String, description : String, reason : Reason }


{-| Create a failure without specifying the given.
-}
fail : { description : String, reason : Reason } -> Expectation
fail { description, reason } =
    Expectation
        { viewer = textViewer
        , result = Fail { given = Nothing, description = description, reason = reason }
        }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven : String -> Expectation -> Expectation
withGiven newGiven (Expectation expectation) =
    case expectation.result of
        Fail failure ->
            Expectation { viewer = expectation.viewer, result = Fail { failure | given = Just newGiven } }

        Pass ->
            Expectation expectation
