module Tests exposing (tests)

import Dict
import DictSafe exposing (DictSafe)
import Expect
import Fuzz
import Test exposing (Test)


tests : Test
tests =
    Test.describe "elm-lookup-safe"
        []
