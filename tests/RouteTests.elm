module RouteTests exposing (testSuite)

import Test exposing (..)
import Expect
import String
import Route exposing (Route(..), fromLocation)
import Navigation exposing (Location)


testSuite : Test
testSuite =
    describe "Route tests"
        [ describe "no hash nor query string"
            [ test "no hash -> `Addition`" <|
                \() ->
                    Expect.equal (parseRoute "") (Just Addition)
            ]
        , describe "Addition paths"
            [ test "`#addition` -> `Addition`" <|
                \() ->
                    Expect.equal (parseRoute "#addition") (Just Addition)
            ]
        , describe "subtraction/standard paths"
            [ test "`#subtraction/standard` -> `StandardSubtraction`" <|
                \() ->
                    Expect.equal (parseRoute "#subtraction/standard") (Just StandardSubtraction)
            ]
        , describe "subtraction/austrian paths"
            [ test "`#subtraction/austrian` -> `AustrianSubtraction`" <|
                \() ->
                    Expect.equal (parseRoute "#subtraction/austrian") (Just AustrianSubtraction)
            ]
        ]


parseRoute : String -> Maybe Route
parseRoute h =
    search h |> fromLocation


search : String -> Location
search h =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = h
    , username = ""
    , password = ""
    }
