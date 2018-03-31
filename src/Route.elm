module Route exposing (Route(..), fromLocation)

import Navigation exposing (Location)
import UrlParser as Url exposing (parseHash, s, (</>), oneOf, Parser)
import String


type Route
    = Addition
    | StandardSubtraction
    | AustrianSubtraction


router : Parser (Route -> a) a
router =
    oneOf
        [ Url.map Addition (s "addition")
        , Url.map StandardSubtraction (s "subtraction" </> s "standard")
        , Url.map AustrianSubtraction (s "subtraction" </> s "austrian")
        ]


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just <| Addition
    else
        parseHash router location
