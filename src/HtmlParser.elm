module HtmlParser exposing (parse, parseOne)

{-| Each functions in this module has the same interface as [Html.App](http://package.elm-lang.org/packages/elm-lang/html/1.0.0/Html-App)
# AST
@docs AST, AttributeValue

# Parse
@docs parse, parseOne
-}

import Combine
import Internal.AST exposing (..)
import Internal.Parser exposing (..)
import String

{-| The AST of node
-}
type alias AST = Internal.AST.AST

{-| The AST of attribute value
-}
type alias AttributeValue = Internal.AST.AttributeValue

{-| parse HTML

```elm
parse "text" == [Text "text"]

parse " <h1>Hello</h1> " == [Text " ", Node "h1" [] [ Text "Hello" ], Text " "]

parseOne "<a href="http://example.com">Example</a>"
  == Node "a" [("href", StringValue "http://example.com")] [ Text "Example" ]
```

-}
parse : String -> Result (List String) (List AST)
parse s =
  fst (Combine.parse nodesAndEnd s)

{-| parse one node

```elm
parseOne "text" == Text "text"

parseOne " text " == Text "text"

parseOne "<h1>Hello</h1><p>bla bla</p>" == Node "h1" [] [ Text "Hello" ]
```

-}
parseOne : String -> Result (List String) AST
parseOne s =
  fst (Combine.parse (node "") (String.trim s))
