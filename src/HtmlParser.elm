module HtmlParser exposing
  ( HtmlNode(..), AttributeValue(..)
  , parse, parseOne
  )

{-|
# AST
@docs HtmlNode, AttributeValue

# Parse
@docs parse, parseOne
-}

import String
import Combine exposing (..)
import Combine.Char
import Set exposing (Set)
import String
import Escape
import Dict


{-| The AST of node
-}
type HtmlNode
  = Text String
  | Node String (List (String, AttributeValue)) (List HtmlNode)
  | Comment String


{-| The AST of attribute value
-}
type AttributeValue
  = StringValue String
  | NumberValue String
  | NoValue

{-| Parses HTML. The input string is trimmed before parsing.

```elm
parse "text" == [ Text "text" ]

parse "<h1>Hello<br>World</h1> "
  == [ Node "h1" [] [ Text "Hello", Node "br" [] [], Text "World" ] ]

parse "<a href="http://example.com">Example</a>"
  == [ Node "a" [("href", StringValue "http://example.com")] [ Text "Example" ] ]
```

-}
parse : String -> Result (List String) (List HtmlNode)
parse s =
  fst (Combine.parse nodesAndEnd (String.trim s))

{-| Parses first node. The input string is trimmed before parsing.

```elm
parseOne "text" == Text "text"

parseOne "<h1>Hello</h1><p>bla bla</p>" == Node "h1" [] [ Text "Hello" ]
```

-}
parseOne : String -> Result (List String) HtmlNode
parseOne s =
  fst (Combine.parse (node "") (String.trim s))


-- PARSER


nodesAndEnd : Parser (List HtmlNode)
nodesAndEnd =
  (\nodes _ -> nodes)
  `map` many (node "")
  `andMap` end


spaces : Parser String
spaces =
  regex "[ \t\r\n]*"


spaced : Parser a -> Parser a
spaced p =
  between spaces spaces p


tagName : Parser String
tagName =
  regex "[a-zA-Z][a-zA-Z0-9\\-]*"


attributeName : Parser String
attributeName =
  regex "[a-zA-Z][a-zA-Z:\\-]*"


attributeValueNumber : Parser AttributeValue
attributeValueNumber =
  map NumberValue (regex "[0-9][0-9.]*")


attributeValueString : Parser AttributeValue
attributeValueString =
  map StringValue (between (string "\"") (string "\"") (regex """(\\\\"|[^"])*""")) `or`
  map StringValue (between (string "'") (string "'") (regex """(\\\\'|[^'])*"""))


attributeValueBareString : Parser AttributeValue
attributeValueBareString =
  map StringValue (regex "[a-zA-Z0-9\\-]+")


attributeValue : Parser AttributeValue
attributeValue =
  attributeValueNumber `or` attributeValueString `or` attributeValueBareString


attributeNameValuePair : Parser (String, AttributeValue)
attributeNameValuePair =
  (\name _ _ _ value -> (name, value))
  `map` attributeName
  `andMap` spaces
  `andMap` string "="
  `andMap` spaces
  `andMap` attributeValue


attribute : Parser (String, AttributeValue)
attribute =
  attributeNameValuePair `or` map (flip (,) NoValue) attributeName


startTagOnly : Set String
startTagOnly =
  Set.fromList
    [ "br", "img", "hr", "meta", "input", "embed", "area", "base", "col"
    , "keygen", "link", "param", "source", "command", "link", "track", "wbr"
    ]


-- see https://html.spec.whatwg.org/multipage/syntax.html#optional-tags
optionalEndTag : Set String
optionalEndTag =
  Set.fromList
    [ "li", "dt", "dd", "p", "rt", "rp", "optgroup", "option", "colgroup"
    , "caption", "thead", "tbody", "tfoot", "tr", "td", "th" ]


ngSetForP : Set String
ngSetForP =
  Set.fromList
    [ "address", "article", "aside", "blockquote", "details", "div", "dl"
    , "fieldset", "figcaption", "figure", "footer", "form", "h1", "h2", "h3"
    , "h4", "h5", "h6", "header", "hgroup", "hr", "main", "menu", "nav", "ol"
    , "p", "pre", "section", "table", "ul"
    ]


-- this logic is used to help optional end tag
isInvalidNest : String -> String -> Bool
isInvalidNest tagName childTagName =
  (tagName == "li" && childTagName == "li") ||
  (tagName == "dt" && (childTagName == "dt" || childTagName == "dd")) ||
  (tagName == "dd" && (childTagName == "dt" || childTagName == "dd")) ||
  (tagName == "p" && Set.member childTagName ngSetForP) ||
  (tagName == "rt" && (childTagName == "rt" || childTagName == "rp")) ||
  (tagName == "rp" && (childTagName == "rt" || childTagName == "rp")) ||
  (tagName == "optgroup" && childTagName == "optgroup") ||
  (tagName == "option" && (childTagName == "option" || childTagName == "optgroup")) ||
  (tagName == "colgroup" && childTagName /= "col") ||
  (tagName == "caption") ||
  (tagName == "thead" && (childTagName == "tbody" || childTagName == "tfoot")) ||
  (tagName == "tbody" && (childTagName == "tbody" || childTagName == "tfoot" || childTagName == "table")) ||
  (tagName == "tfoot" && childTagName == "table") ||
  (tagName == "tr" && childTagName == "tr") ||
  (tagName == "td" && (childTagName == "td" || childTagName == "th" || childTagName == "tr" || childTagName == "tbody" || childTagName == "tfoot")) ||
  (tagName == "th" && (childTagName == "td" || childTagName == "th" || childTagName == "tr" || childTagName == "tbody" || childTagName == "tfoot"))


node : String -> Parser HtmlNode
node parentTagName =
  rec (\_ ->
    doctypeNode `or`
    singleNode `or`
    normalNode parentTagName `or`
    commentNode `or`
    textNode
  )


doctypeNode : Parser HtmlNode
doctypeNode =
  map (\_ -> Node "!DOCTYPE" [] []) (regex "<!DOCTYPE [^>]*>")


normalNode : String -> Parser HtmlNode
normalNode parentTagName =
  rec (\_ ->
    startTag `andThen` \(tagName, attrs) ->
      if tagName == "script" || tagName == "style" then
        (\children _ -> Node tagName attrs children)
        `map` many (commentNode `or` textNode)
        `andMap` endTag tagName
      else if isInvalidNest parentTagName tagName then
        fail []
      else if Set.member tagName startTagOnly then
        succeed (Node tagName attrs [])
      else
        (\children _ -> Node tagName attrs children)
        `map` many (node tagName)
        `andMap`
          ( if Set.member tagName optionalEndTag then
              optional ()
            else
              identity
          ) (endTag tagName)
  )


textNode : Parser HtmlNode
textNode =
  map Text textNodeString


textNodeString : Parser String
textNodeString =
  (\list -> String.join "" list)
  `map` many (textNodeStringEscape `or` textNodeStringNonEscape)


textNodeStringEscape : Parser String
textNodeStringEscape =
  (\code ->
    case Dict.get code Escape.dict of
      Just s ->
        s

      Nothing ->
        code
  )
  `map` (regex "&[#0-9a-zA-Z]*;")


textNodeStringNonEscape : Parser String
textNodeStringNonEscape =
  regex "[^<]*"


singleNode : Parser HtmlNode
singleNode =
  map (\(tagName, attrs) -> Node tagName attrs []) singleTag


startTag : Parser (String, List (String, AttributeValue))
startTag =
  rec (\_ ->
    (\_ tagName _ attrs _ _ -> (String.toLower tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` spaces
    `andMap` sepBy spaces attribute
    `andMap` spaces
    `andMap` string ">"
  )


endTag : String -> Parser ()
endTag tagName =
  (\_ _ _ -> ())
  `map` string "</"
  `andMap` (string tagName `or` string (String.toUpper tagName))
  `andMap` string ">"


untilEndTag : String -> Parser ()
untilEndTag tagName =
  map (always ()) <|
  manyTill Combine.Char.anyChar (endTag tagName)


singleTag : Parser (String, List (String, AttributeValue))
singleTag =
  rec (\_ ->
    (\_ tagName _ attrs _ _ -> (String.toLower tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` spaces
    `andMap` sepBy spaces attribute
    `andMap` spaces
    `andMap` string "/>"
  )


(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  (flip always) `map` lp `andMap` rp


commentNode : Parser HtmlNode
commentNode =
  map Comment comment


comment : Parser String
comment =
  map String.fromList <|
  string "<!--" *> manyTill Combine.Char.anyChar (string "-->")
