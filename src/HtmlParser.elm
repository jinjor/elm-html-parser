module HtmlParser exposing
  ( Node(..), Attributes
  , parse
  )

{-| The HTML Parser.

# AST
@docs Node, Attributes

# Parse
@docs parse
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
type Node
  = Text String
  | Element String Attributes (List Node)
  | Comment String


{-| Attributes of a node
-}
type alias Attributes =
  List (String, String)


{-| Parse HTML.

```elm
parse "text" == [ Text "text" ]

parse "<h1>Hello<br>World</h1> "
  == [ Element "h1" [] [ Text "Hello", Element "br" [] [], Text "World" ] ]

parse """<a href="http://example.com">Example</a>"""
  == [ Element "a" [("href", "http://example.com")] [ Text "Example" ] ]
```

-}
parse : String -> List Node
parse s =
  case fst (Combine.parse nodesAndEnd s) of
    Ok x -> x
    Err _ -> []


-- PARSER


nodesAndEnd : Parser (List Node)
nodesAndEnd =
  (\nodes _ -> nodes)
  `map` many (node "")
  `andMap` end


spaces : Parser String
spaces =
  regex "[ \t\r\n]*"


spaces1 : Parser String
spaces1 =
  regex "[ \t\r\n]+"


spaced : Parser a -> Parser a
spaced p =
  between spaces spaces p


tagName : Parser String
tagName =
  map String.toLower (regex "[a-zA-Z][a-zA-Z0-9\\-]*")


attributeName : Parser String
attributeName =
  map String.toLower (regex "[a-zA-Z][a-zA-Z:\\-]*")


attributeQuotedValue : Parser String
attributeQuotedValue =
  between (string "\"") (string "\"") (attributeString "\"") `or`
  between (string "'") (string "'") (attributeString "'")


-- HTML5
attributeBareValue : Parser String
attributeBareValue =
  regex """[^ ^`^"^'^<^>^=]+"""


attributeValue : Parser String
attributeValue =
  attributeQuotedValue `or` attributeBareValue


attributeNameValuePair : Parser (String, String)
attributeNameValuePair =
  (\name _ value -> (name, value))
  `map` attributeName
  `andMap` between spaces spaces (string "=")
  `andMap` attributeValue


attribute : Parser (String, String)
attribute =
  attributeNameValuePair `or`
  map (flip (,) "") attributeName


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
  (tagName == "head" && childTagName == "body") ||
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
  (tagName == "tr" && (childTagName == "tr" || childTagName == "thead" || childTagName == "tbody" || childTagName == "tfoot")) ||
  (tagName == "td" && (childTagName == "td" || childTagName == "th" || childTagName == "tr" || childTagName == "tbody" || childTagName == "tfoot")) ||
  (tagName == "th" && (childTagName == "td" || childTagName == "th" || childTagName == "tr" || childTagName == "tbody" || childTagName == "tfoot"))


node : String -> Parser Node
node parentTagName =
  rec (\_ ->
    doctypeNode `or`
    singleNode `or`
    normalNode parentTagName `or`
    commentNode `or`
    textNode
  )


doctypeNode : Parser Node
doctypeNode =
  map (\_ -> Element "!DOCTYPE" [] []) (regex "<!DOCTYPE [^>]*>")


normalNode : String -> Parser Node
normalNode parentTagName =
  rec (\_ ->
    startTag `andThen` \(tagName, attrs) ->
      if tagName == "script" || tagName == "style" then
        (\children -> Element tagName attrs children)
        `map` untilScriptEnd tagName
      else if isInvalidNest parentTagName tagName then
        fail []
      else if Set.member tagName startTagOnly then
        succeed (Element tagName attrs [])
      else
        (\children -> Element tagName attrs children)
        `map` untilEndTag tagName
  )


untilEndTag : String -> Parser (List Node)
untilEndTag tagName =
  rec (\_ ->
    (\children1 children2 -> children1 ++ children2)
    `map` many (node tagName)
    `andMap`
      -- if strict, end tag is optional only when `Set.member tagName optionalEndTag`
      optional [] ( generalEndTag `andThen` \endTagName ->
        if tagName == endTagName then
          succeed []
        else
          untilEndTag tagName
      )
  )


textNode : Parser Node
textNode =
  map Text textNodeString


textNodeString : Parser String
textNodeString =
  (\list -> String.join "" list)
  `map` many (entityString `or` textNodeNonEntityString)


attributeString : String -> Parser String
attributeString quote =
  (\list -> String.join "" list)
  `map` many (entityString `or` attributeValueEntityString quote)


entityString : Parser String
entityString =
  (\code ->
    Maybe.withDefault
      code
      (Dict.get code Escape.dict)
  )
  `map` (regex "&[#0-9a-zA-Z]*;")


textNodeNonEntityString : Parser String
textNodeNonEntityString =
  regex "[^<^&]*"


attributeValueEntityString : String -> Parser String
attributeValueEntityString quote =
  regex ("[^<^&^" ++ quote ++ "]*")


singleNode : Parser Node
singleNode =
  map (\(tagName, attrs) -> Element tagName attrs []) singleTag


startTag : Parser (String, List (String, String))
startTag =
  rec (\_ ->
    (\_ tagName attrs _ -> (tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` between spaces spaces (sepBy spaces attribute)
    `andMap` string ">"
  )


endTag : String -> Parser ()
endTag tagName =
  (\_ -> ())
  `map` between (string "</") (string ">") (string tagName `or` string (String.toUpper tagName))


generalEndTag : Parser String
generalEndTag =
  between (string "</") (string ">") tagName


singleTag : Parser (String, List (String, String))
singleTag =
  rec (\_ ->
    (\_ tagName attrs _ -> (tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` between spaces spaces (sepBy spaces attribute)
    `andMap` string "/>"
  )


(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  (flip always) `map` lp `andMap` rp


commentNode : Parser Node
commentNode =
  string "<!--" *> untilCommentEnd


untilCommentEnd : Parser Node
untilCommentEnd =
  map Comment <|
  map String.fromList <|
  manyTill Combine.Char.anyChar (string "-->")


untilScriptEnd : String -> Parser (List Node)
untilScriptEnd tagName =
  rec (\_ ->
    (\(s, rest) -> if s == "" then rest else Text s :: rest)
    `map` untilScriptEndHelp tagName
  )


untilScriptEndHelp : String -> Parser (String, List Node)
untilScriptEndHelp tagName =
  rec (\_ ->
    (regex "[^<]*") `andThen` \s ->
      ( (\_ comment rest -> (s, comment :: rest))
        `map` string "<!--"
        `andMap` untilCommentEnd
        `andMap` untilScriptEnd tagName
      ) `or`
      ( (\_ -> (s, []))
        `map` endTag tagName
      ) `or`
      ( (\lt (next, rest) -> (s ++ lt ++ next, rest))
        `map` string "<"
        `andMap` untilScriptEndHelp tagName
      )
  )
