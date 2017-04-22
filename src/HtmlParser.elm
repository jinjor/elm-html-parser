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


import Char
import Dict
import Combine exposing (..)
import Combine.Char
import Set exposing (Set)
import Escape
import Hex


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

parse "<h1>Hello<br>World</h1>"
  == [ Element "h1" [] [ Text "Hello", Element "br" [] [], Text "World" ] ]

parse """<a href="http://example.com">Example</a>"""
  == [ Element "a" [("href", "http://example.com")] [ Text "Example" ] ]
```

-}
parse : String -> List Node
parse s =
  case Combine.parse nodesAndEnd s of
    Ok (_, _, x) -> x
    Err _ -> []


-- PARSER


nodesAndEnd : Parser s (List Node)
nodesAndEnd =
  (\nodes _ -> nodes)
  <$> untilEndTag ""
  <*> end


spaces : Parser s String
spaces =
  regex "[ \t\r\n]*"


spaces1 : Parser s String
spaces1 =
  regex "[ \t\r\n]+"


spaced : Parser s a -> Parser s a
spaced p =
  between spaces spaces p


tagName : Parser s String
tagName =
  map String.toLower (regex "[a-zA-Z][a-zA-Z0-9\\-]*")


attributeName : Parser s String
attributeName =
  map String.toLower (regex "[a-zA-Z][a-zA-Z0-9:\\-]*")


attributeQuotedValue : Parser s String
attributeQuotedValue =
  between (string "\"") (string "\"") (attributeString "\"") <|>
  between (string "'") (string "'") (attributeString "'")


-- HTML5
attributeBareValue : Parser s String
attributeBareValue =
  regex """[^ `"'<>=\n\r\t]+"""


attributeValue : Parser s String
attributeValue =
  attributeQuotedValue <|> attributeBareValue


attributeNameValuePair : Parser s (String, String)
attributeNameValuePair =
  (\name _ value -> (name, value))
  <$> attributeName
  <*> between spaces spaces (string "=")
  <*> attributeValue


attribute : Parser s (String, String)
attribute =
  attributeNameValuePair <|>
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


node : String -> Parser s Node
node parentTagName =
  lazy (\_ ->
    doctypeNode <|>
    singleNode <|>
    normalNode parentTagName <|>
    commentNode <|>
    textNode
  )


doctypeNode : Parser s Node
doctypeNode =
  map (\_ -> Element "!DOCTYPE" [] []) (regex "<!DOCTYPE [^>]*>")


normalNode : String -> Parser s Node
normalNode parentTagName =
  lazy (\_ ->
    startTag
      |> andThen (\(tagName, attrs) ->
        if tagName == "script" || tagName == "style" then
          (\children -> Element tagName attrs children)
          <$> untilScriptEnd tagName
        else if isInvalidNest parentTagName tagName then
          fail ""
        else if Set.member tagName startTagOnly then
          succeed (Element tagName attrs [])
        else
          (\children -> Element tagName attrs children)
          <$> untilEndTag tagName
      )
  )


untilEndTag : String -> Parser s (List Node)
untilEndTag tagName =
  lazy (\_ ->
    (\children1 children2 -> children1 ++ children2)
    <$> many (node tagName)
    <*>
      -- if strict, end tag is optional only when `Set.member tagName optionalEndTag`
      optional []
        ( generalEndTag
          |> andThen
            (\endTagName ->
              if tagName == endTagName then
                succeed []
              else
                untilEndTag tagName
            )
        )
  )


textNode : Parser s Node
textNode =
  map Text textNodeString


textNodeString : Parser s String
textNodeString =
  (\list -> String.join "" list)
  <$> many (entityString <|> entityStringHex <|> entityStringDec <|> string "&" <|> textNodeNonEntityString)


attributeString : String -> Parser s String
attributeString quote =
  (\list -> String.join "" list)
  <$> many (entityString <|> entityStringHex <|> entityStringDec <|> string "&" <|> attributeValueEntityString quote)


entityString : Parser s String
entityString =
  (\code ->
    Maybe.withDefault
      code
      (Dict.get code Escape.dict)
  )
  <$> (regex "&[0-9a-zA-Z]+;")


entityStringHex : Parser s String
entityStringHex =
  (\num ->
    num
      |> String.dropLeft 3
      |> String.dropRight 1
      |> String.toLower
      |> Hex.fromString
      |> Result.map ( Char.fromCode >> List.singleton >> String.fromList )
      |> Result.withDefault num
  )
  <$> (regex "&#x[0-9A-F]+;")


entityStringDec : Parser s String
entityStringDec =
  (\num ->
    num
      |> String.dropLeft 2
      |> String.dropRight 1
      |> String.toInt
      |> Result.map ( Char.fromCode >> List.singleton >> String.fromList )
      |> Result.withDefault num
  )
  <$> (regex "&#[1-9]*[0-9]+;")


textNodeNonEntityString : Parser s String
textNodeNonEntityString =
  regex "[^<&]*"


attributeValueEntityString : String -> Parser s String
attributeValueEntityString quote =
  regex ("[^<&" ++ quote ++ "]*")


singleNode : Parser s Node
singleNode =
  map (\(tagName, attrs) -> Element tagName attrs []) singleTag


startTag : Parser s (String, List (String, String))
startTag =
  (\_ tagName attrs _ -> (tagName, attrs))
  <$> string "<"
  <*> tagName
  <*> between spaces spaces (sepBy spaces attribute)
  <*> string ">"


endTag : String -> Parser s ()
endTag tagName =
  generalEndTag
    |> andThen
      (\endTagName ->
        if tagName == endTagName then
          succeed ()
        else
          fail ""
      )


generalEndTag : Parser s String
generalEndTag =
  (\_ tagName _ _ -> tagName)
  <$> string "</"
  <*> tagName
  <*> spaces
  <*> string ">"


singleTag : Parser s (String, List (String, String))
singleTag =
  lazy (\_ ->
    (\_ tagName attrs _ -> (tagName, attrs))
    <$> string "<"
    <*> tagName
    <*> between spaces spaces (sepBy spaces attribute)
    <*> string "/>"
  )


commentNode : Parser s Node
commentNode =
  string "<!--" *> untilCommentEnd


untilCommentEnd : Parser s Node
untilCommentEnd =
  map Comment <|
  map String.fromList <|
  manyTill Combine.Char.anyChar (string "-->")


untilScriptEnd : String -> Parser s (List Node)
untilScriptEnd tagName =
  lazy (\_ ->
    (\(s, rest) -> if s == "" then rest else Text s :: rest)
    <$> untilScriptEndHelp tagName
  )


untilScriptEndHelp : String -> Parser s (String, List Node)
untilScriptEndHelp tagName =
  lazy (\_ ->
    (regex "[^<]*")
      |> andThen
        (\s ->
          ( (\_ comment rest -> (s, comment :: rest))
            <$> string "<!--"
            <*> untilCommentEnd
            <*> untilScriptEnd tagName
          ) <|>
          ( (\_ -> (s, []))
            <$> endTag tagName
          ) <|>
          ( (\lt (next, rest) -> (s ++ lt ++ next, rest))
            <$> string "<"
            <*> untilScriptEndHelp tagName
          )
        )
  )
