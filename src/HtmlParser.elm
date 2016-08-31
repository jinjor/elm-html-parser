module HtmlParser exposing (..)

import HtmlParser.AST exposing (..)
import Combine exposing (..)
import Combine.Char
import String
import Set exposing (Set)


parseNode : String -> Result (List String) AST
parseNode s =
  fst (Combine.parse nodeAndEnd (String.trim s))


all : Parser (List AST)
all =
  (\_ _ nodes _ _ -> nodes)
  `map` doctype
  `andMap` spaces
  `andMap` many (node "")
  `andMap` spaces
  `andMap` end


nodeAndEnd : Parser AST
nodeAndEnd =
  (\_ node _ _ -> node)
  `map` spaces
  `andMap` node ""
  `andMap` spaces
  `andMap` end


spaces : Parser String
spaces =
  regex "[ \t\r\n]*"


spaced : Parser a -> Parser a
spaced p =
  between spaces spaces p


tagName : Parser String
tagName =
  regex "[a-zA-Z][a-zA-Z\\-]*"


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


node : String -> Parser AST
node parentTagName =
  rec (\_ ->
    singleNode `or`
    normalNode parentTagName `or`
    commentNode `or`
    textNode
  )


doctype : Parser ()
doctype =
  map (always ()) (regex "<!DOCTYPE [^>]*>")


normalNode : String -> Parser AST
normalNode parentTagName =
  rec (\_ ->
    startTag `andThen` \(tagName, attrs) ->
      if tagName == "script" || tagName == "style" then
        fail [ tagName ++ " is not supported" ]
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


textNode : Parser AST
textNode =
  map Text (regex "[^<]*") -- TODO


singleNode : Parser AST
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


commentNode : Parser AST
commentNode =
  map Comment comment


comment : Parser String
comment =
  map String.fromList <|
  string "<!--" *> manyTill Combine.Char.anyChar (string "-->")
