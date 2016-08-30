module HtmlParser exposing (..)

import HtmlParser.AST exposing (..)
import Combine exposing (..)
import String


parse : String -> Result (List String) AST
parse s =
  fst (Combine.parse node (String.trim s))


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
  regex "[a-zA-Z][a-zA-Z\\-]*"


attributeValueNumber : Parser AttributeValue
attributeValueNumber =
  map NumberValue (regex "[1-9][0-9.]*")


attributeValueString : Parser AttributeValue
attributeValueString =
  map StringValue (between (string "\"") (string "\"") (regex """(\\\\"|[^"])*"""))


attributeValueBareString : Parser AttributeValue
attributeValueBareString =
  map StringValue (regex "[a-zA-Z]+")


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


node : Parser AST
node =
  rec (\_ ->
    singleNode `or`
    (startTag `andThen` \(tagName, attrs) ->
      (\children _ -> Node tagName attrs children)
      `map` many node
      `andMap` endTag tagName
    ) `or`
    textNode
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
    (\_ tagName _ attrs _ _ -> (tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` spaces
    `andMap` sepBy spaces attribute
    `andMap` spaces
    `andMap` string ">"
  )


endTag : String -> Parser ()
endTag tagName =
  rec (\_ ->
    (\_ _ _ -> ())
    `map` string "</"
    `andMap` string tagName
    `andMap` string ">"
  )


singleTag : Parser (String, List (String, AttributeValue))
singleTag =
  rec (\_ ->
    (\_ tagName _ attrs _ _ -> (tagName, attrs))
    `map` string "<"
    `andMap` tagName
    `andMap` spaces
    `andMap` sepBy spaces attribute
    `andMap` spaces
    `andMap` string "/>"
  )


comment : Parser String
comment =
  between (string "<!--") (string "-->") (regex """(\\\\"|[^"])*""")
