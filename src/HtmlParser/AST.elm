module HtmlParser.AST exposing (..)


type AttributeValue
  = StringValue String
  | NumberValue String
  | NoValue


type AST
  = Text String
  | Node String (List (String, AttributeValue)) (List AST)
  | Comment String
