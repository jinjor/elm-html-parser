module Tests exposing (..)

import String
import Combine as RawParser exposing (..)
import HtmlParser.AST exposing (..)
import HtmlParser as HtmlParser exposing (..)
import ElmTest exposing (..)

isOk : Result a b -> Bool
isOk r =
  case r of
    Ok _ -> True
    _ -> False


testParse : String -> AST -> Assertion
testParse s ast =
  assertEqual (Ok ast) (HtmlParser.parse s)


textNodeTests : Test
textNodeTests =
  suite "TextNode"
    [ test "basic" (testParse "1" (Text "1"))
    , test "basic" (testParse "a" (Text "a"))
    , test "basic" (testParse "1a" (Text "1a"))
    ]


nodeTests : Test
nodeTests =
  suite "Node"
    [ test "basic" (testParse "<a></a>" (Node "a" [] []))
    , test "basic" (testParse "<a>a</a>" (Node "a" [] [ Text "a" ]))
    , test "basic" (testParse "<a> a </a>" (Node "a" [] [ Text " a " ]))
    , test "basic" (testParse "<a/>" (Node "a" [] []))
    , test "basic" (testParse "<a><a></a></a>" (Node "a" [] [ Node "a" [] [] ]))
    , test "basic" (testParse "<a> <a> </a> </a>" (Node "a" [] [ Text " ", Node "a" [] [ Text " " ], Text " " ]))
    , test "basic" (testParse "<a><a/></a>" (Node "a" [] [ Node "a" [] [] ]))
    , test "basic" (testParse "<a> <a/> </a>" (Node "a" [] [ Text " ", Node "a" [] [], Text " " ]))
    , test "basic" (testParse "<a><a></a><a></a></a>" (Node "a" [] [ Node "a" [] [], Node "a" [] [] ]))
    , test "basic" (testParse "<a><a><a></a></a></a>" (Node "a" [] [ Node "a" [] [ Node "a" [] [] ] ]))
    , test "basic" (testParse "<a><a></a><b></b></a>" (Node "a" [] [ Node "a" [] [], Node "b" [] [] ]))
    , test "opened" (testParse "<a> <br> </a>" (Node "a" [] [ Text " ", Node "br" [] [], Text " " ]))
    , test "opened" (testParse "<a><br><br></a>" (Node "a" [] [ Node "br" [] [], Node "br" [] [] ]))
    , test "opened" (testParse "<a><br><img><hr><meta></a>" (Node "a" [] [ Node "br" [] [], Node "img" [] [], Node "hr" [] [], Node "meta" [] [] ]))
    , test "opened" (testParse "<a>foo<br>bar</a>" (Node "a" [] [ Text "foo", Node "br" [] [], Text "bar" ]))
    ]


attributeTests : Test
attributeTests =
  suite "Attribute"
    [ test "basic" (testParse """<a href="example.com"></a>""" (Node "a" [("href", StringValue "example.com")] []))
    , test "basic" (testParse """<a href="example.com"/>""" (Node "a" [("href", StringValue "example.com")] []))
    , test "basic" (testParse """<input max=100 min = 10.5></input>""" (Node "input" [("max", NumberValue "100"), ("min", NumberValue "10.5")] []))
    , test "basic" (testParse """<input max=100 min = 10.5/>""" (Node "input" [("max", NumberValue "100"), ("min", NumberValue "10.5")] []))
    , test "basic" (testParse """<input disabled></input>""" (Node "input" [("disabled", NoValue)] []))
    , test "basic" (testParse """<input disabled/>""" (Node "input" [("disabled", NoValue)] []))
    ]


tests : Test
tests =
  suite "HtmlParser"
    [ textNodeTests
    , nodeTests
    , attributeTests
    ]


main : Program Never
main =
  runSuite tests
