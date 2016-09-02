module Tests exposing (..)

import String
import Combine as RawParser exposing (..)
import Internal.AST exposing (..)
import HtmlParser as HtmlParser exposing (..)
import ElmTest exposing (..)


contains : List String -> List String -> Result a b -> Assertion
contains tagList ngTagList r =
  case r of
    Ok ast ->
      if not <| List.all (\tagName -> String.contains tagName (toString ast)) tagList then
        ElmTest.fail ("Expected all of tags" ++ toString tagList ++ " are contained, but got " ++ toString ast)
      else if List.any (\tagName -> String.contains tagName (toString ast)) ngTagList then
        ElmTest.fail ("Expected any of tags" ++ toString ngTagList ++ " are not contained, but got " ++ toString ast)
      else
        ElmTest.pass

    e ->
      ElmTest.fail (toString e)


testParse : String -> AST -> Assertion
testParse s ast =
  assertEqual (Ok ast) (HtmlParser.parseOne s)


testParseComplex : List String -> List String -> String -> Assertion
testParseComplex tagList ngTagList s =
  contains tagList ngTagList (HtmlParser.parseOne s)


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
    , test "basic" (testParse "<A></A>" (Node "a" [] []))
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
    , test "start-only-tag" (testParse "<br>" (Node "br" [] []))
    , test "start-only-tag" (testParse "<BR>" (Node "br" [] []))
    , test "start-only-tag" (testParse "<a> <br> </a>" (Node "a" [] [ Text " ", Node "br" [] [], Text " " ]))
    , test "start-only-tag" (testParse "<a><br><br></a>" (Node "a" [] [ Node "br" [] [], Node "br" [] [] ]))
    , test "start-only-tag" (testParse "<a><br><img><hr><meta></a>" (Node "a" [] [ Node "br" [] [], Node "img" [] [], Node "hr" [] [], Node "meta" [] [] ]))
    , test "start-only-tag" (testParse "<a>foo<br>bar</a>" (Node "a" [] [ Text "foo", Node "br" [] [], Text "bar" ]))
    ]


optionalEndTagTests : Test
optionalEndTagTests =
  suite "optionalEndTag"
    [ test "ul" (testParse "<ul><li></li></ul>" (Node "ul" [] [ Node "li" [] [] ]))
    , test "ul" (testParse "<ul><li></ul>" (Node "ul" [] [ Node "li" [] [] ]))
    , test "ul" (testParse "<ul><li><li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "ul" (testParse "<ul><li></li><li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "ul" (testParse "<ul><li><li></li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "ul" (testParse "<ul><li><ul></ul></ul>" (Node "ul" [] [ Node "li" [] [ Node "ul" [] [] ] ]))
    , test "ul" (testParse "<ul> <li> <li> </ul>" (Node "ul" [] [ Text " ", Node "li" [] [ Text " " ], Node "li" [] [ Text " " ] ]))
    , test "ol" (testParse "<ol><li></ol>" (Node "ol" [] [ Node "li" [] [] ]))
    , test "tr" (testParse "<tr><td></tr>" (Node "tr" [] [ Node "td" [] [] ]))
    , test "tr" (testParse "<tr><td><td></tr>" (Node "tr" [] [ Node "td" [] [], Node "td" [] [] ]))
    , test "tr" (testParse "<tr><th></tr>" (Node "tr" [] [ Node "th" [] [] ]))
    , test "tr" (testParse "<tr><th><th></tr>" (Node "tr" [] [ Node "th" [] [], Node "th" [] [] ]))
    , test "tr" (testParse "<tr><th><td></tr>" (Node "tr" [] [ Node "th" [] [], Node "td" [] [] ]))
    , test "tr" (testParse "<tr><td><th></tr>" (Node "tr" [] [ Node "td" [] [], Node "th" [] [] ]))
    , test "tbody" (testParse "<tbody><tr><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><th><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "th" [] [], Node "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><td><tr><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "td" [] [] ], Node "tr" [] [ Node "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><th><td><tr><th><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "th" [] [], Node "td" [] [] ], Node "tr" [] [ Node "th" [] [], Node "td" [] [] ] ]))
    , test "table" (testParse "<table><caption></table>" (Node "table" [] [ Node "caption" [] [] ]))
    , test "table" (testParse "<table><caption><col></table>" (Node "table" [] [ Node "caption" [] [], Node "col" [] [] ]))
    , test "table" (testParse "<table><caption><colgroup><col></table>" (Node "table" [] [ Node "caption" [] [], Node "colgroup" [] [ Node "col" [] [] ] ]))
    , test "table" (testParse "<table><colgroup><col></table>" (Node "table" [] [ Node "colgroup" [] [ Node "col" [] [] ] ]))
    ]


scriptTests : Test
scriptTests =
  suite "Script"
    [ test "script" (testParse """<script></script>""" (Node "script" [] []))
    , test "script" (testParse """<script src="script.js">foo</script>""" (Node "script" [("src", StringValue "script.js")] [ Text "foo" ]))
    , test "script" (testParse """<script><!----></script>""" (Node "script" [] [ Comment "" ]))
    , test "script" (testParse """<script>a<!--</script><script>-->b</script>""" (Node "script" [] [ Text "a", Comment "</script><script>", Text "b" ]))
    , test "style" (testParse """<style>a<!--</style><style>-->b</style>""" (Node "style" [] [ Text "a", Comment "</style><style>", Text "b" ]))
    ]


commentTests : Test
commentTests =
  suite "Comment"
    [ test "basic" (testParse """<!---->""" (Comment ""))
    , test "basic" (testParse """<!--foo\t\r\n -->""" (Comment "foo\t\r\n "))
    , test "basic" (testParse """<!--<div></div>-->""" (Comment "<div></div>"))
    , test "basic" (testParse """<div><!--</div>--></div>""" (Node "div" [] [ Comment "</div>" ]))
    , test "basic" (testParse """<!--<!---->""" (Comment "<!--"))
    ]


attributeTests : Test
attributeTests =
  suite "Attribute"
    [ test "basic" (testParse """<a href="example.com"></a>""" (Node "a" [("href", StringValue "example.com")] []))
    , test "basic" (testParse """<a href='example.com'></a>""" (Node "a" [("href", StringValue "example.com")] []))
    , test "basic" (testParse """<a href=bare></a>""" (Node "a" [("href", StringValue "bare")] []))
    , test "basic" (testParse """<a href="example.com"/>""" (Node "a" [("href", StringValue "example.com")] []))
    , test "basic" (testParse """<input max=100 min = 10.5>""" (Node "input" [("max", NumberValue "100"), ("min", NumberValue "10.5")] []))
    , test "basic" (testParse """<input max=100 min = 10.5/>""" (Node "input" [("max", NumberValue "100"), ("min", NumberValue "10.5")] []))
    , test "basic" (testParse """<input disabled>""" (Node "input" [("disabled", NoValue)] []))
    , test "basic" (testParse """<input disabled/>""" (Node "input" [("disabled", NoValue)] []))
    ]


intergrationTests : Test
intergrationTests =
  suite "Integration"
    [ test "table" (testParseComplex ["table", "caption", "colgroup", "col", "thead", "tbody", "tr", "th", "td"] [] fullOmission)
    ]


fullOmission : String
fullOmission = """
  <table>
   <caption>37547 TEE Electric Powered Rail Car Train Functions (Abbreviated)
   <colgroup><col><col><col>
   <thead>
    <tr> <th>Function                              <th>Control Unit     <th>Central Station
   <tbody>
    <tr> <td>Headlights                            <td>✔                <td>✔
    <tr> <td>Interior Lights                       <td>✔                <td>✔
    <tr> <td>Electric locomotive operating sounds  <td>✔                <td>✔
    <tr> <td>Engineer's cab lighting               <td>                 <td>✔
    <tr> <td>Station Announcements - Swiss         <td>                 <td>✔
  </table>
  """


tests : Test
tests =
  suite "HtmlParser"
    [ textNodeTests
    , nodeTests
    , optionalEndTagTests
    , scriptTests
    , commentTests
    , attributeTests
    , intergrationTests
    ]


main : Program Never
main =
  runSuite tests
