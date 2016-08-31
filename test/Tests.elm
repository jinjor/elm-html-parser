module Tests exposing (..)

import String
import Combine as RawParser exposing (..)
import HtmlParser.AST exposing (..)
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
  assertEqual (Ok ast) (HtmlParser.parseNode s)


testParseComplex : List String -> List String -> String -> Assertion
testParseComplex tagList ngTagList s =
  contains tagList ngTagList (HtmlParser.parseNode s)


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
    , test "basic" (testParse " <a></a> " (Node "a" [] []))
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
    , test "optional-end-tag" (testParse "<ul><li></li></ul>" (Node "ul" [] [ Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<ul><li></ul>" (Node "ul" [] [ Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<ul><li><li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<ul><li></li><li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<ul><li><li></li></ul>" (Node "ul" [] [ Node "li" [] [], Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<ul><li><ul></ul></ul>" (Node "ul" [] [ Node "li" [] [ Node "ul" [] [] ] ]))
    , test "optional-end-tag" (testParse "<ul> <li> <li> </ul>" (Node "ul" [] [ Text " ", Node "li" [] [ Text " " ], Node "li" [] [ Text " " ] ]))
    , test "optional-end-tag" (testParse "<ol><li></ol>" (Node "ol" [] [ Node "li" [] [] ]))
    , test "optional-end-tag" (testParse "<tbody><tr><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "td" [] [] ] ]))
    , test "optional-end-tag" (testParse "<tbody><tr><th><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "th" [] [], Node "td" [] [] ] ]))
    , test "optional-end-tag" (testParse "<tbody><tr><td><tr><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "td" [] [] ], Node "tr" [] [ Node "td" [] [] ] ]))
    , test "optional-end-tag" (testParse "<tbody><tr><th><td><tr><th><td></tbody>" (Node "tbody" [] [ Node "tr" [] [ Node "th" [] [], Node "td" [] [] ], Node "tr" [] [ Node "th" [] [], Node "td" [] [] ] ]))
    , test "optional-end-tag" (testParse "<table><tr><td><tr><td></table>" (Node "table" [] [ Node "tr" [] [ Node "td" [] [] ], Node "tr" [] [ Node "td" [] [] ] ]))
    -- , test "optional-end-tag" (testParse "<table><tr><!----><tr></table>" (Node "table" [] [ Node "tr" [] [], Comment "", Node "tr" [] [] ]))
    -- , test "optional-end-tag" (testParse "<table><tr><tr><!----></table>" (Node "table" [] [ Node "tr" [] [], Node "tr" [] [], Comment "" ]))
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
    , test "basic" (testParse """<meta http-equiv=Content-Type>""" (Node "meta" [("http-equiv", StringValue "Content-Type")] []))
    , test "basic" (testParse """<html xmlns:v="urn:schemas-microsoft-com:vml"></html>""" (Node "html" [("xmlns:v", StringValue "urn:schemas-microsoft-com:vml")] []))
    ]


intergrationTests : Test
intergrationTests =
  suite "Integration"
    [ test "table" (testParseComplex ["table", "caption", "colgroup", "col", "thead", "tbody", "tr", "th", "td"] [] fullOmission)
    , test "table" (testParseComplex ["body", "table", "col", "tr", "td"] [] clipboardFromExcel2013)
    , test "table" (testParseComplex ["body", "table", "col", "tr", "td"] [] clipboardFromOpenOfficeCalc)
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


clipboardFromExcel2013 : String
clipboardFromExcel2013 = """
  <body link="#0563C1" vlink="#954F72">

  <table border=0 cellpadding=0 cellspacing=0 width=216 style='border-collapse:
   collapse;width:162pt'>
  <!--StartFragment-->
   <col width=72 span=3 style='width:54pt'>
   <tr height=18 style='height:13.5pt'>
    <td height=18 align=right width=72 style='height:13.5pt;width:54pt'>1</td>
    <td align=right width=72 style='width:54pt'>2</td>
    <td align=right width=72 style='width:54pt'>3</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl69 align=right style='height:13.5pt'>2</td>
    <td class=xl66 align=right>3</td>
    <td align=right>4</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl65 align=right style='height:13.5pt'>3</td>
    <td class=xl66 align=right>4</td>
    <td class=xl65 align=right>5</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl65 align=right style='height:13.5pt'>4</td>
    <td class=xl66 align=right>5</td>
    <td class=xl65 align=right>6</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl67 align=right style='height:13.5pt'>5</td>
    <td class=xl67 align=right>6</td>
    <td class=xl67 align=right>7</td>
   </tr>
   <tr height=18 style='height:13.5pt'>
    <td height=18 class=xl68 align=right style='height:13.5pt'>6</td>
    <td class=xl68 align=right>7</td>
    <td class=xl68 align=right>8</td>
   </tr>
  <!--EndFragment-->
  </table>

  </body>
  """

clipboardFromOpenOfficeCalc : String
clipboardFromOpenOfficeCalc = """
  <BODY TEXT="#000000">
  <TABLE FRAME=VOID CELLSPACING=0 COLS=3 RULES=NONE BORDER=0>
    <COLGROUP><COL WIDTH=86><COL WIDTH=86><COL WIDTH=86></COLGROUP>
      <TBODY>
          <TR>
              <TD WIDTH=86 HEIGHT=19 ALIGN=RIGHT VALIGN=MIDDLE SDVAL="1" SDNUM="1041;"><FONT COLOR="#000000">1</FONT></TD>
              <TD WIDTH=86 ALIGN=CENTER VALIGN=MIDDLE SDVAL="2" SDNUM="1041;"><FONT COLOR="#000000">2</FONT></TD>
              <TD WIDTH=86 ALIGN=RIGHT VALIGN=MIDDLE SDVAL="3" SDNUM="1041;"><FONT COLOR="#000000">3</FONT></TD>
          </TR>
          <TR>
              <TD HEIGHT=19 ALIGN=LEFT VALIGN=MIDDLE SDVAL="2" SDNUM="1041;"><FONT COLOR="#000000">2</FONT></TD>
              <TD ALIGN=CENTER VALIGN=MIDDLE SDVAL="3" SDNUM="1041;"><B><FONT COLOR="#000000">3</FONT></B></TD>
              <TD ALIGN=RIGHT VALIGN=MIDDLE SDVAL="4" SDNUM="1041;"><B><FONT COLOR="#000000">4</FONT></B></TD>
          </TR>
          <TR>
              <TD HEIGHT=19 ALIGN=LEFT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="3" SDNUM="1041;"><FONT COLOR="#000000">3</FONT></TD>
              <TD ALIGN=CENTER VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="4" SDNUM="1041;"><B><FONT COLOR="#000000">4</FONT></B></TD>
              <TD ALIGN=RIGHT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="5" SDNUM="1041;"><B><FONT COLOR="#000000">5</FONT></B></TD>
          </TR>
          <TR>
              <TD HEIGHT=19 ALIGN=LEFT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="4" SDNUM="1041;"><FONT COLOR="#000000">4</FONT></TD>
              <TD ALIGN=CENTER VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="5" SDNUM="1041;"><FONT COLOR="#000000">5</FONT></TD>
              <TD ALIGN=RIGHT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="6" SDNUM="1041;"><FONT COLOR="#000000">6</FONT></TD>
          </TR>
          <TR>
              <TD HEIGHT=19 ALIGN=LEFT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="5" SDNUM="1041;"><FONT COLOR="#000000">5</FONT></TD>
              <TD ALIGN=CENTER VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="6" SDNUM="1041;"><FONT COLOR="#000000">6</FONT></TD>
              <TD ALIGN=RIGHT VALIGN=MIDDLE BGCOLOR="#FFFF00" SDVAL="7" SDNUM="1041;"><FONT COLOR="#000000">7</FONT></TD>
          </TR>
          <TR>
              <TD HEIGHT=19 ALIGN=RIGHT VALIGN=MIDDLE SDVAL="6" SDNUM="1041;"><FONT COLOR="#000000">6</FONT></TD>
              <TD ALIGN=CENTER VALIGN=MIDDLE SDVAL="7" SDNUM="1041;"><FONT COLOR="#000000">7</FONT></TD>
              <TD ALIGN=RIGHT VALIGN=MIDDLE SDVAL="8" SDNUM="1041;"><FONT COLOR="#000000">8</FONT></TD>
          </TR>
      </TBODY>
  </TABLE>
  </BODY>
  """


tests : Test
tests =
  suite "HtmlParser"
    [ textNodeTests
    , nodeTests
    , commentTests
    , attributeTests
    , intergrationTests
    ]


main : Program Never
main =
  runSuite tests
