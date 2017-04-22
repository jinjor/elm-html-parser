module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Dict
import Combine as RawParser exposing (..)
import HtmlParser as HtmlParser exposing (..)
import HtmlParser.Util exposing (..)


testParseAll : String -> List Node -> (() -> Expectation)
testParseAll s astList = \_ ->
  Expect.equal astList (HtmlParser.parse s)


testParse : String -> Node -> (() -> Expectation)
testParse s ast =
  testParseAll s [ast]


testParseComplex : (List Node -> Bool) -> String -> (() -> Expectation)
testParseComplex f s = \_ ->
  Expect.true "" (f (HtmlParser.parse s))


textNodeTests : Test
textNodeTests =
  describe "TextNode"
    [ test "basic" (testParse "1" (Text "1"))
    , test "basic" (testParse "a" (Text "a"))
    , test "basic" (testParse "1a" (Text "1a"))
    , test "basic" (testParse "^" (Text "^"))
    , test "decode1" (testParse "&" (Text "&"))
    , test "decode2" (testParse "&amp;" (Text "&"))
    , test "decode3" (testParse "&lt;" (Text "<"))
    , test "decode4" (testParse "&gt;" (Text ">"))
    , test "decode5" (testParse "&nbsp;" (Text " "))
    , test "decode6" (testParse "&apos;" (Text "'"))
    , test "decode7" (testParse "&#38;" (Text "&"))
    , test "decode8" (testParse "&#x26;" (Text "&"))
    , test "decode9" (testParse "&#x3E;" (Text ">"))
    , test "decodeA" (testParse "&#383;" (Text "ſ"))
    , test "decodeB" (testParse "&nbsp;" (Text " "))
    , test "decodeC" (testParse "&nbsp;&nbsp;" (Text "  "))
    , test "decodeD" (testParse "a&nbsp;b" (Text "a b"))
    , test "decodeE" (testParse "a&nbsp;&nbsp;b" (Text "a  b"))
    , test "decodeF" (testParse """<img alt="&lt;">""" (Element "img" [("alt", "<")] []))
    ]


nodeTests : Test
nodeTests =
  describe "Node"
    [ test "basic" (testParse "<a></a>" (Element "a" [] []))
    , test "basic" (testParse "<a></a >" (Element "a" [] []))
    , test "basic" (testParse "<A></A >" (Element "a" [] []))
    , test "basic" (testParseAll " <a></a> " [Text " ", Element "a" [] [], Text " "])
    , test "basic" (testParseAll "a<a></a>b" [Text "a", Element "a" [] [], Text "b"])
    , test "basic" (testParse "<A></A>" (Element "a" [] []))
    , test "basic" (testParse "<a>a</a>" (Element "a" [] [ Text "a" ]))
    , test "basic" (testParse "<a> a </a>" (Element "a" [] [ Text " a " ]))
    , test "basic" (testParse "<a />" (Element "a" [] []))
    , test "basic" (testParse "<br />" (Element "br" [] []))
    , test "basic" (testParse "<a><a></a></a>" (Element "a" [] [ Element "a" [] [] ]))
    , test "basic" (testParse "<a> <a> </a> </a>" (Element "a" [] [ Text " ", Element "a" [] [ Text " " ], Text " " ]))
    , test "basic" (testParse "<a><a/></a>" (Element "a" [] [ Element "a" [] [] ]))
    , test "basic" (testParse "<a> <br /> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
    , test "basic" (testParse "<a><a></a><a></a></a>" (Element "a" [] [ Element "a" [] [], Element "a" [] [] ]))
    , test "basic" (testParse "<a><a><a></a></a></a>" (Element "a" [] [ Element "a" [] [ Element "a" [] [] ] ]))
    , test "basic" (testParse "<a><a></a><b></b></a>" (Element "a" [] [ Element "a" [] [], Element "b" [] [] ]))
    , test "basic" (testParse "<h1></h1>" (Element "h1" [] []))
    , test "basic" (testParse "<custom-element></custom-element>" (Element "custom-element" [] []))
    , test "start-only-tag" (testParse "<br>" (Element "br" [] []))
    , test "start-only-tag" (testParse "<BR>" (Element "br" [] []))
    , test "start-only-tag" (testParse "<br >" (Element "br" [] []))
    , test "start-only-tag" (testParse "<BR >" (Element "br" [] []))
    , test "start-only-tag" (testParse "<a> <br> </a>" (Element "a" [] [ Text " ", Element "br" [] [], Text " " ]))
    , test "start-only-tag" (testParse "<a><br><br></a>" (Element "a" [] [ Element "br" [] [], Element "br" [] [] ]))
    , test "start-only-tag" (testParse "<a><br><img><hr><meta></a>" (Element "a" [] [ Element "br" [] [], Element "img" [] [], Element "hr" [] [], Element "meta" [] [] ]))
    , test "start-only-tag" (testParse "<a>foo<br>bar</a>" (Element "a" [] [ Text "foo", Element "br" [] [], Text "bar" ]))
    ]


-- see https://html.spec.whatwg.org/multipage/syntax.html#optional-tags
optionalEndTagTests : Test
optionalEndTagTests =
  describe "OptionalEndTag"
    [ test "ul" (testParse "<ul><li></li></ul>" (Element "ul" [] [ Element "li" [] [] ]))
    , test "ul" (testParse "<ul><li></ul>" (Element "ul" [] [ Element "li" [] [] ]))
    , test "ul" (testParse "<ul><li><li></ul>" (Element "ul" [] [ Element "li" [] [], Element "li" [] [] ]))
    , test "ul" (testParse "<ul><li></li><li></ul>" (Element "ul" [] [ Element "li" [] [], Element "li" [] [] ]))
    , test "ul" (testParse "<ul><li><li></li></ul>" (Element "ul" [] [ Element "li" [] [], Element "li" [] [] ]))
    , test "ul" (testParse "<ul><li><ul></ul></ul>" (Element "ul" [] [ Element "li" [] [ Element "ul" [] [] ] ]))
    , test "ul" (testParse "<ul> <li> <li> </ul>" (Element "ul" [] [ Text " ", Element "li" [] [ Text " " ], Element "li" [] [ Text " " ] ]))
    , test "ol" (testParse "<ol><li></ol>" (Element "ol" [] [ Element "li" [] [] ]))
    , test "tr" (testParse "<tr><td></tr>" (Element "tr" [] [ Element "td" [] [] ]))
    , test "tr" (testParse "<tr><td><td></tr>" (Element "tr" [] [ Element "td" [] [], Element "td" [] [] ]))
    , test "tr" (testParse "<tr><th></tr>" (Element "tr" [] [ Element "th" [] [] ]))
    , test "tr" (testParse "<tr><th><th></tr>" (Element "tr" [] [ Element "th" [] [], Element "th" [] [] ]))
    , test "tr" (testParse "<tr><th><td></tr>" (Element "tr" [] [ Element "th" [] [], Element "td" [] [] ]))
    , test "tr" (testParse "<tr><td><th></tr>" (Element "tr" [] [ Element "td" [] [], Element "th" [] [] ]))
    , test "tbody" (testParse "<tbody><tr><td></tbody>" (Element "tbody" [] [ Element "tr" [] [ Element "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><th><td></tbody>" (Element "tbody" [] [ Element "tr" [] [ Element "th" [] [], Element "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><td><tr><td></tbody>" (Element "tbody" [] [ Element "tr" [] [ Element "td" [] [] ], Element "tr" [] [ Element "td" [] [] ] ]))
    , test "tbody" (testParse "<tbody><tr><th><td><tr><th><td></tbody>" (Element "tbody" [] [ Element "tr" [] [ Element "th" [] [], Element "td" [] [] ], Element "tr" [] [ Element "th" [] [], Element "td" [] [] ] ]))
    , test "table" (testParse "<table><caption></table>" (Element "table" [] [ Element "caption" [] [] ]))
    , test "table" (testParse "<table><caption><col></table>" (Element "table" [] [ Element "caption" [] [], Element "col" [] [] ]))
    , test "table" (testParse "<table><caption><colgroup><col></table>" (Element "table" [] [ Element "caption" [] [], Element "colgroup" [] [ Element "col" [] [] ] ]))
    , test "table" (testParse "<table><colgroup><col></table>" (Element "table" [] [ Element "colgroup" [] [ Element "col" [] [] ] ]))
    , test "table" (testParse "<table><thead><tr><th><tbody></table>" (Element "table" [] [ Element "thead" [] [ Element "tr" [] [ Element "th" [] [] ] ], Element "tbody" [] [] ]))
    , test "html" (testParse "<html>a" (Element "html" [] [ Text "a" ]))
    , test "html" (testParse "<html>a<head>b<body>c" (Element "html" [] [ Text "a", Element "head" [] [ Text "b" ], Element "body" [] [ Text "c" ] ]))
    ]


scriptTests : Test
scriptTests =
  describe "Script"
    [ test "script" (testParse """<script></script>""" (Element "script" [] []))
    , test "script" (testParse """<SCRIPT></SCRIPT>""" (Element "script" [] []))
    , test "script" (testParse """<script src="script.js">foo</script>""" (Element "script" [("src", "script.js")] [ Text "foo" ]))
    , test "script" (testParse """<script>var a = 0 < 1; b = 1 > 0;</script>""" (Element "script" [] [ Text "var a = 0 < 1; b = 1 > 0;" ]))
    , test "script" (testParse """<script><!----></script>""" (Element "script" [] [ Comment "" ]))
    , test "script" (testParse """<script>a<!--</script><script>-->b</script>""" (Element "script" [] [ Text "a", Comment "</script><script>", Text "b" ]))
    , test "style" (testParse """<style>a<!--</style><style>-->b</style>""" (Element "style" [] [ Text "a", Comment "</style><style>", Text "b" ]))
    ]


commentTests : Test
commentTests =
  describe "Comment"
    [ test "basic" (testParse """<!---->""" (Comment ""))
    , test "basic" (testParse """<!--foo\t\r\n -->""" (Comment "foo\t\r\n "))
    , test "basic" (testParse """<!--<div></div>-->""" (Comment "<div></div>"))
    , test "basic" (testParse """<div><!--</div>--></div>""" (Element "div" [] [ Comment "</div>" ]))
    , test "basic" (testParse """<!--<!---->""" (Comment "<!--"))
    ]


attributeTests : Test
attributeTests =
  describe "Attribute"
    [ test "basic" (testParse """<a href="example.com"></a>""" (Element "a" [("href", "example.com")] []))
    , test "basic" (testParse """<a href='example.com'></a>""" (Element "a" [("href", "example.com")] []))
    , test "basic" (testParse """<a href=example.com></a>""" (Element "a" [("href", "example.com")] []))
    , test "basic" (testParse """<a HREF=example.com></a>""" (Element "a" [("href", "example.com")] []))
    , test "basic" (testParse """<a href=bare></a>""" (Element "a" [("href", "bare")] []))
    , test "basic" (testParse """<a href="example.com"/>""" (Element "a" [("href", "example.com")] []))
    , test "basic" (testParse """<a href="example.com?a=b&amp;c=d"></a>""" (Element "a" [("href", "example.com?a=b&c=d")] []))
    , test "basic" (testParse """<a href="example.com?a=b&c=d"></a>""" (Element "a" [("href", "example.com?a=b&c=d")] []))
    , test "basic" (testParse """<input max=100 min = 10.5>""" (Element "input" [("max", "100"), ("min", "10.5")] []))
    , test "basic" (testParse """<input max=100 min = 10.5 />""" (Element "input" [("max", "100"), ("min", "10.5")] []))
    , test "basic" (testParse """<input disabled>""" (Element "input" [("disabled", "")] []))
    , test "basic" (testParse """<input DISABLED>""" (Element "input" [("disabled", "")] []))
    , test "basic" (testParse """<input disabled />""" (Element "input" [("disabled", "")] []))
    , test "basic" (testParse """<meta http-equiv=Content-Type>""" (Element "meta" [("http-equiv", "Content-Type")] []))
    , test "basic" (testParse """<input data-foo2="a">""" (Element "input" [("data-foo2", "a")] []))
    , test "basic" (testParse """<html xmlns:v="urn:schemas-microsoft-com:vml"></html>""" (Element "html" [("xmlns:v", "urn:schemas-microsoft-com:vml")] []))
    , test "basic" (testParse """<link rel=stylesheet\nhref="">""" (Element "link" [("rel", "stylesheet"), ("href", "")] []))
    ]

intergrationTests : Test
intergrationTests =
  describe "Integration"
    [ test "table" (testParseComplex (\nodes -> (List.length <| getElementsByTagName "td" nodes) == 15) fullOmission)
    , test "table" (testParseComplex (\nodes -> (List.length <| getElementsByTagName "td" nodes) == 18) clipboardFromExcel2013)
    , test "table" (testParseComplex (\nodes -> (List.length <| getElementsByTagName "td" nodes) == 18) clipboardFromOpenOfficeCalc)
    , test "query" (\_ -> Expect.equal "1\t2\t3\n2\t3\t4\n3\t4\t5\n4\t5\t6\n5\t6\t7\n6\t7\t8"
      ( HtmlParser.parse clipboardFromOpenOfficeCalc
          |> getElementsByTagName "tr"
          |> mapElements
            (\_ _ innerTr ->
              innerTr
                |> mapElements (\_ _ innerTd -> textContent innerTd)
                |> String.join "\t"
                |> String.trim
            )
          |> String.join "\n"
        )
      )
    , test "query" (\_ -> Expect.equal
      ["Headlights", "Interior Lights", "Electric locomotive operating sounds"]
      ( HtmlParser.parse fullOmission
        |> getElementsByTagName "tbody"
        |> getElementsByTagName "tr"
        |> filterMapElements
          (\_ _ innerTr ->
            case filterElements (\tagName _ _ -> tagName == "td") innerTr of
              [td1, td2, td3] ->
                if String.trim (textContent [td2]) == "✔" &&
                   String.trim (textContent [td3]) == "✔" then
                  Just (String.trim (textContent [td1]))
                else
                  Nothing

              _ ->
                Nothing
          )
        )
      )
    , test "query" (\_ -> Expect.equal
      (Just ("0", "0", "0", "216"))
      ( HtmlParser.parse clipboardFromExcel2013
          |> getElementsByTagName "table"
          |> (\nodes ->
            case nodes of
              Element "table" attrs children :: _ ->
                -- need a combinator?
                getValue "border" attrs |> Maybe.andThen (\a1 ->
                getValue "cellpadding" attrs |> Maybe.andThen (\a2 ->
                getValue "cellspacing" attrs |> Maybe.andThen (\a3 ->
                getValue "width" attrs |> Maybe.andThen (\a4 ->
                Just (a1, a2, a3, a4)
                ))))

              _ ->
                Nothing
            )
        )
      )
    ]


testInvalid : String -> String -> (() -> Expectation)
testInvalid included s = \_ ->
  Expect.true "" <| String.contains included <| toString <| HtmlParser.parse s


-- This tests is NOT from spec. You cannot expect this behavior as proper one.
invalidTests : Test
invalidTests =
  describe "Invalid"
    [ test "basic" (testInvalid "aaa" "<div>aaa")
    , test "basic" (testInvalid "aaa" "<div>aaa</br>bbb</div>")
    , test "basic" (testInvalid "bbb" "<div>aaa</br>bbb</div>")
    , test "basic" (testInvalid "aaa" "<div>aaa</br>bbb")
    , test "basic" (testInvalid "bbb" "<div>aaa</br>bbb")
    , test "basic" (testInvalid "aaa" "<input>aaa</input>")
    , test "basic" (testInvalid "aaa" "<div>aaa<input>bbb</input>ccc</div>")
    , test "basic" (testInvalid "bbb" "<div>aaa<input>bbb</input>ccc</div>")
    , test "basic" (testInvalid "ccc" "<div>aaa<input>bbb</input>ccc</div>")
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


parserTests : Test
parserTests =
  describe "Parser"
    [ textNodeTests
    , nodeTests
    , optionalEndTagTests
    , scriptTests
    , commentTests
    , attributeTests
    , intergrationTests
    , invalidTests
    ]


testSearch : List String -> (List Node -> List Node) -> String -> (() -> Expectation)
testSearch idList f s = \_ ->
  Expect.equal
    idList
    ( filterMapElements
        (\_ attrs _ -> getId attrs)
        (f (HtmlParser.parse s))
    )


utilTests : Test
utilTests =
  describe "Util"
    [ test "tag" (testSearch [] (getElementsByTagName "input") "<img id=1>")
    , test "tag" (testSearch ["1"] (getElementsByTagName "img") "<img id=1>")
    , test "tag" (testSearch ["1", "2"] (getElementsByTagName "img") "<img id=1><img id=2>")
    , test "tag" (testSearch ["0", "1", "2", "3", "4"] (getElementsByTagName "a")
      "<a id=0> <a id=1> </a> <a id=2> a </a> </a> <a id=3> b <div id=9> </div> <a id=4> </a> c </a>"
      )
    , test "class" (testSearch [] (getElementsByClassName ["c"]) "<img class=0 id=1>")
    , test "class" (testSearch ["1"] (getElementsByClassName ["c"]) "<img class=c id=1>")
    , test "class" (testSearch ["1", "2"] (getElementsByClassName ["c"]) "<img class=c id=1><img class=c id=2>")
    , test "class" (testSearch ["1", "5"] (getElementsByClassName ["c1", "c2"])
        "<img class='c2 c1' id=1><img class='c1' id=2><img class='c2' id=3><img class='c2 c3' id=4><img class='c2 c3 c1' id=5>"
      )
    , test "class" (testSearch ["0", "1", "2", "3", "4"] (getElementsByClassName ["c"])
      "<a class=c id=0><input class=c id=1><a class=c id=2></a></a><a class=c id=3><a id=9></a><input class=c id=4></a>"
      )
    , test "id" (testSearch ["1"] (getElementById "1") "<img id=1>")
    , test "id" (testSearch [] (getElementById "1") "<img id=0>")
    , test "createIdDict" (testParseComplex (\nodes ->
      case Dict.get "1" (createIdDict nodes) of
        Just _ -> False
        Nothing -> True
      ) "<img id=0><img id=2>")
    , test "createIdDict" (testParseComplex (\nodes ->
      case Dict.get "1" (createIdDict nodes) of
        Just [x,y] -> True
        _ -> False
      ) "<div> <img id=0> <img id=1> <img id=3> <img id=1> </div>")
    , test "createTagDict" (testParseComplex (\nodes ->
      case Dict.get "img" (createTagDict nodes) of
        Just nodes ->
          (filterMapElements (\_ attrs _ -> getId attrs) nodes) == ["0", "1"]
        Nothing -> False
      ) "<img id=0><img id=1>")
    , test "createTagDict" (testParseComplex (\nodes ->
      case Dict.get "a" (createTagDict nodes) of
        Just nodes ->
          (filterMapElements (\_ attrs _ -> getId attrs) nodes) == ["0", "1", "2", "3", "4"]
        Nothing -> False
      ) "<a id=0> <a id=1> </a> <a id=2> a </a> </a> <a id=3> b <div id=9> </div> <a id=4> </a> c </a>")
    , test "createClassDict" (testParseComplex (\nodes ->
      case Dict.get "c" (createClassDict nodes) of
        Just nodes ->
          (filterMapElements (\_ attrs _ -> getId attrs) nodes) == ["0", "1"]
        Nothing -> False
      ) "<input class=c id=0><img class=c id=1>")
    , test "createClassDict" (testParseComplex (\nodes ->
      case Dict.get "c" (createClassDict nodes) of
        Just nodes ->
          (filterMapElements (\_ attrs _ -> getId attrs) nodes) == ["0", "1", "2", "3", "4"]
        Nothing -> False
      ) "<a class=c id=0><input class=c id=1><a class=c id=2></a></a><a class=c id=3><a id=9></a><input class=c id=4></a>")
    , test "attr" (\_ -> Expect.true "" <| getId [("id", "foo")] == Just "foo")
    , test "attr" (\_ -> Expect.true "" <| getId [("id", "FOO")] == Just "FOO")
    , test "attr" (\_ -> Expect.true "" <| getId [("id", " foo ")] == Just " foo ")
    , test "attr" (\_ -> Expect.true "" <| getClassList [("class", "foo bar baz")] == ["foo", "bar", "baz"])
    , test "attr" (\_ -> Expect.true "" <| getClassList [("class", "FOO")] == ["FOO"])
    , test "attr" (\_ -> Expect.true "" <| getClassList [("class", "   foo    bar   ")] == ["foo", "bar"])
    , test "textContent" (testParseComplex (\nodes ->
      textContent nodes == "This is some text"
      ) "<div>This is <span>some</span> text</div>")
    , test "textContent" (testParseComplex (\nodes ->
      textContent nodes == "This is  text"
      ) "<div>This is <!--some--> text</div>")
    ]


all : Test
all =
  describe "HtmlParser"
    [ parserTests
    , utilTests
    ]
