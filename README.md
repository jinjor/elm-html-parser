# elm-html-parser

[![Build Status](https://travis-ci.org/jinjor/elm-html-parser.svg)](https://travis-ci.org/jinjor/elm-html-parser)

Parse HTML in Elm! ([DEMO](https://jinjor.github.io/elm-html-parser/))

## Parse

```elm
import HtmlParser as HtmlParser exposing (..)

parse "text" == [ Text "text" ]

parse "<h1>Hello<br>World</h1>"
  == [ Element "h1" [] [ Text "Hello", Element "br" [] [], Text "World" ] ]

parse """<a href="http://example.com">Example</a>"""
  == [ Element "a" [("href", "http://example.com")] [ Text "Example" ] ]
```

## Query

```elm
import HtmlParser exposing (..)
import HtmlParser.Util exposing (..)

table = """
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
  <!--EndFragment-->
  </table>
"""

( parse table
  |> getElementsByTagName "tr"
  |> mapElements
    (\_ _ innerTr ->
      innerTr
        |> mapElements (\_ _ innerTd -> textContent innerTd)
        |> String.join "\t"
        |> String.trim
    )
  |> String.join "\n"
) == "1\t2\t3\n2\t3\t4"
```

## LICENSE

BSD3
