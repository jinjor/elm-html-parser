# elm-html-parser

[![Build Status](https://travis-ci.org/jinjor/elm-html-parser.svg)](https://travis-ci.org/jinjor/elm-html-parser)

Parse HTML in Elm! ([DEMO](https://jinjor.github.io/elm-html-parser/))

```elm
parse "text" == [ Text "text" ]

parse "<h1>Hello<br>World</h1> "
  == [ Element "h1" [] [ Text "Hello", Element "br" [] [], Text "World" ] ]

parse """<a href="http://example.com">Example</a>"""
  == [ Element "a" [("href", "http://example.com")] [ Text "Example" ] ]
```

## LICENSE

BSD3
