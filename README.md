# elm-html-parser

[![Build Status](https://travis-ci.org/jinjor/elm-html-parser.svg)](https://travis-ci.org/jinjor/elm-html-parser)

Parse HTML in Elm!

```elm
parse "text" == [ Text "text" ]

parse "<h1>Hello<br>World</h1> "
  == [ Node "h1" [] [ Text "Hello", Node "br" [] [], Text "World" ] ]

parse "<a href="http://example.com">Example</a>"
  == [ Node "a" [("href", StringValue "http://example.com")] [ Text "Example" ] ]
```

## LICENSE

BSD3
