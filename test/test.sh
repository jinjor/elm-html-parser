node ../src/gen-escape.js
elm-make Demo.elm --output ../docs/script.js &&
elm-make Tests.elm --output tests.js &&
node tests.js
