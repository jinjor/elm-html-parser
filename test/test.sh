elm-make Demo.elm --output ../docs/index.html &&
elm-make Tests.elm --output tests.js &&
node tests.js
