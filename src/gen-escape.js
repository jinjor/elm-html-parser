var fs = require('fs');

// this list is from https://dev.w3.org/html5/html-author/charref
// $$('tr').map(tr => tr.children[0].textContent.substring(1) + '__' + [1,2,3].map(i => tr.children[i].textContent).join('__')).join(',\n')

var escapeText = fs.readFileSync(__dirname + '/escape.txt', 'utf8');

var rows = escapeText.split(',\n');
var ent = [];

rows.forEach(row => {
  var r = row.split('__');
  var char = r[0] == '\n' ? '\\n' : r[0] == '"' ? '\\"' : r[0] == '\\' ? '\\\\' : r[0];
  var names = r[1].split(' ');
  names.forEach(name => {
    ent.push(`("${name}", "${char}")`);
  });
});

var pairs =
  ent
  .map((x, index) => [x, index])
  .reduce((s, x) => {
    return x[1] === 0 ? x[0] : x[1] % 40 === 0 ? s + '\n    ] ++\n    [ ' + x[0] : s + '\n    , ' + x[0];
  }, '');

var source = `module Escape exposing (..)

import Dict exposing (..)


dict : Dict String String
dict =
  Dict.fromList <|
    [ ${pairs}
    ]

`;

fs.writeFileSync(__dirname + '/Escape.elm', source);
