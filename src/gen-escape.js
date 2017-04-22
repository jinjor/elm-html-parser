var fs = require('fs');

// this list is from http://www.theukwebdesigncompany.com/articles/entity-escape-characters.php
var escapeText = fs.readFileSync(__dirname + '/escape.txt', 'utf8');

var rows = escapeText.split('\n');
var codeToSymbol = rows.map(row => {
  var r = row.split('\t');
  var code = r[1] ? r[1].trim() : null;
  if(code) {
    var symbol = r[0].length > 1 ? ' ' : r[0] == '"' ? '\\"' : r[0] == '\\' ? '\\\\' : r[0];
    return `("${code}", "${symbol}")`;
  } else {
    return null;
  }
});
var code16ToSymbol = rows.map(row => {
  var r = row.split('\t');
  var code = r[1] ? r[1].trim() : null;
  if(code) {
    code = '&#x' + (+code.split('&#')[1].split(';')[0]).toString(16).toUpperCase() + ';';
    var symbol = r[0].length > 1 ? ' ' : r[0] == '"' ? '\\"' : r[0] == '\\' ? '\\\\' : r[0];
    return `("${code}", "${symbol}")`;
  } else {
    return null;
  }
});
var nameToSymbol = rows.map(row => {
  var r = row.split('\t');
  var name = r[2] ? r[2].trim() : null;
  if(name) {
    var symbol = r[0].length > 1 ? ' ' : r[0] == '"' ? '\\"' : r[0] == '\\' ? '\\\\' : r[0];
    return `("${name}", "${symbol}")`;
  } else {
    return null;
  }
});
var pairs = codeToSymbol.concat(code16ToSymbol).concat(nameToSymbol).filter(x => {
  return !!x;
}).map((x, index) => [x, index])
.reduce((s, x) => {
  return x[1] === 0 ? x[0] : x[1] % 20 === 0 ? s + '\n    ] ++\n    [ ' + x[0] : s + '\n    , ' + x[0];
}, '');

var source = `module Escape exposing (..)

import Dict exposing (..)


dict : Dict String String
dict =
  Dict.fromList
    [ ${pairs}
    ]

`;

fs.writeFileSync(__dirname + '/Escape.elm', source);
