CodeMirror.defineMode("elm", function() {

  function switchState(source, setState, f) {
    setState(f);
    return f(source, setState);
  }
  
  // These should all be Unicode extended, as per the Haskell 2010 report
  var smallRE = /[a-z_]/;
  var largeRE = /[A-Z]/;
  var digitRE = /[0-9]/;
  var hexitRE = /[0-9A-Fa-f]/;
  var octitRE = /[0-7]/;
  var idRE = /[a-z_A-Z0-9\']/;
  var symbolRE = /[-!#$%&*+.\/<=>?@\\^|~:\u03BB\u2192]/;
  var specialRE = /[(),;[\]`{}]/;
  var whiteCharRE = /[ \t\v\f]/; // newlines are handled in tokenizer
    
  function normal(source, setState) {
    if (source.eatWhile(whiteCharRE)) {
      return null;
    }
      
    var ch = source.next();
    if (specialRE.test(ch)) {
      if (ch == '{' && source.eat('-')) {
        var t = "comment";
        if (source.eat('#')) {
          t = "meta";
        }
        return switchState(source, setState, ncomment(t, 1));
      }
      if (ch == '['  &&
	  source.eat('m') && source.eat('a') && source.eat('r') && source.eat('k') && 
	  source.eat('d') && source.eat('o') && source.eat('w') && source.eat('n') &&
	  source.eat('|')) {
        setState(nmarkdown);
        return null;
      }
      return null;
    }
    
    if (ch == '\'') {
      if (source.eat('\\')) {
        source.next();  // should handle other escapes here
      }
      else {
        source.next();
      }
      if (source.eat('\'')) {
        return "string";
      }
      return "error";
    }
    
    if (ch == '"') {
      return switchState(source, setState, stringLiteral);
    }
      
    if (largeRE.test(ch)) {
      source.eatWhile(idRE);
      if (source.eat('.')) {
        return "qualifier";
      }
      return "variable-2";
    }
      
    if (smallRE.test(ch)) {
      var isDef = source.pos === 1;
      source.eatWhile(idRE);
      return isDef ? "variable-3" : "variable";
    }
      
    if (digitRE.test(ch)) {
      if (ch == '0') {
        if (source.eat(/[xX]/)) {
          source.eatWhile(hexitRE); // should require at least 1
          return "integer";
        }
        if (source.eat(/[oO]/)) {
          source.eatWhile(octitRE); // should require at least 1
          return "number";
        }
      }
      source.eatWhile(digitRE);
      var t = "number";
      if (source.eat('.')) {
        t = "number";
        source.eatWhile(digitRE); // should require at least 1
      }
      if (source.eat(/[eE]/)) {
        t = "number";
        source.eat(/[-+]/);
        source.eatWhile(digitRE); // should require at least 1
      }
      return t;
    }
      
    if (symbolRE.test(ch)) {
      if (ch == '-' && source.eat(/-/)) {
        source.eatWhile(/-/);
        if (!source.eat(symbolRE)) {
          source.skipToEnd();
          return "comment";
        }
      }
      var t = "variable";
      if (ch == ':') {
        t = "variable-2";
      }
      source.eatWhile(symbolRE);
      return t;    
    }
      
    return "error";
  }
    
  function ncomment(type, nest) {
    if (nest == 0) {
      return normal;
    }
    return function(source, setState) {
      var currNest = nest;
      while (!source.eol()) {
        var ch = source.next();
        if (ch == '{' && source.eat('-')) {
          ++currNest;
        }
        else if (ch == '-' && source.eat('}')) {
          --currNest;
          if (currNest == 0) {
            setState(normal);
            return type;
          }
        }
      }
      setState(ncomment(type, currNest));
      return type;
    }
  }

  function nmarkdown(source, setState) {
    while (!source.eol()) {
      var ch = source.next();
      if (ch == '|' && source.eat(']')) {
        setState(normal);
	return null;
      }
    }
    setState(nmarkdown);
    return "string";
  }
    
  function stringLiteral(source, setState) {
    while (!source.eol()) {
      var ch = source.next();
      if (ch == '"') {
        setState(normal);
        return "string";
      }
      if (ch == '\\') {
        if (source.eol() || source.eat(whiteCharRE)) {
          setState(stringGap);
          return "string";
        }
        if (source.eat('&')) {
        }
        else {
          source.next(); // should handle other escapes here
        }
      }
    }
    setState(normal);
    return "error";
  }
  
  function stringGap(source, setState) {
    if (source.eat('\\')) {
      return switchState(source, setState, stringLiteral);
    }
    source.next();
    setState(normal);
    return "error";
  }
  
  
  var wellKnownWords = (function() {
    var wkw = {};
    function setType(t) {
      return function () {
        for (var i = 0; i < arguments.length; i++)
          wkw[arguments[i]] = t;
      };
    }
    
    setType("keyword")(
      "as", "case", "class", "data", "default", "deriving", "do", "else", "export", "foreign",
      "hiding", "jsevent", "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
      "module", "newtype", "of", "open", "then", "type", "where", "_");
      
    setType("keyword")(
      "\.\.", "|", ":", "=", "\\", "\"", "->", "<-", "\u2192", "\u03BB");

    setType("builtin")(
      "$", "&&", "+", "++", "-", ".", "/", "/=", "<", "<=", "::",
      "==", ">", ">=", "^", "||", "*", "<~", "~", "|>", "<|", "<<", ">>");

    setType("builtin")(
      "Bool", "Char", "False", "Float", "GT", "Int", "Just", "LT",
      "Maybe", "Nothing", "String", "True");
      
    setType("builtin")(
      "abs", "acos", "acosh", "all", "and", "any",
      "asin", "asinh", "atan", "atan2", "atanh", "ceiling",
      "compare", "concat", "concatMap", "cos", "cosh", "curry",
      "div", "drop", "dropWhile", "either", "filter",
      "flip", "floor", "foldl", "foldl1", "foldr", "foldr1", "fst",
      "head", "id", "last", "length", "lift", "lift2", "lift3", "lift4",
      "lift5", "lift6", "lift7", "lift8", "log", "logBase", "lookup", "map",
      "max", "maximum", "maybe", "min", "minimum", "mod", "not", "or",
      "otherwise", "pi", "product", "quot", "rem", "reverse",
      "round", "scanl", "scanl1", "scanr", "scanr1",
      "show", "sin", "sinh", "snd", "sqrt", "sum",
      "tail", "take", "takeWhile", "tan", "tanh", "truncate", "uncurry",
      "unzip", "unzip3", "zip", "zip3", "zipWith", "zipWith3",
      "constant", "spacer", "container", "up", "down", "left", "right",
      "inward", "outward", "flow", "layers", "collage", "image", "fittedImage",
      "images", "video", "plainText", "text", "centeredText", "rightedText",
      "justifiedText", "above", "below", "beside", "width", "height", "size",
      "opacity", "color", "link", "widthOf", "heightOf", "sizeOf", "topLeft",
      "midLeft", "bottomLeft", "midTop", "middle", "midBottom", "topRight",
      "midRight", "bottomRight", "merge", "merges", "foldp", "count",
      "countIf", "average", "foldp1", "foldp'", "keepIf", "dropIf", "keepWhen",
      "dropWhen", "dropRepeats", "sampleOn", "red", "green", "blue", "cyan",
      "yellow", "magenta", "black", "white", "grey", "gray", "rgb", "rgba",
      "complement", "hsv", "hsva");
      
    return wkw;
  })();
    
  
  
  return {
    startState: function ()  { return { f: normal }; },
    copyState:  function (s) { return { f: s.f }; },
    
    token: function(stream, state) {
      var t = state.f(stream, function(s) { state.f = s; });
      var w = stream.current();
      return (wellKnownWords.hasOwnProperty(w)) ? wellKnownWords[w] : t;
    }
  };

});
