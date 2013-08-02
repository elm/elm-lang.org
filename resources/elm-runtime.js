Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};
Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {};
Elm = {}; Elm.Native = {}; Elm.Native.Graphics = {};
Elm.Graphics = {}; ElmRuntime = {}; ElmRuntime.Render = {}

Elm.Native.Basics = function(elm) {
  'use strict';
  if (elm.Native.Basics) return elm.Native.Basics;

  var JS = Elm.Native.JavaScript(elm);
  // var Maybe = Elm.Maybe(elm);
  var Utils = Elm.Native.Utils(elm);
  //var Char = Elm.Char(elm);

  function div(a,b) { return (a/b)|0; }
  function rem(a,b) { return a % b; }
  var mod = Utils.mod;
  function abs(x) { return x < 0 ? -x : x; }
  function logBase(base,n) { return Math.log(n) / Math.log(base); }
  function min(a,b) { return Utils.cmp(a,b) < 0 ? a : b; }
  function max(a,b) { return Utils.cmp(a,b) > 0 ? a : b; }
  function clamp(lo,hi,n) {
      return Utils.cmp(n,lo) < 0 ? lo : Utils.cmp(n,hi) > 0 ? hi : n; }
  function xor(a,b) { return a !== b; }
  function not(b) { return !b; }

  function truncate(n) { return n|0; }

  function curry(f,a,b) { return f(Utils.Tuple2(a,b)); }
  function uncurry(f,v) { return A2(f,v._0,v._1); }
  function fst(t) { return t._0; }
  function snd(t) { return t._1; }
/*
  function readInt(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    for (var i = start; i < len; ++i) {
      if (!Char.isDigit(s[i])) { return Maybe.Nothing; }
    }
    return Maybe.Just(parseInt(s, 10));
  }

  function readFloat(str) {
    var s = JS.fromString(str);
    var len = s.length;
    if (len === 0) { return Maybe.Nothing; }
    var start = 0;
    if (s[0] == '-') {
      if (len === 1) { return Maybe.Nothing; }
      start = 1;
    }
    var dotCount = 0;
    for (var i = start; i < len; ++i) {
      if (Char.isDigit(s[i])) { continue; }
      if (s[i] === '.') {
        dotCount += 1;
        if (dotCount <= 1) { continue; }
      }
      return Maybe.Nothing;
    }
    return Maybe.Just(parseFloat(s));
  }
*/
  var basics = {
      div:F2(div),
      rem:F2(rem),
      mod:mod,

      pi:Math.PI,
      e:Math.e,
      cos:Math.cos,
      sin:Math.sin,
      tan:Math.tan,
      acos:Math.acos,
      asin:Math.asin,
      atan:Math.atan,
      atan2:F2(Math.atan2),

      sqrt:Math.sqrt,
      abs:abs,
      logBase:F2(logBase),
      min:F2(min),
      max:F2(max),
      clamp:F3(clamp),
      compare:Utils.compare,

      xor:F2(xor),
      not:not,

      truncate:truncate,
      ceiling:Math.ceil,
      floor:Math.floor,
      round:Math.round,
      toFloat:function(x) { return x; },

      //readInt:readInt,
      //readFloat:readFloat,

      curry:F3(curry),
      uncurry:F2(uncurry),
      fst:fst,
      snd:snd
  };

  return elm.Native.Basics = basics;
};

Elm.Native.Char = function(elm) {
 'use strict';

 elm.Native = elm.Native || {};
 if (elm.Native.Char) return elm.Native.Char;

 function isBetween(lo,hi) { return function(chr) {
	 var c = chr.charCodeAt(0);
	 return lo <= c && c <= hi;
     };
 }
 var isDigit = isBetween('0'.charCodeAt(0),'9'.charCodeAt(0));
 var chk1 = isBetween('a'.charCodeAt(0),'f'.charCodeAt(0));
 var chk2 = isBetween('A'.charCodeAt(0),'F'.charCodeAt(0));

 return elm.Native.Char = {
     fromCode : function(c) { return String.fromCharCode(c); },
     toCode   : function(c) { return c.charCodeAt(0); },
     toUpper  : function(c) { return c.toUpperCase(); },
     toLower  : function(c) { return c.toLowerCase(); },
     toLocaleUpper : function(c) { return c.toLocaleUpperCase(); },
     toLocaleLower : function(c) { return c.toLocaleLowerCase(); },
     isLower    : isBetween('a'.charCodeAt(0),'z'.charCodeAt(0)),
     isUpper    : isBetween('A'.charCodeAt(0),'Z'.charCodeAt(0)),
     isDigit    : isDigit,
     isOctDigit : isBetween('0'.charCodeAt(0),'7'.charCodeAt(0)),
     isHexDigit : function(c) { return isDigit(c) || chk1(c) || chk2(c); }
 };

};


Elm.Native.Color = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 if (elm.Native.Color) return elm.Native.Color;

 var Utils = Elm.Native.Utils(elm);

 function complement(rgb) {
     var hsv = toHSV(rgb);
     hsv.hue = (hsv.hue + 180) % 360;
     return toRGB(hsv);
 }

 function hsva(h,s,v,a) {
     var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
     var clr = toRGB({hue:degree, saturation:s, value:v});
     clr._3 = a;
     return clr;
 }

 function hsv(h,s,v) {
     var degree = A2(Utils.mod, h * 180 / Math.PI, 360);
     return toRGB({hue:degree, saturation:s, value:v});
 }

 function toHSV(rgb) {
  var hsv = {};
  var r = rgb._0 / 255.0, g = rgb._1 / 255.0, b = rgb._2 / 255.0;
  var M = Math.max(r,g,b);
  var m = Math.min(r,g,b);
  var c = M - m;

  var h = 0;
       if (c === 0) { h = 0; }
  else if (M === r) { h = ((g - b) / c) % 6; }
  else if (M === g) { h = ((b - r) / c) + 2; }
  else if (M === b) { h = ((r - g) / c) + 4; }
  h *= 60;

  return { value : M, hue : h, saturation : (M === 0 ? 0 : c / M) };
 }

 function between(lo,hi,x) { return lo <= x && x < hi; }
 function norm(n) { return Math.round(n*255); }

 function toRGB(hsv) {
  var c = hsv.value * hsv.saturation;
  var hue = hsv.hue / 60;
  var x = c * (1 - Math.abs((hue % 2) - 1));
  var r = 0, g = 0, b = 0;
       if (between(0,1,hue)) { r = c; g = x; b = 0; }
  else if (between(1,2,hue)) { r = x; g = c; b = 0; }
  else if (between(2,3,hue)) { r = 0; g = c; b = x; }
  else if (between(3,4,hue)) { r = 0; g = x; b = c; }
  else if (between(4,5,hue)) { r = x; g = 0; b = c; }
  else if (between(5,6,hue)) { r = c; g = 0; b = x; }

  var m = hsv.value - c;
  return { ctor:"Color", _0:norm(r+m), _1:norm(g+m), _2:norm(b+m), _3:1 };
 }

 return elm.Native.Color = {
    hsva:F4(hsva),
    hsv:F3(hsv),
    complement:complement
 };

};
Elm.Native.Date = function(elm) {
 'use strict';

 elm.Native = elm.Native || {};
 if (elm.Native.Date) return elm.Native.Date;

 var JS = Elm.JavaScript(elm);
 var Maybe = Elm.Maybe(elm);

 function dateNow() { return new window.Date; }
 function readDate(str) {
     var d = new window.Date(JS.fromString(str));
     if (isNaN(d.getTime())) return Maybe.Nothing;
     return Maybe.Just(d);
 }

 var dayTable = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
 var monthTable = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
		   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]; 

 return elm.Native.Date = {
     read    : readDate,
     year    : function(d) { return d.getFullYear(); },
     month   : function(d) { return { ctor:monthTable[d.getMonth()] }; },
     day     : function(d) { return d.getDate(); },
     hour    : function(d) { return d.getHours(); },
     minute  : function(d) { return d.getMinutes(); },
     second  : function(d) { return d.getSeconds(); },
     toTime  : function(d) { return d.getTime(); },
     dayOfWeek : function(d) { return { ctor:dayTable[d.getDay()] }; }
 };

};

Elm.Native.Error = function(elm) {
    'use strict';
    elm.Native = elm.Native || {};
    if (elm.Native.Error) return elm.Native.Error;

    var fromString = Elm.Native.JavaScript(elm).fromString;

    function Case(span) { 
	var msg = 'Non-exhaustive pattern match in case expression'
	throw new Error(msg + " (" + span + ")")
    }

    function If(span) { 
	var msg = 'Non-exhaustive pattern match in multi-way-if expression'
	throw new Error(msg + " (" + span + ")")
    }

    function raise(str) { throw new Error(fromString(str)); }

    return elm.Native.Error = { Case: Case, If: If, raise: raise };
};
function F2(fun) {
  function wrapper(a) { return function(b) { return fun(a,b) } }
  wrapper.arity = 2;
  wrapper.func = fun;
  return wrapper;
}

function F3(fun) {
  function wrapper(a) {
    return function(b) { return function(c) { return fun(a,b,c) }}
  }
  wrapper.arity = 3;
  wrapper.func = fun;
  return wrapper;
}

function F4(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return fun(a,b,c,d) }}}
  }
  wrapper.arity = 4;
  wrapper.func = fun;
  return wrapper;
}

function F5(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a,b,c,d,e) }}}}
  }
  wrapper.arity = 5;
  wrapper.func = fun;
  return wrapper;
}

function F6(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return fun(a,b,c,d,e,f) }}}}}
  }
  wrapper.arity = 6;
  wrapper.func = fun;
  return wrapper;
}

function F7(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
      return function(g) { return fun(a,b,c,d,e,f,g) }}}}}}
  }
  wrapper.arity = 7;
  wrapper.func = fun;
  return wrapper;
}

function F8(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) {return fun(a,b,c,d,e,f,g,h)}}}}}}}
  }
  wrapper.arity = 8;
  wrapper.func = fun;
  return wrapper;
}

function F9(fun) {
  function wrapper(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
	return function(g) { return function(h) { return function(i) {
        return fun(a,b,c,d,e,f,g,h,i) }}}}}}}}
  }
  wrapper.arity = 9;
  wrapper.func = fun;
  return wrapper;
}

function A2(fun,a,b) {
  return fun.arity === 2 ? fun.func(a,b) : fun(a)(b);
}
function A3(fun,a,b,c) {
  return fun.arity === 3 ? fun.func(a,b,c) : fun(a)(b)(c);
}
function A4(fun,a,b,c,d) {
  return fun.arity === 4 ? fun.func(a,b,c,d) : fun(a)(b)(c)(d);
}
function A5(fun,a,b,c,d,e) {
  return fun.arity === 5 ? fun.func(a,b,c,d,e) : fun(a)(b)(c)(d)(e);
}
function A6(fun,a,b,c,d,e,f) {
  return fun.arity === 6 ? fun.func(a,b,c,d,e,f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun,a,b,c,d,e,f,g) {
  return fun.arity === 7 ? fun.func(a,b,c,d,e,f,g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun,a,b,c,d,e,f,g,h) {
  return fun.arity === 8 ? fun.func(a,b,c,d,e,f,g,h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun,a,b,c,d,e,f,g,h,i) {
  return fun.arity === 9 ? fun.func(a,b,c,d,e,f,g,h,i)
                         : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

Elm.Native.JavaScript = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.JavaScript) return elm.Native.JavaScript;

  var List = Elm.Native.List(elm);
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);

  function fromJS(v) {
      var type = typeof v;
      if (type === 'number' ) return v;
      if (type === 'boolean') return v;
      if (type === 'string' ) return List.fromArray(v);
      if (v instanceof Array) {
          var arr = [];
          var len = v.length;
          for (var i = 0; i < len; ++i) {
              var x = fromJS(v[i]);
              if (x !== null) arr.push(x);
          }
          return List.fromArray(arr);
      }
      if (type === 'object') {
          var rec = { _:{} };
          for (var f in v) {
              var x = fromJS(v[f]);
              if (x !== null) rec[f] = x;
          }
          return rec;
      }
      return null;
  }

  function toJS(v) {
      var type = typeof v;
      if (type === 'number' || type === 'boolean') return v;
      if (type === 'object' && '_' in v) {
          var obj = {};
          for (var k in v) {
              var x = toJS(v[k]);
              if (x !== null) obj[k] = x;
          }
          return obj;
      }
      if (type === 'object' && (v.ctor === '::' || v.ctor === '[]')) {
          var array = List.toArray(v);
          if (typeof array[0] === 'string') {
              array = array.join('');
          } else {
              for (var i = array.length; i--; ) {
                  array[i] = toJS(array[i]);
              }
          }
          return array;
      }
      return null;
  }

  function fromRecord(r) {
      if (typeof r === 'object' && '_' in r) {
          return toJS(r);
      }
      throw new Error("'fromRecord' must be called on a record.");
  }

  function id(n) { return n; }

  function toElement(w,h,domNode) {
      return A3( newElement, w, h, {
              ctor: 'Custom',
              type: 'DomNode',
              render: function(node) { return node; },
              update: function(node,oldNode,newNode) {
                  if (node === newNode) return;
                  node.parentNode.replaceChild(newNode, node);
              },
              model: domNode
          });
  }

  function fromElement(element) {
      return Render.render(element);
  }

  return elm.Native.JavaScript = {
      toFloat    : id,
      toBool     : id,
      toInt      : function(n) { return n|0; },
      toString   : List.fromArray,
      toList     : List.fromArray,
      fromString : function(s) { return List.toArray(s).join(''); },
      fromList   : List.toArray,
      fromInt    : id,
      fromFloat  : id,
      fromBool   : id,

      toElement   : toElement,
      fromElement : fromElement,
      toRecord    : fromJS,
      fromRecord  : fromRecord
  };

};

Elm.Native.Json = function(elm) {
  'use strict';

  var Maybe = Elm.Maybe(elm);
  var Dict = Elm.Dict(elm);
  var List = Elm.List(elm);
  var JS = Elm.JavaScript(elm);
  var Utils = Elm.Native.Utils(elm);

  function fromValue(v) {
    switch (v.ctor) {
    case 'Null'   : return null;
    case 'String' : return JS.fromString(v._0);
    case 'Object' :
      var obj = {};
      var array = JS.fromList(Dict.toList(v._0));
      for (var i = array.length; i--; ) {
	obj[JS.fromString(array[i]._0)] = fromValue(array[i]._1);
      }
      return obj;
    case 'Array'  :
      var array = JS.fromList(v._0);
      for (var i = array.length; i--; ) {
	array[i] = fromValue(array[i]);
      }
      return array;
    default :
      return v._0;
    }
  }

  function toPrettyJSString(sep, obj) {
    return JSON.stringify(fromValue(obj), null, JS.fromString(sep));
  }

  function toValue(v) {
    switch (typeof v) {
    case 'string' : return { ctor:"String", _0: JS.toString(v) };
    case 'number' : return { ctor:"Number", _0: JS.toFloat(v)  };
    case 'boolean': return { ctor:"Bool"  , _0: JS.toBool(v)   };
    case 'object' :
      if (v === null) return { ctor:"Null" };
      if (v instanceof Array) {
          for (var i = v.length; i--; ) { v[i] = toValue(v[i]); }
	  return { ctor:"Array", _0: JS.toList(v) };
      }
      var array = [];
      for (var k in v) array.push(Utils.Tuple2(JS.toString(k), toValue(v[k])));
      return { ctor:"Object", _0: Dict.fromList(JS.toList(array)) };
    }
  }

  function fromJSString(str) {
    try {
	return Maybe.Just(toValue(JSON.parse(str)));
    } catch (e) {
	return Maybe.Nothing;
    }
  }

  return elm.Native.Json = {
      toJSString : F2(toPrettyJSString),
      fromJSString : fromJSString,
      toJSObject : fromValue,
      fromJSObject : toValue
  };

};
Elm.Native.List = function(elm) {
  "use strict";

  elm.Native = elm.Native || {};
  if (elm.Native.List) return elm.Native.List;
  if ('values' in Elm.Native.List)
      return elm.Native.List = Elm.Native.List.values;

  var Utils = Elm.Native.Utils(elm);

  // TODO: Improve Nil handling
  // We can change places like:  if (xs.ctor === '[]') ... to if (xs === Nil) ...
  // but only if we're confident Nil can only be defined once.
  // Currently (27Mar2013) each module can have different instantiations, so multiple Nil objects can exist
  // (and if they're used interchangeably then direct object comparison fails where ctor doesn't).
  // So, this can only be fixed when modules initialisation is also fixed.
  // The performance overhead of the .ctor calls is 5-10% according to jsperf (depending on fn + list size)
  // (on firefox 19)

  var Nil = { ctor:'[]' };

  // using freeze for every cons would be nice but is a huge (9x on firefox 19)
  // performance penalty
  function Cons(hd,tl) { return { ctor:"::", _0:hd, _1:tl }; }

  function throwError(f) {
    throw new Error("Function '" + f + "' expects a non-empty list!");
  }

  function toArray(xs) {
    var out = [];
    while (xs.ctor !== '[]') {
      out.push(xs._0);
      xs = xs._1;
    }
    return out;
  }

  function fromArray(arr) {
    var out = Nil;
    for (var i = arr.length; i--; ) {
      out = Cons(arr[i], out);
    }
    return out;
  }

  function range(lo,hi) {
    var lst = Nil;
    if (lo <= hi) {
      do { lst = Cons(hi,lst) } while (hi-->lo);
    }
    return lst
  }

  function append(xs,ys) {
    if (typeof xs === "string") { return xs.concat(ys); }
    if (xs.ctor === '[]') { return ys; }
    var root = Cons(xs._0, Nil);
    var curr = root;
    xs = xs._1;
    while (xs.ctor !== '[]') {
	curr._1 = Cons(xs._0, Nil);
	xs = xs._1;
	curr = curr._1;
    }
    curr._1 = ys;
    return root;
  }

  function head(v) { return v.ctor === '[]' ? throwError('head') : v._0; }
  function tail(v) { return v.ctor === '[]' ? throwError('tail') : v._1; }

  function last(xs) {
    if (xs.ctor === '[]') { throwError('last'); }
    var out = xs._0;
    while (xs.ctor !== '[]') {
      out = xs._0;
      xs = xs._1;
    }
    return out;
  }

  function map(f, xs) {
    var arr = [];
    while (xs.ctor !== '[]') {
      arr.push(f(xs._0));
      xs = xs._1;
    }
    return fromArray(arr);
  }

   // f defined similarly for both foldl and foldr (NB: different from Haskell)
   // ie, foldl : (a -> b -> b) -> b -> [a] -> b
  function foldl(f, b, xs) {
    var acc = b;
    while (xs.ctor !== '[]') {
      acc = A2(f, xs._0, acc);
      xs = xs._1;
    }
    return acc;
  }

  function foldr(f, b, xs) {
    var arr = toArray(xs);
    var acc = b;
    for (var i = arr.length; i--; ) {
      acc = A2(f, arr[i], acc);
    }
    return acc;
  }

  function foldl1(f, xs) {
    return xs.ctor === '[]' ? throwError('foldl1') : foldl(f, xs._0, xs._1);
  }

  function foldr1(f, xs) {
    if (xs.ctor === '[]') { throwError('foldr1'); }
    var arr = toArray(xs);
    var acc = arr.pop();
    for (var i = arr.length; i--; ) {
      acc = A2(f, arr[i], acc);
    }
    return acc;
  }

  function scanl(f, b, xs) {
    var arr = toArray(xs);
    arr.unshift(b);
    var len = arr.length;
    for (var i = 1; i < len; ++i) {
      arr[i] = A2(f, arr[i], arr[i-1]);
    }
    return fromArray(arr);
  }

  function scanl1(f, xs) {
    return xs.ctor === '[]' ? throwError('scanl1') : scanl(f, xs._0, xs._1);
  }

  function filter(pred, xs) {
    var arr = [];
    while (xs.ctor !== '[]') {
      if (pred(xs._0)) { arr.push(xs._0); }
      xs = xs._1;
    }
    return fromArray(arr);
  }

  function length(xs) {
    var out = 0;
    while (xs.ctor !== '[]') {
      out += 1;
      xs = xs._1;
    }
    return out;
  }

  function member(x, xs) {
    while (xs.ctor !== '[]') {
      if (Utils.eq(x,xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function reverse(xs) { return fromArray(toArray(xs).reverse()); }

  function concat(xss) {
      if (xss.ctor === '[]') return xss;
      var arr = toArray(xss);
      var xs = arr[arr.length-1];
      for (var i = arr.length-1; i--; ) {
	  xs = append(arr[i], xs);
      }
      return xs;
  }

  function all(pred, xs) {
    while (xs.ctor !== '[]') {
      if (!pred(xs._0)) return false;
      xs = xs._1;
    }
    return true;
  }

  function any(pred, xs) {
    while (xs.ctor !== '[]') {
      if (pred(xs._0)) return true;
      xs = xs._1;
    }
    return false;
  }

  function zipWith(f, xs, ys) {
    var arr = [];
    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
      arr.push(A2(f, xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function zip(xs, ys) {
    var arr = [];
    while (xs.ctor !== '[]' && ys.ctor !== '[]') {
      arr.push(Utils.Tuple2(xs._0, ys._0));
      xs = xs._1;
      ys = ys._1;
    }
    return fromArray(arr);
  }

  function sort(xs) {
    function cmp(a,b) {
      var ord = Utils.compare(a,b).ctor;
      return ord=== 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
    }
    return fromArray(toArray(xs).sort(cmp));
  }

  function nth(xs, n) {
    return toArray(xs)[n];
  }

  function take(n, xs) {
    var arr = [];
    while (xs.ctor !== '[]' && n > 0) {
      arr.push(xs._0);
      xs = xs._1;
      --n;
    }
    return fromArray(arr);
  }

  function drop(n, xs) {
    while (xs.ctor !== '[]' && n > 0) {
      xs = xs._1;
      --n;
    }
    return xs;
  }

  function join(sep, xss) {
    if (typeof sep === 'string') return toArray(xss).join(sep);
    if (xss.ctor === '[]') return Nil;
    var s = toArray(sep);
    var out = toArray(xss._0);
    xss = xss._1;
    while (xss.ctor !== '[]') {
      out = out.concat(s, toArray(xss._0));
      xss = xss._1;
    }
    return fromArray(out);
  }

  function split(seperator, list) {
    var array = toArray(list);
    var alen = array.length;
    if (alen === 0) {
      // splitting an empty list is a list of lists: [[]]
      return Cons(Nil,Nil);
    }

    var sep = toArray(seperator);
    var seplen = sep.length;
    if (seplen === 0) {
      // splitting with an empty sep is a list of all elements
      // Same as (map (\x -> [x]) list)
      var out = Nil;
      for (var i = alen; i--; ) {
        out = Cons(Cons(array[i],Nil), out);
      }
      return out;
    }

    var matches = [-seplen];
    var sepStart = sep[0];
    var len = alen - seplen + 1;
    for (var i = 0; i < len; ++i) {
      if (Utils.eq(array[i], sepStart)) {
        var match = true;
        for (var j = seplen; --j; ) {
          if (!Utils.eq(array[i+j], sep[j])) { match = false;  break; }
        }
        if (match) {
          matches.push(i);
          i += seplen - 1;
        }
      }
    }

    // shortcut in case of no matches
    if (matches.length === 0) {
      return Cons(list,Nil);
    }

    var out = Nil;
    var index = alen - 1;
    for (var i = matches.length; i--; ) {
      var temp = Nil;
      var stop = matches[i] + seplen - 1;
      for ( ; index > stop; --index ) {
        temp = Cons(array[index], temp);
      }
      out = Cons(temp,out);
      index -= seplen;
    }
    return out;
  }

  Elm.Native.List.values = {
      Nil:Nil,
      Cons:Cons,
      cons:F2(Cons),
      toArray:toArray,
      fromArray:fromArray,
      range:range,
      append:append,

      head:head,
      tail:tail,
      last:last,

      map:F2(map),
      foldl:F3(foldl),
      foldr:F3(foldr),

      foldl1:F2(foldl1),
      foldr1:F2(foldr1),
      scanl:F3(scanl),
      scanl1:F2(scanl1),
      filter:F2(filter),
      length:length,
      member:F2(member),
      reverse:reverse,
      concat:concat,

      all:F2(all),
      any:F2(any),
      zipWith:F3(zipWith),
      zip:F2(zip),
      sort:sort,
      nth:F2(nth),
      take:F2(take),
      drop:F2(drop),

      join:F2(join),
      split:F2(split)
  };
  return elm.Native.List = Elm.Native.List.values;

};
Elm.Native.Matrix2D = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 if (elm.Native.Matrix2D) return elm.Native.Matrix2D;

 var A;
 if (typeof Float32Array === 'undefined') {
     A = function(arr) {
         this.length = arr.length;
         this[0] = arr[0];
         this[1] = arr[1];
         this[2] = arr[2];
         this[3] = arr[3];
         this[4] = arr[4];
         this[5] = arr[5];
     };
 } else {
     A = Float32Array;
 }

 // layout of matrix in an array is
 //
 //   | m11 m12 dx |
 //   | m21 m22 dy |
 //   |  0   0   1 |
 //
 //  new A([ m11, m12, dx, m21, m22, dy ])

 var identity = new A([1,0,0,0,1,0]);
 function matrix(m11, m12, m21, m22, dx, dy) {
     return new A([m11, m12, dx, m21, m22, dy]);
 }
 function rotation(t) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     return new A([c, -s, 0, s, c, 0]);
 }

 function rotate(t,m) {
     var c = Math.cos(t);
     var s = Math.sin(t);
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11*c + m12*s, -m11*s + m12*c, m[2],
                   m21*c + m22*s, -m21*s + m22*c, m[5]]);
 }
 /*
 function move(xy,m) {
     var x = xy._0;
     var y = xy._1;
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
     return new A([m11, m12, m11*x + m12*y + m[2],
                   m21, m22, m21*x + m22*y + m[5]]);
 }
 function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
 function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
 function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
 function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
 function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

 function transform(m11, m21, m12, m22, mdx, mdy, n) {
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }
 */
 function multiply(m, n) {
     var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
     var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
     return new A([m11*n11 + m12*n21,
                   m11*n12 + m12*n22,
                   m11*ndx + m12*ndy + mdx,
                   m21*n11 + m22*n21,
                   m21*n12 + m22*n22,
                   m21*ndx + m22*ndy + mdy]);
 }

 return elm.Native.Matrix2D = {
     identity:identity,
     matrix:F6(matrix),
     rotation:rotation,
     multiply:F2(multiply)
     /*
     transform:F7(transform),
     rotate:F2(rotate),
     move:F2(move),
     scale:F2(scale),
     scaleX:F2(scaleX),
     scaleY:F2(scaleY),
     reflectX:reflectX,
     reflectY:reflectY
     */
 };

};

Elm.Native.Show = function(elm) {
    'use strict';

    elm.Native = elm.Native || {};
    if (elm.Native.Show) return elm.Native.Show;

    var NList = Elm.Native.List(elm);
    var List = Elm.List(elm);
    var Maybe = Elm.Maybe(elm);
    var JS = Elm.JavaScript(elm);
    var Dict = Elm.Dict(elm);
    var Json = Elm.Json(elm);
    var Tuple2 = Elm.Native.Utils(elm).Tuple2;

    var toString = function(v) {
        if (typeof v === "function") {
            var name = v.func ? v.func.name : v.name;
            return '<function' + (name === '' ? '' : ': ') + name + '>';
        } else if (typeof v === "boolean") {
            return v ? "True" : "False";
        } else if (typeof v === "number") {
            return v+"";
        } else if (typeof v === "string" && v.length < 2) {
            return "'" + showChar(v) + "'";
        } else if (typeof v === "object" && '_' in v) {
            var output = [];
            for (var k in v._) {
                for (var i = v._[k].length; i--; ) {
                    output.push(k + " = " + toString(v._[k][i]));
                }
            }
            for (var k in v) {
                if (k === '_') continue;
                output.push(k + " = " + toString(v[k]));
            }
            if (output.length === 0) return "{}";
            return "{ " + output.join(", ") + " }";
        } else if (typeof v === "object" && 'ctor' in v) {
            if (v.ctor.substring(0,6) === "_Tuple") {
                var output = [];
                for (var k in v) {
                    if (k === 'ctor') continue;
                    output.push(toString(v[k]));
                }
                return "(" + output.join(",") + ")";
            } else if (v.ctor === "::") {
                var isStr = typeof v._0 === "string",
                start = isStr ? '"' : "[",
                end   = isStr ? '"' : "]",
                sep   = isStr ?  "" : ",",
                f     = !isStr ? toString : showChar;
                var output = start + f(v._0);
                v = v._1;
                while (v.ctor === "::") {
                    output += sep + f(v._0);
                    v = v._1;
                }
                return output + end;
            } else if (v.ctor === "[]") {
                return "[]";
            } else if (v.ctor === "RBNode" || v.ctor === "RBEmpty") {
                var cons = F3(function(k,v,acc){return NList.Cons(Tuple2(k,v),acc)});
                var list = A3(Dict.foldr, cons, NList.Nil, v);
                var name = "Dict";
                if (list.ctor === "::" && list._0._1.ctor === "_Tuple0") {
                    name = "Set";
                    list = A2(List.map, function(x){return x._0}, list);
                }
                return name + ".fromList " + toString(list);
            } else {
                var output = "";
                for (var i in v) {
                    if (i === 'ctor') continue;
                    var str = toString(v[i]);
                    var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
                    output += ' ' + (parenless ? str : '(' + str + ')');
                }
                return v.ctor + output;
            }
        }
        if (typeof v === 'object' && 'recv' in v) return '<signal>';
        return "<internal structure>";
    };
    function show(v) { return NList.fromArray(toString(v)); }

    function showChar (c) {
        return c === '\n' ? '\\n' :
               c === '\t' ? '\\t' :
               c === '\b' ? '\\b' :
               c === '\r' ? '\\r' :
               c === '\v' ? '\\v' :
               c === '\0' ? '\\0' :
               c === '\'' ? "\\'" :
               c === '\"' ? '\\"' :
               c === '\\' ? '\\\\' : c;
    }

    return elm.Native.Show = { show:show };
};

Elm.Native.Text = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Text) return elm.Native.Text;

  var JS = Elm.JavaScript(elm);
  var htmlHeight = Elm.Native.Utils(elm).htmlHeight;
  var Color = Elm.Native.Color(elm);
  var Element = Elm.Graphics.Element(elm);
  var show = Elm.Native.Show(elm).show;

  function makeSpaces(s) {
    if (s.length == 0) { return s; }
    var arr = s.split('');
    if (arr[0] == ' ') { arr[0] = "&nbsp;" }      
    for (var i = arr.length; --i; ) {
      if (arr[i][0] == ' ' && arr[i-1] == ' ') {
        arr[i-1] = arr[i-1] + arr[i];
        arr[i] = '';
      }
    }
    for (var i = arr.length; i--; ) {
      if (arr[i].length > 1 && arr[i][0] == ' ') {
        var spaces = arr[i].split('');
        for (var j = spaces.length - 2; j >= 0; j -= 2) {
          spaces[j] = '&nbsp;';
        }
        arr[i] = spaces.join('');
      }
    }
    arr = arr.join('');
    if (arr[arr.length-1] === " ") {
	return arr.slice(0,-1) + '&nbsp;';
    }
    return arr;
  }

  function properEscape(str) {
    if (str.length == 0) return str;
    str = str //.replace(/&/g,  "&#38;")
	.replace(/"/g, /*"*/ '&#34;')
	.replace(/'/g, /*'*/ "&#39;")
	.replace(/</g,  "&#60;")
	.replace(/>/g,  "&#62;")
	.replace(/\n/g, "<br/>");
    var arr = str.split('<br/>');
    for (var i = arr.length; i--; ) {
	arr[i] = makeSpaces(arr[i]);
    }
    return arr.join('<br/>');
  }

  function toText(str) { return properEscape(JS.fromString(str)); }

  function addTag(tag) { return function(text) {
      return '<' + tag + ' style="padding:0;margin:0">' + text + '</' + tag + '>';
    }
  }
  
  function addStyle(style, value, text) {
    return "<span style='" + style + ":" + value + "'>" + text + "</span>";
  }

  function typeface(name, text) {
    return addStyle('font-family', JS.fromString(name), text);
  }
  function monospace(text) {
    return addStyle('font-family', 'monospace', text);
  }
  function size(px, text) { return addStyle('font-size', px + 'px', text) }
  var header = addTag('h1');
  function height(h, text) { return addStyle('font-size', h+'px', text) }
  function italic(text) { return addStyle('font-style', 'italic', text) }
  var bold = addTag('b');

  function extract(c) {
    if (c._3 === 1) { return 'rgb(' + c._0 + ',' + c._1 + ',' + c._2 + ')'; }
    return 'rgba(' + c._0 + ',' + c._1 + ',' + c._2 + ',' + c._3 + ')';
  }
  function color(c, text) {
    return addStyle('color', extract(c), text);
  }
  function underline(text) { return addStyle('text-decoration', 'underline', text) }
  function overline(text) { return addStyle('text-decoration', 'overline', text) }
  function strikeThrough(text) {
      return addStyle('text-decoration', 'line-through', text);
  }
  function link(href, text) {
    return "<a href='" + toText(href) + "'>" + text + "</a>";
  }

  function position(pos) { return function(text) {
    var e = {ctor:'RawHtml',
	     _0: '<div style="padding:0;margin:0;text-align:' +
                   pos + '">' + text + '</div>'
            };
    var p = A2(htmlHeight, 0, text);
    return A3(Element.newElement, p._0, p._1, e);
   }
  }

  function asText(v) {
      return position('left')(monospace(toText(show(v))));
  }

  function plainText(v) {
      return position('left')(toText(v));
  }

  return elm.Native.Text = {
      toText: toText,

      header : header,
      height : F2(height),
      italic : italic,
      bold : bold,
      underline : underline,
      overline : overline,
      strikeThrough : strikeThrough,
      monospace : monospace,
      typeface : F2(typeface),
      color : F2(color),
      link : F2(link),

      justified : position('justify'),
      centered : position('center'),
      righted : position('right'),
      text : position('left'),
      plainText : plainText,

      asText : asText
  };

};
Elm.Native.Utils = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Utils) return elm.Native.Utils;

  function eq(x,y) {
    if (x === y) return true;
    if (typeof x === "object") {
      var c = 0;
      for (var i in x) { ++c; if (!eq(x[i],y[i])) return false; }
      return c === Object.keys(y).length;
    }
    if (typeof x === 'function') {
      throw new Error('Equality error: general function equality is ' +
      'undecidable, and therefore, unsupported');
    }
    return x === y;
  }

  // code in Generate/JavaScript.hs depends on the particular
  // integer values assigned to LT, EQ, and GT
  var LT = -1, EQ = 0, GT = 1, ord = ['LT','EQ','GT'];
  function compare(x,y) { return { ctor: ord[cmp(x,y)+1] } }
  function cmp(x,y) {
    var ord;
    if (typeof x !== 'object') return x === y ? EQ : x < y ? LT : GT;

    if (x.ctor === "::" || x.ctor === "[]") {
      while (true) {
          if (x.ctor === "[]" && y.ctor === "[]") return EQ;
          if (x.ctor !== y.ctor) return x.ctor === '[]' ? LT : GT;
          ord = cmp(x._0, y._0);
          if (ord !== EQ) return ord;
          x = x._1;
          y = y._1;
      }
    }

    if (x.ctor.slice(0,6) === '_Tuple') {
      var n = x.ctor.slice(6) - 0;
      var err = 'cannot compare tuples with more than 6 elements.';
      if (n === 0) return EQ;
      if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
      if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
      if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
      if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
      if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
      if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
      if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
      return EQ;
    }
    throw new Error('Comparison error: comparison is only defined on ints, ' +
        'floats, times, chars, strings, lists of comparable values, ' +
        'and tuples of comparable values.')
  }


  var Tuple0 = { ctor: "_Tuple0" };
  function Tuple2(x,y) { return { ctor:"_Tuple2", _0:x, _1:y } }

  var count = 0;
  function guid(_) { return count++ }

  function copy(r) {
    var o = {};
    for (var i in r) { o[i] = r[i]; }
    return o;
  }

  function remove(x,r) {
    var o = copy(r);
    if (x in o._) {
      o[x] = o._[x][0];
      o._[x] = o._[x].slice(1);
      if (o._[x].length === 0) { delete o._[x]; }
    } else {
      delete o[x];
    }
    return o;
  }

  function replace(kvs,r) {
    var o = copy(r);
    for (var i = kvs.length; i--; ) {
      var kvsi = kvs[i];
      o[kvsi[0]] = kvsi[1];
    }
    return o;
  }

  function insert(x,v,r) {
    var o = copy(r);
    if (x in o) o._[x] = [o[x]].concat(x in o._ ? o._[x].slice(0) : []);
    o[x] = v;
    return o;
  }

  function max(a,b) { return a > b ? a : b }
  function min(a,b) { return a < b ? a : b }

  function mod(a,b) {
    var r = a % b;
    var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r+b) : -mod(-a,-b));

    return m === b ? 0 : m;
  }

  function htmlHeight(width, html) {
    var t = document.createElement('div');
    t.innerHTML = html;
    if (width > 0) { t.style.width = width + "px"; }
    t.style.visibility = "hidden";
    t.style.styleFloat = "left";
    t.style.cssFloat   = "left";

    elm.node.appendChild(t);
    var style = window.getComputedStyle(t, null);
    var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
    var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
    elm.node.removeChild(t);
    return Tuple2(w,h);
  }

  function adjustOffset() {
      var node = elm.node;
      var offsetX = 0, offsetY = 0;
      if (node.offsetParent) {
          do {
              offsetX += node.offsetLeft;
              offsetY += node.offsetTop;
          } while (node = node.offsetParent);
      }
      elm.node.offsetX = offsetX;
      elm.node.offsetY = offsetY;
  }

  if (elm.display === ElmRuntime.Display.COMPONENT) {
      elm.addListener(elm.inputs, elm.node, 'mouseover', adjustOffset);
  }

  return elm.Native.Utils = {
      eq:eq,
      cmp:cmp,
      compare:F2(compare),
      Tuple0:Tuple0,
      Tuple2:Tuple2,
      copy: copy,
      remove: remove,
      replace: replace,
      insert: insert,
      guid: guid,
      max : F2(max),
      min : F2(min),
      mod : F2(mod),
      htmlHeight: F2(htmlHeight),
      toFloat: function(x){return x}
  };
};

Elm.Native.Graphics.Collage = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 if (elm.Native.Graphics.Collage) return elm.Native.Graphics.Collage;

 var newElement = Elm.Graphics.Element(elm).newElement;
 var C = ElmRuntime.use(ElmRuntime.Render.Collage);

 function collage(w,h,forms) {
     return A3(newElement, w, h, {
                 ctor: 'Custom',
		 type: 'Collage',
		 render: C.render,
		 update: C.update,
		 model: {w:w, h:h, forms:forms}
	 });
 }
 return elm.Native.Graphics.Collage = { collage:F3(collage) };

};
Elm.Native.Graphics.Input = function(elm) {
 "use strict";

 elm.Native = elm.Native || {};
 elm.Native.Graphics = elm.Native.Graphics || {};
 if (elm.Native.Graphics.Input) return elm.Native.Graphics.Input;

 var Render = ElmRuntime.use(ElmRuntime.Render.Element);
 var newNode = ElmRuntime.use(ElmRuntime.Render.Utils).newElement;

 var Signal = Elm.Signal(elm);
 var newElement = Elm.Graphics.Element(elm).newElement;
 var JS = Elm.Native.JavaScript(elm);
 var Utils = Elm.Native.Utils(elm);
 var Tuple2 = Utils.Tuple2;

 function dropDown(values) {
     var entries = JS.fromList(values);
     var events = Signal.constant(entries[0]._1);

     var drop = newNode('select');
     drop.style.border = '0 solid';
     for (var i = 0; i < entries.length; ++i) {
         var option = newNode('option');
         var name = JS.fromString(entries[i]._0);
         option.value = name;
         option.innerHTML = name;
         drop.appendChild(option);
     }
     drop.addEventListener('change', function() {
             elm.notify(events.id, entries[drop.selectedIndex]._1);
         });

     var t = drop.cloneNode(true);
     t.style.visibility = "hidden";

     elm.node.appendChild(t);
     var style = window.getComputedStyle(t, null);
     var w = Math.ceil(style.getPropertyValue("width").slice(0,-2) - 0);
     var h = Math.ceil(style.getPropertyValue("height").slice(0,-2) - 0);
     elm.node.removeChild(t);
     
     console.log(w,h);
     var element = A3(newElement, w, h, {
             ctor: 'Custom',
             type: 'DropDown',
             render: function render(model) { return drop; },
             update: function update(node, oldModel, newModel) {},
             model: {}
         });

     return Tuple2(Signal.constant(element), events);
 }

 function buttons(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
         var b = newNode('button');
         b.style.display = 'block';
         b.elmEvent = model.event;
         function click() { elm.notify(events.id, b.elmEvent); }
         b.addEventListener('click', click);
         b.innerHTML = model.text;
         return b;
     }

     function update(node, oldModel, newModel) {
         node.elmEvent = newModel.event;
         var txt = newModel.text;
         if (oldModel.text !== txt) node.innerHTML = txt;
     }

     function button(evnt, txt) {
         return A3(newElement, 100, 40, {
                     ctor: 'Custom',
                     type: 'Button',
                     render: render,
                     update: update,
                     model: { event:evnt, text:JS.fromString(txt) }
             });
     }

     return { _:{}, button:F2(button), events:events };
 }

 function customButtons(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
         var btn = newNode('div');
         btn.elmEvent = model.event;

         btn.elmUp    = Render.render(model.up);
         btn.elmHover = Render.render(model.hover);
         btn.elmDown  = Render.render(model.down);

         function replace(node) {
           if (node !== btn.firstChild) btn.replaceChild(node, btn.firstChild);
         }
         var overCount = 0;
         function over(e) {
             if (overCount++ > 0) return;
             replace(btn.elmHover);
         }
         function out(e) {
             if (btn.contains(e.toElement || e.relatedTarget)) return;
             overCount = 0;
             replace(btn.elmUp);
         }
         function up() {
             replace(btn.elmHover);
             elm.notify(events.id, btn.elmEvent);
         }
         function down() { replace(btn.elmDown); }
         btn.addEventListener('mouseover', over);
         btn.addEventListener('mouseout' , out);
         btn.addEventListener('mousedown', down);
         btn.addEventListener('mouseup'  , up);

         btn.appendChild(btn.elmUp);

         var clicker = newNode('div');
         clicker.style.width = btn.elmUp.style.width;
         clicker.style.height = btn.elmUp.style.height;
         clicker.style.position = 'absolute';
         clicker.style.top = 0;
         btn.appendChild(clicker);

         return btn;
     }

     function update(node, oldModel, newModel) {
         node.elmEvent = newModel.event;
         Render.update(node.elmUp, oldModel.up, newModel.up)
         Render.update(node.elmHover, oldModel.hover, newModel.hover)
         Render.update(node.elmDown, oldModel.down, newModel.down)
     }

     function button(evnt, up, hover, down) {
         return A3(newElement,
                   Math.max(up.props.width, hover.props.width, down.props.width),
                   Math.max(up.props.height, hover.props.height, down.props.height),
                   { ctor: 'Custom',
                     type: 'CustomButton',
                     render: render,
                     update: update,
                     model: { event:evnt, up:up, hover:hover, down:down }
                   });
     }

     return { _:{}, customButton:F4(button), events:events };
 }


 function hoverables(defaultValue) {
     var events = Signal.constant(defaultValue);
     function hoverable(handler, elem) {
         function onHover(bool) {
             elm.notify(events.id, handler(bool));
         }
         var props = Utils.replace([['hover',onHover]], elem.props);
         return { props:props, element:elem.element };
     }
     return { _:{}, hoverable:F2(hoverable), events:events };
 }


 function checkboxes(defaultValue) {
     var events = Signal.constant(defaultValue);

     function render(model) {
         var b = newNode('input');
         b.type = 'checkbox';
         b.checked = model.checked;
         b.style.display = 'block';
         b.elmHandler = model.handler;
         function change() { elm.notify(events.id, b.elmHandler(b.checked)); }
         b.addEventListener('change', change);
         return b;
     }

     function update(node, oldModel, newModel) {
         node.elmHandler = newModel.handler;
         node.checked = newModel.checked;
         return true;
     }

     function box(handler, checked) {
         return A3(newElement, 13, 13, {
                     ctor: 'Custom',
                     type: 'CheckBox',
                     render: render,
                     update: update,
                     model: { checked:checked, handler:handler  }
             });
     }

     return { _:{}, box:F2(box), events:events };
 }

 function setRange(node, start, end, dir) {
     if (node.parentNode) {
         node.setSelectionRange(start, end, dir);
     } else {
         setTimeout(function(){node.setSelectionRange(start, end, dir);}, 0);
     }
 }

 function mkTextPool(type) { return function fields(defaultValue) {
     var events = Signal.constant(defaultValue);

     var state = null;

     function render(model) {
         var field = newNode('input');
         field.elmHandler = model.handler;

         field.id = 'test';
         field.type = type;
         field.placeholder = JS.fromString(model.placeHolder);
         field.value = JS.fromString(model.state.string);
         setRange(field, model.state.selectionStart, model.state.selectionEnd, 'forward');
         field.style.border = 'none';
         state = model.state;

         function update() {
             var start = field.selectionStart,
                 end = field.selectionEnd;
             if (field.selectionDirection === 'backward') {
                 start = end;
                 end = field.selectionStart;
             }
             state = { _:{},
                       string:JS.toString(field.value),
                       selectionStart:start,
                       selectionEnd:end };
             elm.notify(events.id, field.elmHandler(state));
         }
         function mousedown() {
             update();
             elm.node.addEventListener('mouseup', mouseup);
         }
         function mouseup() {
             update();
             elm.node.removeEventListener('mouseup', mouseup)
         }
         field.addEventListener('keyup', update);
         field.addEventListener('mousedown', mousedown);

         return field;
     }

     function update(node, oldModel, newModel) {
         node.elmHandler = newModel.handler;
         if (state === newModel.state) return;
         var newStr = JS.fromString(newModel.state.string);
         if (node.value !== newStr) node.value = newStr;

         var start = newModel.state.selectionStart;
         var end = newModel.state.selectionEnd;
         var direction = 'forward';
         if (end < start) {
             start = end;
             end = newModel.state.selectionStart;
             direction = 'backward';
         }
 
         if (node.selectionStart !== start
             || node.selectionEnd !== end
             || node.selectionDirection !== direction) {
             setRange(node, start, end, direction);
         }
     }

     function field(handler, placeHolder, state) {
         return A3(newElement, 200, 30,
                   { ctor: 'Custom',
                     type: type + 'Input',
                     render: render,
                     update: update,
                     model: { handler:handler,
                              placeHolder:placeHolder,
                              state:state }
                   });
     }

     return { _:{}, field:F3(field), events:events };
   }
 }

 return elm.Native.Graphics.Input = {
     buttons:buttons,
     customButtons:customButtons,
     hoverables:hoverables,
     checkboxes:checkboxes,
     fields:mkTextPool('text'),
     emails:mkTextPool('email'),
     passwords:mkTextPool('password'),
     dropDown:dropDown
 };

};

Elm.Native.Http = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Http) return elm.Native.Http;


  var JS = Elm.JavaScript(elm);
  var List = Elm.List(elm);
  var Signal = Elm.Signal(elm);


  function registerReq(queue,responses) { return function(req) {
    if (req.url.ctor !== '[]') { sendReq(queue,responses,req); }
   };
  }

  function updateQueue(queue,responses) {
    if (queue.length > 0) {
      elm.notify(responses.id, queue[0].value);
      if (queue[0].value.ctor !== 'Waiting') {
        queue.shift();
        setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    }
  }

  function setHeader(pair) {
    request.setRequestHeader( JS.fromString(pair._0), JS.fromString(pair._1) );
  }

  function sendReq(queue,responses,req) {
    var response = { value: { ctor:'Waiting' } };
    queue.push(response);

    var request = null;
    if (window.ActiveXObject)  { request = new ActiveXObject("Microsoft.XMLHTTP"); }
    if (window.XMLHttpRequest) { request = new XMLHttpRequest(); }
    request.onreadystatechange = function(e) {
      if (request.readyState === 4) {
        response.value = (request.status >= 200 && request.status < 300 ?
        { ctor:'Success', _0:JS.toString(request.responseText) } :
        { ctor:'Failure', _0:request.status, _1:JS.toString(request.statusText) });
        setTimeout(function() { updateQueue(queue,responses); }, 0);
      }
    };
    request.open(JS.fromString(req.verb), JS.fromString(req.url), true);
    List.map(setHeader)(req.headers);
    request.send(JS.fromString(req.body));
  }

  function send(requests) {
    var responses = Signal.constant(elm.Http.Waiting);
    var sender = A2( Signal.lift, registerReq([],responses), requests );
    function f(x) { return function(y) { return x; } }
    return A3( Signal.lift2, f, responses, sender );
  }

  return elm.Native.Http = {send:send};

};

Elm.Native.Keyboard = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Keyboard) return elm.Native.Keyboard;

  var Signal = Elm.Signal(elm);
  var NList = Elm.Native.List(elm);

  var keysDown = Signal.constant(NList.Nil);
  var lastKey = Signal.constant('\0');

  elm.addListener([keysDown.id], document, 'keydown', function down(e) {
          if (NList.member(e.keyCode)(keysDown.value)) return;
          elm.notify(keysDown.id, NList.Cons(e.keyCode, keysDown.value));
      });
  elm.addListener([keysDown.id], document, 'keyup', function up(e) {
          function notEq(kc) { return kc !== e.keyCode; }
          elm.notify(keysDown.id, NList.filter(notEq)(keysDown.value));
      });
  elm.addListener([keysDown.id], document, 'blur', function blur(e) {
          elm.notify(keysDown.id, NList.Nil);
      });
  elm.addListener([lastKey.id], document, 'keypress', function press(e) {
          elm.notify(lastKey.id, e.charCode || e.keyCode);
      });

  function keySignal(f) {
    var signal = Signal.dropRepeats(A2(Signal.lift, f, keysDown));
    keysDown.defaultNumberOfKids += 1;
    signal.defaultNumberOfKids = 0;
    return signal;
  }

  function dir(up, down, left, right) {
    function f(ks) {
      var x = 0, y = 0;
      while (ks.ctor === "::") {
        switch (ks._0) {
          case left : --x; break;
          case right: ++x; break;
          case up   : ++y; break;
          case down : --y; break;
        }
        ks = ks._1;
      }
      return { _:{}, x:x, y:y };
    }
    return keySignal(f);
  }

  function is(key) { return keySignal(NList.member(key)); }

  return elm.Native.Keyboard = {
      isDown:is,
      directions:F4(dir),
      keysDown:keysDown,
      lastPressed:lastKey
  };

};

Elm.Native.Mouse = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Mouse) return elm.Native.Mouse;

  var Signal = Elm.Signal(elm);
  var Utils = Elm.Native.Utils(elm);

  var position  = Signal.constant(Utils.Tuple2(0,0));
  position.defaultNumberOfKids = 2;

  // do not move x and y into Elm. By setting their default number
  // of kids, it is possible to detatch the mouse listeners if
  // they are not needed.
  var x = A2( Signal.lift, function(p){return p._0}, position);
  x.defaultNumberOfKids = 0;
  var y = A2( Signal.lift, function(p){return p._1}, position);
  y.defaultNumberOfKids = 0;

  var isDown    = Signal.constant(false);
  var isClicked = Signal.constant(false);
  var clicks = Signal.constant(Utils.Tuple0);

  function getXY(e) {
    var posx = 0;
    var posy = 0;
    if (!e) e = window.event;
    if (e.pageX || e.pageY) {
	posx = e.pageX;
	posy = e.pageY;
    } else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft +
	  document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop +
	  document.documentElement.scrollTop;
    }
    return Utils.Tuple2(posx-elm.node.offsetX, posy-elm.node.offsetY);
  }

  var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

  elm.addListener([isClicked.id, clicks.id], node, 'click', function click() {
          elm.notify(isClicked.id, true);
          elm.notify(clicks.id, Utils.Tuple0);
          elm.notify(isClicked.id, false);
      });
  elm.addListener([isDown.id], node, 'mousedown', function down() {
          elm.notify(isDown.id, true);
      });
  elm.addListener([isDown.id], node, 'mouseup', function up() {
          elm.notify(isDown.id, false);
      });
  elm.addListener([position.id], node, 'mousemove', function move(e) {
          elm.notify(position.id, getXY(e));
      });

  return elm.Native.Mouse = {
      position: position,
      x:x,
      y:y,
      isClicked: isClicked,
      isDown: isDown,
      clicks: clicks
  };
};
Elm.Native.Random = function(elm) {
  'use strict';
  elm.Native = elm.Native || {};
  if (elm.Native.Random) return elm.Native.Random;

  var Signal = Elm.Signal(elm);

  function range(min, max, signal) {
    function f(x) { return Math.floor(Math.random() * (max-min+1)) + min; }
    return A2( Signal.lift, f, signal );
  }

  function flt(signal) {
    function f(x) { return Math.random(); }
    return A2( Signal.lift, f, signal );
  }

  elm.Native.Random = { range: F3(range) };
  elm.Native.Random['float'] = flt;
  return elm.Native.Random;

};

Elm.Native.Signal = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Signal) return elm.Native.Signal;

  var Utils = Elm.Native.Utils(elm);
  var foldl1 = Elm.List(elm).foldl1;

  function send(node, timestep, changed) {
    var kids = node.kids;
    for (var i = kids.length; i--; ) {
      kids[i].recv(timestep, changed, node.id);
    }
  }

  function Input(base) {
    this.id = Utils.guid();
    this.value = base;
    this.kids = [];
    this.defaultNumberOfKids = 0;
    this.recv = function(timestep, eid, v) {
      var changed = eid === this.id;
      if (changed) { this.value = v; }
      send(this, timestep, changed);
      return changed;
    };
    elm.inputs.push(this);
  }

  function LiftN(update, args) {
    this.id = Utils.guid();
    this.value = update();
    this.kids = [];

    var n = args.length;
    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      ++count;
      if (changed) { isChanged = true; }
      if (count == n) {
        if (isChanged) { this.value = update(); }
        send(this, timestep, isChanged);
        isChanged = false;
        count = 0;
      }
    };
    for (var i = n; i--; ) { args[i].kids.push(this); }
  }

  function lift(func, a) {
    function update() { return func(a.value); }
    return new LiftN(update, [a]);
  }
  function lift2(func, a, b) {
    function update() { return A2( func, a.value, b.value ); }
    return new LiftN(update, [a,b]);
  }
  function lift3(func, a, b, c) {
    function update() { return A3( func, a.value, b.value, c.value ); }
    return new LiftN(update, [a,b,c]);
  }
  function lift4(func, a, b, c, d) {
    function update() { return A4( func, a.value, b.value, c.value, d.value ); }
    return new LiftN(update, [a,b,c,d]);
  }
  function lift5(func, a, b, c, d, e) {
    function update() { return A5( func, a.value, b.value, c.value, d.value, e.value ); }
    return new LiftN(update, [a,b,c,d,e]);
  }
  function lift6(func, a, b, c, d, e, f) {
    function update() { return A6( func, a.value, b.value, c.value, d.value, e.value, f.value ); }
    return new LiftN(update, [a,b,c,d,e,f]);
  }
  function lift7(func, a, b, c, d, e, f, g) {
    function update() { return A7( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g]);
  }
  function lift8(func, a, b, c, d, e, f, g, h) {
    function update() { return A8( func, a.value, b.value, c.value, d.value, e.value, f.value, g.value, h.value ); }
    return new LiftN(update, [a,b,c,d,e,f,g,h]);
  }

  function Foldp(step, state, input) {
    this.id = Utils.guid();
    this.value = state;
    this.kids = [];

    this.recv = function(timestep, changed, parentID) {
      if (changed) {
          this.value = A2( step, input.value, this.value );
      }
      send(this, timestep, changed);
    };
    input.kids.push(this);
  }

  function foldp(step, state, input) {
      return new Foldp(step, state, input);
  }

  function DropIf(pred,base,input) {
    this.id = Utils.guid();
    this.value = pred(input.value) ? base : input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !pred(input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function DropRepeats(input) {
    this.id = Utils.guid();
    this.value = input.value;
    this.kids = [];
    this.recv = function(timestep, changed, parentID) {
      var chng = changed && !Utils.eq(this.value,input.value);
      if (chng) { this.value = input.value; }
      send(this, timestep, chng);
    };
    input.kids.push(this);
  }

  function dropWhen(s1,b,s2) {
    var pairs = lift2( F2(function(x,y){return {x:x,y:y};}), s1, s2 );
    var dropped = new DropIf(function(p){return p.x;},{x:true,y:b},pairs);
    return lift(function(p){return p.y;}, dropped);
  }

  function timestamp(a) {
    function update() { return Utils.Tuple2(Date.now(), a.value); }
    return new LiftN(update, [a]);
  }

  function SampleOn(s1,s2) {
    this.id = Utils.guid();
    this.value = s2.value;
    this.kids = [];

    var count = 0;
    var isChanged = false;

    this.recv = function(timestep, changed, parentID) {
      if (parentID === s1.id) isChanged = changed;
      ++count;
      if (count == 2) {
        if (isChanged) { this.value = s2.value; }
        send(this, timestep, isChanged);
        count = 0;
        isChanged = false;
      }
    };
    s1.kids.push(this);
    s2.kids.push(this);
  }

  function sampleOn(s1,s2) { return new SampleOn(s1,s2); }

  function delay(t,s) {
      var delayed = new Input(s.value);
      var firstEvent = true;
      function update(v) {
        if (firstEvent) { firstEvent = false; return; }
        setTimeout(function() { elm.notify(delayed.id, v); }, t);
      }
      function first(a,b) { return a; }
      return new SampleOn(delayed, lift2(F2(first), delayed, lift(update,s)));
  }

  function Merge(s1,s2) {
      this.id = Utils.guid();
      this.value = s1.value;
      this.kids = [];

      var next = null;
      var count = 0;
      var isChanged = false;

      this.recv = function(timestep, changed, parentID) {
        ++count;
        if (changed) {
            isChanged = true;
            if (parentID == s2.id && next === null) { next = s2.value; }
            if (parentID == s1.id) { next = s1.value; }
        }

        if (count == 2) {
            if (isChanged) { this.value = next; next = null; }
            send(this, timestep, isChanged);
            isChanged = false;
            count = 0;
        }
      };
      s1.kids.push(this);
      s2.kids.push(this);
  }

  function merge(s1,s2) { return new Merge(s1,s2); }
  function merges(ss) { return A2(foldl1, F2(merge), ss); }

  return elm.Native.Signal = {
    constant : function(v) { return new Input(v); },
    lift  : F2(lift ),
    lift2 : F3(lift2),
    lift3 : F4(lift3),
    lift4 : F5(lift4),
    lift5 : F6(lift5),
    lift6 : F7(lift6),
    lift7 : F8(lift7),
    lift8 : F9(lift8),
    foldp : F3(foldp),
    delay : F2(delay),
    merge : F2(merge),
    merges : merges,
    count : function(s) { return foldp(F2(function(_,c) { return c+1; }), 0, s); },
    countIf : F2(function(pred,s) {
      return foldp(F2(function(x,c){
        return pred(x) ? c+1 : c; }), 0, s)}),
    keepIf : F3(function(pred,base,sig) {
      return new DropIf(function(x) {return !pred(x);},base,sig); }),
    dropIf : F3(function(pred,base,sig) { return new DropIf(pred,base,sig); }),
    keepWhen : F3(function(s1,b,s2) {
      return dropWhen(lift(function(b){return !b;},s1), b, s2); }),
    dropWhen : F3(dropWhen),
    dropRepeats : function(s) { return new DropRepeats(s);},
    sampleOn : F2(sampleOn),
    timestamp : timestamp
  };
};

Elm.Native.Time = function(elm) {
  'use strict';

  var Signal = Elm.Signal(elm);
  var Maybe = Elm.Maybe(elm);
  var Utils = Elm.Native.Utils(elm);

  function fpsWhen(desiredFPS, isOn) {
    var msPerFrame = 1000 / desiredFPS;
    var prev = Date.now(), curr = prev, diff = 0, wasOn = true;
    var ticker = Signal.constant(diff);
    function tick(zero) { return function() {
        curr = Date.now();
        diff = zero ? 0 : curr - prev;
        prev = curr;
        elm.notify(ticker.id, diff);
      };
    }
    var timeoutID = 0;
    function f(isOn, t) {
      if (isOn) {
        timeoutID = setTimeout(tick(!wasOn && isOn), msPerFrame);
      } else if (wasOn) {
        clearTimeout(timeoutID);
      }
      wasOn = isOn;
      return t;
    }
    return A3( Signal.lift2, F2(f), isOn, ticker );
  }

  function everyWhen(t, isOn) {
    var clock = Signal.constant(Date.now());
    var id = setInterval(function tellTime() {
            if (!elm.notify(clock.id, Date.now())) {
                clearInterval(id);
            }
        }, t);
    return clock;
  }

  function since(t, s) {
    function cmp(a,b) { return !Utils.eq(a,b); }
    var dcount = Signal.count(A2(Signal.delay, t, s));
    return A3( Signal.lift2, F2(cmp), Signal.count(s), dcount );
  }
  function read(s) {
      var t = Date.parse(s);
      return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
  }
  return elm.Native.Time = {
      fpsWhen : F2(fpsWhen),
      fps : function(t) { return fpsWhen(t, Signal.constant(true)); },
      every : function(t) { return everyWhen(t, Signal.constant(true)) },
      delay : Signal.delay,
      timestamp : Signal.timestamp,
      since : F2(since),
      toDate : function(t) { return new window.Date(t); },
      read   : read
  };

};

Elm.Native.Touch = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Touch) return elm.Native.Touch;

  var Signal = Elm.Signal(elm);
  var JS = Elm.JavaScript(elm);
  var _ = Elm.Native.Utils(elm);

  function Dict() {
    this.keys = [];
    this.values = [];

    this.insert = function(key,value) {
      this.keys.push(key);
      this.values.push(value);
    };
    this.lookup = function(key) {
      var i = this.keys.indexOf(key)
      return i >= 0 ? this.values[i] : {x:0,y:0,t:0};
    };
    this.remove = function(key) {
      var i = this.keys.indexOf(key);
      if (i < 0) return;
      var t = this.values[i];
      this.keys.splice(i,1);
      this.values.splice(i,1);
      return t;
    };
    this.clear = function() {
        this.keys = [];
        this.values = [];
    };
  }
  
  var root = Signal.constant([]),
      tapTime = 500,
      hasTap = false,
      tap = {_:{},x:0,y:0},
      dict = new Dict();

  function touch(t) {
      var r = dict.lookup(t.identifier);
      return {_ : {},
	      id: t.identifier,
	      x : t.pageX - elm.node.offsetX,
	      y : t.pageY - elm.node.offsetY,
	      x0: r.x,
	      y0: r.y,
	      t0: r.t
	      };
  }

  var node = elm.display === ElmRuntime.Display.FULLSCREEN ? document : elm.node;

  function start(e) {
    dict.insert(e.identifier,
                {x: e.pageX - elm.node.offsetX,
                 y: e.pageY - elm.node.offsetY,
                 t: Date.now()});
  }
  function end(e) {
    var t = dict.remove(e.identifier);
    if (Date.now() - t.t < tapTime) {
        hasTap = true;
        tap = {_:{}, x:t.x, y:t.y};
    }
  }

  function listen(name, f) {
    function update(e) {
      for (var i = e.changedTouches.length; i--; ) { f(e.changedTouches[i]); }
      var ts = new Array(e.touches.length);
      for (var i = e.touches.length; i--; ) { ts[i] = touch(e.touches[i]); }
      elm.notify(root.id, ts);
      e.preventDefault();
    }
    elm.addListener([root.id], node, name, update);
  }

  listen("touchstart", start);
  listen("touchmove", function(_){});
  listen("touchend", end);
  listen("touchcancel", end);
  listen("touchleave", end);

  var mouseID = -1;
  function move(e) {
      for (var i = root.value.length; i--; ) {
          if (root.value[i].id === mouseID) {
              root.value[i].x = e.pageX - elm.node.offsetX;
              root.value[i].y = e.pageY - elm.node.offsetY;
              elm.notify(root.id, root.value);
              break;
          }
      }
  }
  elm.addListener([root.id], node, "mousedown", function down(e) {
          node.addEventListener("mousemove", move);
          e.identifier = mouseID;
          start(e);
          root.value.push(touch(e));
          elm.notify(root.id, root.value);
      });
  elm.addListener([root.id], node, "mouseup", function up(e) {
          node.removeEventListener("mousemove", move);
          e.identifier = mouseID;
          end(e);
          for (var i = root.value.length; i--; ) {
              if (root.value[i].id === mouseID) {
                  root.value.splice(i, 1);
                  --mouseID;
                  break;
              }
          }
          elm.notify(root.id, root.value);
      });
  elm.addListener([root.id], node, "blur", function blur(e) {
          node.removeEventListener("mousemove", move);
          if (root.values.length > 0) {
              elm.notify(root.id, []);
              --mouseID;
          }
          dict.clear();
      });

  function dependency(f) {
      var sig = A2( Signal.lift, f, root );
      root.defaultNumberOfKids += 1;
      sig.defaultNumberOfKids = 0;
      return sig;
  }

  var touches = dependency(JS.toList);

  var taps = function() {
      var sig = dependency(function(_) { return tap; });
      sig.defaultNumberOfKids = 1;
      function pred(_) { var b = hasTap; hasTap = false; return b; }
      var sig2 = A3( Signal.keepIf, pred, {_:{},x:0,y:0}, sig);
      sig2.defaultNumberOfKids = 0;
      return sig2;
  }();

  return elm.Native.Touch = { touches: touches, taps: taps };

};
Elm.Native.WebSocket = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.WebSocket) return elm.Native.WebSocket;

  var Signal = Elm.Signal(elm);
  var JS = Elm.JavaScript(elm);
  var List = Elm.Native.List(elm);

  function open(url, outgoing) {
    var incoming = Signal.constant(List.Nil);
    var ws = new WebSocket(JS.fromString(url));

    var pending = [];
    var ready = false;
    
    ws.onopen = function(e) {
      var len = pending.length;
      for (var i = 0; i < len; ++i) { ws.send(pending[i]); }
      ready = true;
    };
    ws.onmessage = function(event) {
      elm.notify(incoming.id, JS.toString(event.data));
    };
    
    function send(msg) {
      var s = JS.fromString(msg);
      ready ? ws.send(s) : pending.push(s);
    }
    
    function take1(x,y) { return x }
    return A3(Signal.lift2, F2(take1), incoming, A2(Signal.lift, send, outgoing));
  }

  return elm.Native.WebSocket = { connect: F2(open) };
};

Elm.Native.Window = function(elm) {
  'use strict';

  elm.Native = elm.Native || {};
  if (elm.Native.Window) return elm.Native.Window;

  var Signal = Elm.Signal(elm);
  var Tuple2 = Elm.Native.Utils(elm).Tuple2;

  function getWidth() { return elm.node.clientWidth; }
  function getHeight() {
      if (elm.display === ElmRuntime.Display.FULLSCREEN) {
          return window.innerHeight;
      }
      return elm.node.clientHeight;
  }

  var dimensions = Signal.constant(Tuple2(getWidth(), getHeight()));
  dimensions.defaultNumberOfKids = 2;

  // Do not move width and height into Elm. By setting the default number of kids,
  // the resize listener can be detached.
  var width  = A2(Signal.lift, function(p){return p._0;}, dimensions);
  width.defaultNumberOfKids = 0;

  var height = A2(Signal.lift, function(p){return p._1;}, dimensions);
  height.defaultNumberOfKids = 0;

  function resizeIfNeeded() {
      var w = getWidth();
      var h = getHeight();
      if (dimensions.value._0 === w && dimensions.value._1 === h) return;
      elm.notify(dimensions.id, Tuple2(w,h));
  }
  elm.addListener([dimensions.id], window, 'resize', resizeIfNeeded);

  return elm.Native.Window = {
      dimensions:dimensions,
      width:width,
      height:height,
      resizeIfNeeded:resizeIfNeeded
  };

};

Elm.Automaton = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Signal = Elm.Signal(elm);
  var List = Elm.List(elm);
  var Maybe = Elm.Maybe(elm);
  var _op = {};
  var enqueue = F2(function(x, arg1){
    return function(){
      switch (arg1.ctor) {
        case '_Tuple2':
          return {ctor:"_Tuple2", _0:_L.Cons(x,arg1._0), _1:arg1._1};
      }_E.Case('Line 72, Column 22')}();});
  var empty = {ctor:"_Tuple2", _0:_J.toList([]), _1:_J.toList([])};
  var dequeue = function(q){
    return function(){
      switch (q.ctor) {
        case '_Tuple2':
          switch (q._0.ctor) {
            case '[]':
              switch (q._1.ctor) {
                case '[]':
                  return Maybe.Nothing;
              }break;
          }
          switch (q._1.ctor) {
            case '::':
              return Maybe.Just({ctor:"_Tuple2", _0:q._1._0, _1:{ctor:"_Tuple2", _0:q._0, _1:q._1._1}});
            case '[]':
              return dequeue({ctor:"_Tuple2", _0:_J.toList([]), _1:List.reverse(q._0)});
          }break;
      }_E.Case('Line 73, Column 13')}();};
  var Step = function(a){
    return {ctor:"Step", _0:a};};
  var hiddenState = F2(function(s, f){
    return Step(function(x){
      return function(){
        var _8 = A2(f, x, s);
        var out = function(){
          switch (_8.ctor) {
            case '_Tuple2':
              return _8._1;
          }_E.Case('Line 63, Column 46')}();
        var s$ = function(){
          switch (_8.ctor) {
            case '_Tuple2':
              return s$;
          }_E.Case('Line 63, Column 46')}();
        return {ctor:"_Tuple2", _0:A2(hiddenState, s$, f), _1:out};}();});});
  var average = function(k){
    return function(){
      var stepFull = F2(function(n, arg1){
        return function(){
          switch (arg1.ctor) {
            case '_Tuple3':
              return function(){
                var case19 = dequeue(arg1._0);
                switch (case19.ctor) {
                  case 'Just':
                    switch (case19._0.ctor) {
                      case '_Tuple2':
                        return function(){
                          var sum$ = ((arg1._2+n)-case19._0._0);
                          return {ctor:"_Tuple2", _0:{ctor:"_Tuple3", _0:A2(enqueue, n, case19._0._1), _1:arg1._1, _2:sum$}, _1:(sum$/arg1._1)};}();
                    }break;
                  case 'Nothing':
                    return {ctor:"_Tuple2", _0:{ctor:"_Tuple3", _0:arg1._0, _1:arg1._1, _2:arg1._2}, _1:0};
                }_E.Case('Line 85, Column 11')}();
          }_E.Case('Line 85, Column 11')}();});
      var step = F2(function(n, arg1){
        return function(){
          switch (arg1.ctor) {
            case '_Tuple3':
              return (_N.eq(arg1._1,k) ? A2(stepFull, n, {ctor:"_Tuple3", _0:arg1._0, _1:arg1._1, _2:arg1._2}) : (Basics.otherwise ? {ctor:"_Tuple2", _0:{ctor:"_Tuple3", _0:A2(enqueue, n, arg1._0), _1:(arg1._1+1), _2:(arg1._2+n)}, _1:((arg1._2+n)/(arg1._1+1))} : _E.If('Line 82, Column 11')));
          }_E.Case('Line 82, Column 11')}();});
      return A2(hiddenState, {ctor:"_Tuple3", _0:empty, _1:0, _2:0}, step);}();};
  var pure = function(f){
    return Step(function(x){
      return {ctor:"_Tuple2", _0:pure(f), _1:f(x)};});};
  var run = F3(function(auto, base, inputs){
    return function(){
      var step = F2(function(a, arg1){
        return function(){
          switch (arg1.ctor) {
            case '_Tuple2':
              switch (arg1._0.ctor) {
                case 'Step':
                  return arg1._0._0(a);
              }break;
          }_E.Case('Line 18, Column 28')}();});
      return A2(Signal.lift, function(arg1){
        return function(){
          switch (arg1.ctor) {
            case '_Tuple2':
              return arg1._1;
          }_E.Case('Line 19, Column 23')}();}, A3(Signal.foldp, step, {ctor:"_Tuple2", _0:auto, _1:base}, inputs));}();});
  var state = F2(function(s, f){
    return Step(function(x){
      return function(){
        var s$ = A2(f, x, s);
        return {ctor:"_Tuple2", _0:A2(state, s$, f), _1:s$};}();});});
  var count = A2(state, 0, F2(function(arg2, c){
    return function(){
      return (c+1);}();}));
  var step = F2(function(a, arg1){
    return function(){
      switch (arg1.ctor) {
        case 'Step':
          return arg1._0(a);
      }_E.Case('Line 23, Column 19')}();});
  var combine = function(autos){
    return Step(function(a){
      return function(){
        var _37 = List.unzip(A2(List.map, step(a), autos));
        var autos$ = function(){
          switch (_37.ctor) {
            case '_Tuple2':
              return autos$;
          }_E.Case('Line 39, Column 34')}();
        var bs = function(){
          switch (_37.ctor) {
            case '_Tuple2':
              return _37._1;
          }_E.Case('Line 39, Column 34')}();
        return {ctor:"_Tuple2", _0:combine(autos$), _1:bs};}();});};
  _op['>>>'] = F2(function(f, g){
    return Step(function(a){
      return function(){
        var _44 = A2(step, a, f);
        var b = function(){
          switch (_44.ctor) {
            case '_Tuple2':
              return _44._1;
          }_E.Case('Line 28, Column 29')}();
        var f$ = function(){
          switch (_44.ctor) {
            case '_Tuple2':
              return f$;
          }_E.Case('Line 28, Column 29')}();
        var _51 = A2(step, b, g);
        var c = function(){
          switch (_51.ctor) {
            case '_Tuple2':
              return _51._1;
          }_E.Case('Line 29, Column 29')}();
        var g$ = function(){
          switch (_51.ctor) {
            case '_Tuple2':
              return g$;
          }_E.Case('Line 29, Column 29')}();
        return {ctor:"_Tuple2", _0:A2(_op['>>>'], f$, g$), _1:c};}();});});
  _op['<<<'] = F2(function(g, f){
    return A2(_op['>>>'], f, g);});
  return elm.Automaton = {
    _op : _op, 
    run : run, 
    step : step, 
    combine : combine, 
    pure : pure, 
    state : state, 
    hiddenState : hiddenState, 
    count : count, 
    empty : empty, 
    enqueue : enqueue, 
    dequeue : dequeue, 
    average : average, 
    Step : Step};};
Elm.Basics = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.Basics = Elm.Native.Basics(elm);
  var _op = {};
  _op['||'] = Native.Basics.or;
  _op['|>'] = F2(function(x, f){
    return f(x);});
  var xor = Native.Basics.xor;
  var uncurry = Native.Basics.uncurry;
  var truncate = Native.Basics.truncate;
  var toFloat = Native.Basics.toFloat;
  var tan = Native.Basics.tan;
  var sqrt = Native.Basics.sqrt;
  var snd = Native.Basics.snd;
  var sin = Native.Basics.sin;
  var show = Native.Basics.show;
  var round = Native.Basics.round;
  var rem = Native.Basics.rem;
  var radians = function(t){
    return t;};
  var pi = Native.Basics.pi;
  var otherwise = true;
  var not = Native.Basics.not;
  var mod = Native.Basics.mod;
  var min = Native.Basics.min;
  var max = Native.Basics.max;
  var logBase = Native.Basics.logBase;
  var id = function(x){
    return x;};
  var fst = Native.Basics.fst;
  var floor = Native.Basics.floor;
  var flip = F3(function(f, b, a){
    return A2(f, a, b);});
  var e = Native.Basics.e;
  var div = Native.Basics.div;
  var curry = Native.Basics.curry;
  var cos = Native.Basics.cos;
  var compare = Native.Basics.compare;
  var clamp = Native.Basics.clamp;
  var ceiling = Native.Basics.ceiling;
  var atan2 = Native.Basics.atan2;
  var atan = Native.Basics.atan;
  var asin = Native.Basics.asin;
  var acos = Native.Basics.acos;
  var abs = Native.Basics.abs;
  _op['^'] = Native.Basics.exp;
  var LT = {ctor:"LT"};
  var GT = {ctor:"GT"};
  var EQ = {ctor:"EQ"};
  _op['>='] = Native.Basics.ge;
  _op['>'] = Native.Basics.gt;
  _op['=='] = Native.Basics.eq;
  _op['<|'] = F2(function(f, x){
    return f(x);});
  _op['<='] = Native.Basics.le;
  _op['<'] = Native.Basics.lt;
  _op['/='] = Native.Basics.neq;
  _op['/'] = Native.Basics.floatDiv;
  _op['.'] = F3(function(f, g, x){
    return f(g(x));});
  _op['-'] = Native.Basics.sub;
  _op['+'] = Native.Basics.add;
  var toPolar = function(arg1){
    return function(){
      switch (arg1.ctor) {
        case '_Tuple2':
          return {ctor:"_Tuple2", _0:Native.Basics.sqrt((Math.pow(arg1._0,2)+Math.pow(arg1._1,2))), _1:A2(Native.Basics.atan2, arg1._1, arg1._0)};
      }_E.Case('Line 29, Column 18')}();};
  _op['*'] = Native.Basics.mul;
  var degrees = function(d){
    return ((d*Native.Basics.pi)/180);};
  var fromPolar = function(arg1){
    return function(){
      switch (arg1.ctor) {
        case '_Tuple2':
          return {ctor:"_Tuple2", _0:(arg1._0*Native.Basics.cos(arg1._1)), _1:(arg1._0*Native.Basics.sin(arg1._1))};
      }_E.Case('Line 24, Column 20')}();};
  var turns = function(r){
    return ((2*Native.Basics.pi)*r);};
  _op['&&'] = Native.Basics.and;
  return elm.Basics = {
    _op : _op, 
    radians : radians, 
    degrees : degrees, 
    turns : turns, 
    fromPolar : fromPolar, 
    toPolar : toPolar, 
    div : div, 
    rem : rem, 
    mod : mod, 
    cos : cos, 
    sin : sin, 
    tan : tan, 
    acos : acos, 
    asin : asin, 
    atan : atan, 
    atan2 : atan2, 
    sqrt : sqrt, 
    abs : abs, 
    logBase : logBase, 
    min : min, 
    max : max, 
    clamp : clamp, 
    pi : pi, 
    e : e, 
    compare : compare, 
    xor : xor, 
    not : not, 
    otherwise : otherwise, 
    round : round, 
    truncate : truncate, 
    floor : floor, 
    ceiling : ceiling, 
    toFloat : toFloat, 
    show : show, 
    id : id, 
    fst : fst, 
    snd : snd, 
    flip : flip, 
    curry : curry, 
    uncurry : uncurry, 
    LT : LT, 
    EQ : EQ, 
    GT : GT};};
Elm.Char = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.Char = Elm.Native.Char(elm);
  var _op = {};
  var toUpper = Native.Char.toUpper;
  var toLower = Native.Char.toLower;
  var toLocaleUpper = Native.Char.toLocaleUpper;
  var toLocaleLower = Native.Char.toLocaleLower;
  var toCode = Native.Char.toCode;
  var isUpper = Native.Char.isUpper;
  var isOctDigit = Native.Char.isOctDigit;
  var isLower = Native.Char.isLower;
  var isHexDigit = Native.Char.isHexDigit;
  var isDigit = Native.Char.isDigit;
  var fromCode = Native.Char.fromCode;
  return elm.Char = {
    _op : _op, 
    isUpper : isUpper, 
    isLower : isLower, 
    isDigit : isDigit, 
    isOctDigit : isOctDigit, 
    isHexDigit : isHexDigit, 
    toUpper : toUpper, 
    toLower : toLower, 
    toLocaleUpper : toLocaleUpper, 
    toLocaleLower : toLocaleLower, 
    toCode : toCode, 
    fromCode : fromCode};};
Elm.Color = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.Color = Elm.Native.Color(elm);
  var _op = {};
  var hsva = Native.Color.hsva;
  var hsv = Native.Color.hsv;
  var complement = Native.Color.complement;
  var Radial = F5(function(a, b, c, d, e){
    return {ctor:"Radial", _0:a, _1:b, _2:c, _3:d, _4:e};});
  var radial = Radial;
  var Linear = F3(function(a, b, c){
    return {ctor:"Linear", _0:a, _1:b, _2:c};});
  var linear = Linear;
  var Color = F4(function(a, b, c, d){
    return {ctor:"Color", _0:a, _1:b, _2:c, _3:d};});
  var black = A4(Color, 0, 0, 0, 1);
  var blue = A4(Color, 0, 0, 255, 1);
  var cyan = A4(Color, 0, 255, 255, 1);
  var forestGreen = A4(Color, 34, 139, 34, 1);
  var gray = A4(Color, 128, 128, 128, 1);
  var green = A4(Color, 0, 128, 0, 1);
  var grey = A4(Color, 128, 128, 128, 1);
  var lime = A4(Color, 0, 255, 0, 1);
  var magenta = A4(Color, 255, 0, 255, 1);
  var maroon = A4(Color, 128, 0, 0, 1);
  var navy = A4(Color, 0, 0, 128, 1);
  var purple = A4(Color, 128, 0, 128, 1);
  var red = A4(Color, 255, 0, 0, 1);
  var rgb = F3(function(r, g, b){
    return A4(Color, r, g, b, 1);});
  var rgba = Color;
  var teal = A4(Color, 0, 128, 128, 1);
  var violet = A4(Color, 238, 130, 238, 1);
  var white = A4(Color, 255, 255, 255, 1);
  var yellow = A4(Color, 255, 255, 0, 1);
  return elm.Color = {
    _op : _op, 
    rgba : rgba, 
    rgb : rgb, 
    red : red, 
    lime : lime, 
    blue : blue, 
    yellow : yellow, 
    cyan : cyan, 
    magenta : magenta, 
    black : black, 
    white : white, 
    gray : gray, 
    grey : grey, 
    maroon : maroon, 
    navy : navy, 
    green : green, 
    teal : teal, 
    purple : purple, 
    violet : violet, 
    forestGreen : forestGreen, 
    complement : complement, 
    hsva : hsva, 
    hsv : hsv, 
    linear : linear, 
    radial : radial, 
    Color : Color, 
    Linear : Linear, 
    Radial : Radial};};
Elm.Date = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Native = Native || {};
  Native.Date = Elm.Native.Date(elm);
  var Time = Elm.Time(elm);
  var Maybe = Elm.Maybe(elm);
  var _op = {};
  var year = Native.Date.year;
  var toTime = Native.Date.toTime;
  var second = Native.Date.second;
  var read = Native.Date.read;
  var month = Native.Date.month;
  var minute = Native.Date.minute;
  var hour = Native.Date.hour;
  var dayOfWeek = Native.Date.dayOfWeek;
  var day = Native.Date.day;
  var Wed = {ctor:"Wed"};
  var Tue = {ctor:"Tue"};
  var Thu = {ctor:"Thu"};
  var Sun = {ctor:"Sun"};
  var Sep = {ctor:"Sep"};
  var Sat = {ctor:"Sat"};
  var Oct = {ctor:"Oct"};
  var Nov = {ctor:"Nov"};
  var Mon = {ctor:"Mon"};
  var May = {ctor:"May"};
  var Mar = {ctor:"Mar"};
  var Jun = {ctor:"Jun"};
  var Jul = {ctor:"Jul"};
  var Jan = {ctor:"Jan"};
  var Fri = {ctor:"Fri"};
  var Feb = {ctor:"Feb"};
  var Dec = {ctor:"Dec"};
  var Date = {ctor:"Date"};
  var Aug = {ctor:"Aug"};
  var Apr = {ctor:"Apr"};
  return elm.Date = {
    _op : _op, 
    read : read, 
    toTime : toTime, 
    year : year, 
    month : month, 
    day : day, 
    dayOfWeek : dayOfWeek, 
    hour : hour, 
    minute : minute, 
    second : second, 
    Date : Date, 
    Mon : Mon, 
    Tue : Tue, 
    Wed : Wed, 
    Thu : Thu, 
    Fri : Fri, 
    Sat : Sat, 
    Sun : Sun, 
    Jan : Jan, 
    Feb : Feb, 
    Mar : Mar, 
    Apr : Apr, 
    May : May, 
    Jun : Jun, 
    Jul : Jul, 
    Aug : Aug, 
    Sep : Sep, 
    Oct : Oct, 
    Nov : Nov, 
    Dec : Dec};};
Elm.Dict = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Maybe = Elm.Maybe(elm);
  var Native = Native || {};
  Native.Error = Elm.Native.Error(elm);
  var List = Elm.List(elm);
  var Native = Native || {};
  Native.Utils = Elm.Native.Utils(elm);
  var _op = {};
  var Red = {ctor:"Red"};
  var RBNode = F5(function(a, b, c, d, e){
    return {ctor:"RBNode", _0:a, _1:b, _2:c, _3:d, _4:e};});
  var isRed = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._0.ctor) {
            case 'Red':
              return true;
          }break;
      }
      return false;}();};
  var isRedLeft = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              switch (t._3._0.ctor) {
                case 'Red':
                  return true;
              }break;
          }break;
      }
      return false;}();};
  var isRedLeftLeft = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              switch (t._3._3.ctor) {
                case 'RBNode':
                  switch (t._3._3._0.ctor) {
                    case 'Red':
                      return true;
                  }break;
              }break;
          }break;
      }
      return false;}();};
  var isRedRight = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._4.ctor) {
            case 'RBNode':
              switch (t._4._0.ctor) {
                case 'Red':
                  return true;
              }break;
          }break;
      }
      return false;}();};
  var isRedRightLeft = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._4.ctor) {
            case 'RBNode':
              switch (t._4._3.ctor) {
                case 'RBNode':
                  switch (t._4._3._0.ctor) {
                    case 'Red':
                      return true;
                  }break;
              }break;
          }break;
      }
      return false;}();};
  var rotateLeft = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._4.ctor) {
            case 'RBNode':
              return A5(RBNode, t._0, t._4._1, t._4._2, A5(RBNode, Red, t._1, t._2, t._3, t._4._3), t._4._4);
          }break;
      }
      return Native.Error.raise(_str('rotateLeft of a node without enough children'));}();};
  var rotateLeftIfNeeded = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._4.ctor) {
            case 'RBNode':
              switch (t._4._0.ctor) {
                case 'Red':
                  return rotateLeft(t);
              }break;
          }break;
      }
      return t;}();};
  var rotateRight = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              return A5(RBNode, t._0, t._3._1, t._3._2, t._3._3, A5(RBNode, Red, t._1, t._2, t._3._4, t._4));
          }break;
      }
      return Native.Error.raise(_str('rotateRight of a node without enough children'));}();};
  var rotateRightIfNeeded = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              switch (t._3._0.ctor) {
                case 'Red':
                  switch (t._3._3.ctor) {
                    case 'RBNode':
                      switch (t._3._3._0.ctor) {
                        case 'Red':
                          return rotateRight(t);
                      }break;
                  }break;
              }break;
          }break;
      }
      return t;}();};
  var RBEmpty = {ctor:"RBEmpty"};
  var empty = RBEmpty;
  var findWithDefault = F3(function(base, k, t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return base;
        case 'RBNode':
          return function(){
            var case115 = A2(Native.Utils.compare, k, t._1);
            switch (case115.ctor) {
              case 'EQ':
                return t._2;
              case 'GT':
                return A3(findWithDefault, base, k, t._4);
              case 'LT':
                return A3(findWithDefault, base, k, t._3);
            }_E.Case('Line 137, Column 5')}();
      }_E.Case('Line 134, Column 2')}();});
  var foldl = F3(function(f, acc, t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return acc;
        case 'RBNode':
          return A3(foldl, f, A3(f, t._1, t._2, A3(foldl, f, acc, t._3)), t._4);
      }_E.Case('Line 360, Column 3')}();});
  var foldr = F3(function(f, acc, t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return acc;
        case 'RBNode':
          return A3(foldr, f, A3(f, t._1, t._2, A3(foldr, f, acc, t._4)), t._3);
      }_E.Case('Line 368, Column 3')}();});
  var keys = function(t){
    return A3(foldr, F3(function(k, v, acc){
      return _L.Cons(k,acc);}), _J.toList([]), t);};
  var toList = function(t){
    return A3(foldr, F3(function(k, v, acc){
      return _L.Cons({ctor:"_Tuple2", _0:k, _1:v},acc);}), _J.toList([]), t);};
  var values = function(t){
    return A3(foldr, F3(function(k, v, acc){
      return _L.Cons(v,acc);}), _J.toList([]), t);};
  var lookup = F2(function(k, t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return Maybe.Nothing;
        case 'RBNode':
          return function(){
            var case134 = A2(Native.Utils.compare, k, t._1);
            switch (case134.ctor) {
              case 'EQ':
                return Maybe.Just(t._2);
              case 'GT':
                return A2(lookup, k, t._4);
              case 'LT':
                return A2(lookup, k, t._3);
            }_E.Case('Line 125, Column 5')}();
      }_E.Case('Line 122, Column 2')}();});
  var member = F2(function(k, t){
    return Maybe.isJust(A2(lookup, k, t));});
  var map = F2(function(f, t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return RBEmpty;
        case 'RBNode':
          return A5(RBNode, t._0, t._1, f(t._2), A2(map, f, t._3), A2(map, f, t._4));
      }_E.Case('Line 352, Column 3')}();});
  var min = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBEmpty':
          return Native.Error.raise(_str('(min Empty) is not defined'));
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBEmpty':
              return {ctor:"_Tuple2", _0:t._1, _1:t._2};
          }
          return min(t._3);
      }_E.Case('Line 105, Column 3')}();};
  var Black = {ctor:"Black"};
  var ensureBlackRoot = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._0.ctor) {
            case 'Red':
              return A5(RBNode, Black, t._1, t._2, t._3, t._4);
          }break;
      }
      return t;}();};
  var otherColor = function(c){
    return function(){
      switch (c.ctor) {
        case 'Black':
          return Red;
        case 'Red':
          return Black;
      }_E.Case('Line 186, Column 16')}();};
  var color_flip = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              switch (t._4.ctor) {
                case 'RBNode':
                  return A5(RBNode, otherColor(t._0), t._1, t._2, A5(RBNode, otherColor(t._3._0), t._3._1, t._3._2, t._3._3, t._3._4), A5(RBNode, otherColor(t._4._0), t._4._1, t._4._2, t._4._3, t._4._4));
              }break;
          }break;
      }
      return Native.Error.raise(_str('color_flip called on a Empty or Node with a Empty child'));}();};
  var color_flipIfNeeded = function(t){
    return function(){
      switch (t.ctor) {
        case 'RBNode':
          switch (t._3.ctor) {
            case 'RBNode':
              switch (t._3._0.ctor) {
                case 'Red':
                  switch (t._4.ctor) {
                    case 'RBNode':
                      switch (t._4._0.ctor) {
                        case 'Red':
                          return color_flip(t);
                      }break;
                  }break;
              }break;
          }break;
      }
      return t;}();};
  var fixUp = function(t){
    return color_flipIfNeeded(rotateRightIfNeeded(rotateLeftIfNeeded(t)));};
  var insert = F3(function(k, v, t){
    return function(){
      var ins = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBEmpty':
              return A5(RBNode, Red, k, v, RBEmpty, RBEmpty);
            case 'RBNode':
              return function(){
                var h = function(){
                  var case192 = A2(Native.Utils.compare, k, t._1);
                  switch (case192.ctor) {
                    case 'EQ':
                      return A5(RBNode, t._0, t._1, v, t._3, t._4);
                    case 'GT':
                      return A5(RBNode, t._0, t._1, t._2, t._3, ins(t._4));
                    case 'LT':
                      return A5(RBNode, t._0, t._1, t._2, ins(t._3), t._4);
                  }_E.Case('Line 219, Column 19')}();
                return fixUp(h);}();
          }_E.Case('Line 216, Column 7')}();};
      return ensureBlackRoot(ins(t));}();});
  var fromList = function(assocs){
    return A3(List.foldl, F2(function(arg2, d){
      return function(){
        switch (arg2.ctor) {
          case '_Tuple2':
            return A3(insert, arg2._0, arg2._1, d);
        }_E.Case('Line 403, Column 43')}();}), empty, assocs);};
  var intersect = F2(function(t1, t2){
    return function(){
      var combine = F3(function(k, v, t){
        return (A2(member, k, t2) ? A3(insert, k, v, t) : (Basics.otherwise ? t : _E.If('Line 381, Column 22')));});
      return A3(foldl, combine, empty, t1);}();});
  var singleton = F2(function(k, v){
    return A3(insert, k, v, RBEmpty);});
  var union = F2(function(t1, t2){
    return A3(foldl, insert, t2, t1);});
  var moveRedLeft = function(t){
    return function(){
      var t$ = color_flip(t);
      return function(){
        switch (t$.ctor) {
          case 'RBNode':
            return function(){
              switch (t$._4.ctor) {
                case 'RBNode':
                  switch (t$._4._3.ctor) {
                    case 'RBNode':
                      switch (t$._4._3._0.ctor) {
                        case 'Red':
                          return color_flip(rotateLeft(A5(RBNode, t$._0, t$._1, t$._2, t$._3, rotateRight(t$._4))));
                      }break;
                  }break;
              }
              return t$;}();
        }
        return t$;}();}();};
  var moveRedLeftIfNeeded = function(t){
    return ((isRedLeft(t)||isRedLeftLeft(t)) ? t : (Basics.otherwise ? moveRedLeft(t) : _E.If('Line 286, Column 3')));};
  var deleteMin = function(t){
    return function(){
      var del = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBNode':
              switch (t._3.ctor) {
                case 'RBEmpty':
                  return RBEmpty;
              }break;
          }
          return function(){
            var case219 = moveRedLeftIfNeeded(t);
            switch (case219.ctor) {
              case 'RBEmpty':
                return RBEmpty;
              case 'RBNode':
                return fixUp(A5(RBNode, case219._0, case219._1, case219._2, del(case219._3), case219._4));
            }_E.Case('Line 297, Column 12')}();}();};
      return ensureBlackRoot(del(t));}();};
  var moveRedRight = function(t){
    return function(){
      var t$ = color_flip(t);
      return (isRedLeftLeft(t$) ? color_flip(rotateRight(t$)) : (Basics.otherwise ? t$ : _E.If('Line 282, Column 3')));}();};
  var moveRedRightIfNeeded = function(t){
    return ((isRedRight(t)||isRedRightLeft(t)) ? t : (Basics.otherwise ? moveRedRight(t) : _E.If('Line 290, Column 3')));};
  var remove = F2(function(k, t){
    return function(){
      var eq_and_noRightNode = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBNode':
              switch (t._4.ctor) {
                case 'RBEmpty':
                  return _N.eq(k,t._1);
              }break;
          }
          return false;}();};
      var eq = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBNode':
              return _N.eq(k,t._1);
          }
          return false;}();};
      var delEQ = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBEmpty':
              return Native.Error.raise(_str('delEQ called on a Empty'));
            case 'RBNode':
              return function(){
                var _243 = min(t._4);
                var k$ = function(){
                  switch (_243.ctor) {
                    case '_Tuple2':
                      return k$;
                  }_E.Case('Line 326, Column 53')}();
                var v$ = function(){
                  switch (_243.ctor) {
                    case '_Tuple2':
                      return v$;
                  }_E.Case('Line 326, Column 53')}();
                return fixUp(A5(RBNode, t._0, k$, v$, t._3, deleteMin(t._4)));}();
          }_E.Case('Line 325, Column 17')}();};
      var del = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBEmpty':
              return RBEmpty;
            case 'RBNode':
              return ((_N.cmp(k,t._1)<0) ? delLT(t) : (Basics.otherwise ? function(){
                var u = (isRedLeft(t) ? rotateRight(t) : (Basics.otherwise ? t : _E.If('Line 336, Column 33')));
                return (eq_and_noRightNode(u) ? RBEmpty : (Basics.otherwise ? function(){
                  var t$ = moveRedRightIfNeeded(t);
                  return (eq(t$) ? delEQ(t$) : (Basics.otherwise ? delGT(t$) : _E.If('Line 339, Column 29')));}() : _E.If('Line 337, Column 25')));}() : _E.If('Line 335, Column 21')));
          }_E.Case('Line 332, Column 15')}();};
      var delGT = function(t){
        return function(){
          switch (t.ctor) {
            case 'RBEmpty':
              return Native.Error.raise(_str('delGT called on a Empty'));
            case 'RBNode':
              return fixUp(A5(RBNode, t._0, t._1, t._2, t._3, del(t._4)));
          }_E.Case('Line 329, Column 17')}();};
      var delLT = function(t){
        return function(){
          var case262 = moveRedLeftIfNeeded(t);
          switch (case262.ctor) {
            case 'RBEmpty':
              return Native.Error.raise(_str('delLT on Empty'));
            case 'RBNode':
              return fixUp(A5(RBNode, case262._0, case262._1, case262._2, del(case262._3), case262._4));
          }_E.Case('Line 322, Column 17')}();};
      return (A2(member, k, t) ? ensureBlackRoot(del(t)) : (Basics.otherwise ? t : _E.If('Line 340, Column 7')));}();});
  var diff = F2(function(t1, t2){
    return A3(foldl, F3(function(k, v, t){
      return A2(remove, k, t);}), t1, t2);});
  return elm.Dict = {
    _op : _op, 
    empty : empty, 
    singleton : singleton, 
    insert : insert, 
    lookup : lookup, 
    findWithDefault : findWithDefault, 
    remove : remove, 
    member : member, 
    foldl : foldl, 
    foldr : foldr, 
    map : map, 
    union : union, 
    intersect : intersect, 
    diff : diff, 
    keys : keys, 
    values : values, 
    toList : toList, 
    fromList : fromList};};
Elm.Either = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var List = Elm.List(elm);
  var _op = {};
  var Right = function(a){
    return {ctor:"Right", _0:a};};
  var isRight = function(e){
    return function(){
      switch (e.ctor) {
        case 'Right':
          return true;
      }
      return false;}();};
  var Left = function(a){
    return {ctor:"Left", _0:a};};
  var consEither = F2(function(e, arg1){
    return function(){
      switch (arg1.ctor) {
        case '_Tuple2':
          return function(){
            switch (e.ctor) {
              case 'Left':
                return {ctor:"_Tuple2", _0:_L.Cons(e._0,arg1._0), _1:arg1._1};
              case 'Right':
                return {ctor:"_Tuple2", _0:arg1._0, _1:_L.Cons(e._0,arg1._1)};
            }_E.Case('Line 50, Column 5')}();
      }_E.Case('Line 50, Column 5')}();});
  var partition = function(es){
    return A3(List.foldr, consEither, {ctor:"_Tuple2", _0:_J.toList([]), _1:_J.toList([])}, es);};
  var consLeft = F2(function(e, vs){
    return function(){
      switch (e.ctor) {
        case 'Left':
          return _L.Cons(e._0,vs);
        case 'Right':
          return vs;
      }_E.Case('Line 40, Column 5')}();});
  var lefts = function(es){
    return A3(List.foldr, consLeft, _J.toList([]), es);};
  var consRight = F2(function(e, vs){
    return function(){
      switch (e.ctor) {
        case 'Left':
          return vs;
        case 'Right':
          return _L.Cons(e._0,vs);
      }_E.Case('Line 45, Column 5')}();});
  var rights = function(es){
    return A3(List.foldr, consRight, _J.toList([]), es);};
  var either = F3(function(f, g, e){
    return function(){
      switch (e.ctor) {
        case 'Left':
          return f(e._0);
        case 'Right':
          return g(e._0);
      }_E.Case('Line 16, Column 16')}();});
  var isLeft = function(e){
    return function(){
      switch (e.ctor) {
        case 'Left':
          return true;
      }
      return false;}();};
  return elm.Either = {
    _op : _op, 
    either : either, 
    isLeft : isLeft, 
    isRight : isRight, 
    lefts : lefts, 
    rights : rights, 
    partition : partition, 
    consLeft : consLeft, 
    consRight : consRight, 
    consEither : consEither, 
    Left : Left, 
    Right : Right};};
Elm.Http = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Http = Elm.Native.Http(elm);
  var _op = {};
  var send = Native.Http.send;
  var Waiting = {ctor:"Waiting"};
  var Success = function(a){
    return {ctor:"Success", _0:a};};
  var Request = F4(function(a, b, c, d){
    return {
      _:{
      },
      body:c,
      headers:d,
      url:b,
      verb:a};});
  var get = function(url){
    return A4(Request, _str('GET'), url, _str(''), _J.toList([]));};
  var sendGet = function(reqs){
    return send(A2(Signal.lift, get, reqs));};
  var post = F2(function(url, body){
    return A4(Request, _str('POST'), url, body, _J.toList([]));});
  var request = Request;
  var Failure = F2(function(a, b){
    return {ctor:"Failure", _0:a, _1:b};});
  return elm.Http = {
    _op : _op, 
    request : request, 
    get : get, 
    post : post, 
    send : send, 
    sendGet : sendGet, 
    Success : Success, 
    Waiting : Waiting, 
    Failure : Failure};};
Elm.JavaScript = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.JavaScript = Elm.Native.JavaScript(elm);
  var Basics = Elm.Basics(elm);
  var _op = {};
  var toString = Native.JavaScript.toString;
  var toList = Native.JavaScript.toList;
  var toInt = Native.JavaScript.toInt;
  var toFloat = Native.JavaScript.toFloat;
  var toBool = Native.JavaScript.toBool;
  var fromString = Native.JavaScript.fromString;
  var fromList = Native.JavaScript.fromList;
  var fromInt = Native.JavaScript.fromInt;
  var fromFloat = Native.JavaScript.fromFloat;
  var fromBool = Native.JavaScript.fromBool;
  var JSString = {ctor:"JSString"};
  var JSObject = {ctor:"JSObject"};
  var JSNumber = {ctor:"JSNumber"};
  var JSDomNode = {ctor:"JSDomNode"};
  var JSBool = {ctor:"JSBool"};
  var JSArray = function(a){
    return {ctor:"JSArray", _0:a};};
  return elm.JavaScript = {
    _op : _op, 
    toList : toList, 
    toInt : toInt, 
    toFloat : toFloat, 
    toBool : toBool, 
    toString : toString, 
    fromList : fromList, 
    fromInt : fromInt, 
    fromFloat : fromFloat, 
    fromBool : fromBool, 
    fromString : fromString, 
    JSNumber : JSNumber, 
    JSBool : JSBool, 
    JSString : JSString, 
    JSArray : JSArray, 
    JSDomNode : JSDomNode, 
    JSObject : JSObject};};
Elm.Json = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Dict = Elm.Dict(elm);
  var Maybe = Elm.Maybe(elm);
  var JavaScript = Elm.JavaScript(elm);
  var Native = Native || {};
  Native.Json = Elm.Native.Json(elm);
  var JavaScript = Elm.JavaScript(elm);
  var _op = {};
  var toString = F2(function(sep, v){
    return JavaScript.toString(A2(Native.Json.toJSString, sep, v));});
  var toJSString = Native.Json.toJSString;
  var toJSObject = Native.Json.toJSObject;
  var fromString = function(s){
    return Native.Json.fromJSString(JavaScript.fromString(s));};
  var fromJSString = Native.Json.fromJSString;
  var fromJSObject = Native.Json.fromJSObject;
  var String = function(a){
    return {ctor:"String", _0:a};};
  var Object = function(a){
    return {ctor:"Object", _0:a};};
  var Number = function(a){
    return {ctor:"Number", _0:a};};
  var Null = {ctor:"Null"};
  var Boolean = function(a){
    return {ctor:"Boolean", _0:a};};
  var Array = function(a){
    return {ctor:"Array", _0:a};};
  return elm.Json = {
    _op : _op, 
    toString : toString, 
    toJSString : toJSString, 
    fromString : fromString, 
    fromJSString : fromJSString, 
    fromJSObject : fromJSObject, 
    toJSObject : toJSObject, 
    String : String, 
    Number : Number, 
    Boolean : Boolean, 
    Null : Null, 
    Array : Array, 
    Object : Object};};
Elm.Keyboard = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Keyboard = Elm.Native.Keyboard(elm);
  var _op = {};
  var lastPressed = Native.Keyboard.lastPressed;
  var keysDown = Native.Keyboard.keysDown;
  var isDown = Native.Keyboard.isDown;
  var shift = isDown(16);
  var space = isDown(32);
  var enter = isDown(13);
  var directions = Native.Keyboard.directions;
  var wasd = A4(directions, 87, 83, 65, 68);
  var ctrl = isDown(17);
  var arrows = A4(directions, 38, 40, 37, 39);
  return elm.Keyboard = {
    _op : _op, 
    directions : directions, 
    arrows : arrows, 
    wasd : wasd, 
    isDown : isDown, 
    shift : shift, 
    ctrl : ctrl, 
    space : space, 
    enter : enter, 
    keysDown : keysDown, 
    lastPressed : lastPressed};};
Elm.List = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Native = Native || {};
  Native.List = Elm.Native.List(elm);
  var _op = {};
  var zipWith = Native.List.zipWith;
  var zip = Native.List.zip;
  var take = Native.List.take;
  var tail = Native.List.tail;
  var split = Native.List.split;
  var scanl1 = Native.List.scanl1;
  var scanl = Native.List.scanl;
  var reverse = Native.List.reverse;
  var or = Native.List.or;
  var map = Native.List.map;
  var length = Native.List.length;
  var last = Native.List.last;
  var join = Native.List.join;
  var isEmpty = function(xs){
    return function(){
      switch (xs.ctor) {
        case '[]':
          return true;
      }
      return false;}();};
  var head = Native.List.head;
  var foldr1 = Native.List.foldr1;
  var foldr = Native.List.foldr;
  var foldl1 = Native.List.foldl1;
  var maximum = foldl1(Basics.max);
  var minimum = foldl1(Basics.min);
  var foldl = Native.List.foldl;
  var product = A2(foldl, F2(function(x, y){
    return (x*y);}), 1);
  var sum = A2(foldl, F2(function(x, y){
    return (x+y);}), 0);
  var filter = Native.List.filter;
  var drop = Native.List.drop;
  var concat = Native.List.concat;
  var concatMap = F2(function(f, list){
    return concat(A2(map, f, list));});
  var any = Native.List.any;
  var and = Native.List.and;
  var all = Native.List.all;
  _op['::'] = Native.List.cons;
  var intersperse = F2(function(sep, xs){
    return function(){
      switch (xs.ctor) {
        case '::':
          switch (xs._1.ctor) {
            case '::':
              return _L.Cons(xs._0,_L.Cons(sep,A2(intersperse, sep, _L.Cons(xs._1._0,xs._1._1))));
            case '[]':
              return _J.toList([xs._0]);
          }break;
        case '[]':
          return _J.toList([]);
      }_E.Case('Line 176, Column 3')}();});
  var partition = F2(function(pred, lst){
    return function(){
      switch (lst.ctor) {
        case '::':
          return function(){
            var _9 = A2(partition, pred, lst._1);
            var bs = function(){
              switch (_9.ctor) {
                case '_Tuple2':
                  return _9._0;
              }_E.Case('Line 133, Column 30')}();
            var cs = function(){
              switch (_9.ctor) {
                case '_Tuple2':
                  return _9._1;
              }_E.Case('Line 133, Column 30')}();
            return (pred(lst._0) ? {ctor:"_Tuple2", _0:_L.Cons(lst._0,bs), _1:cs} : (Basics.otherwise ? {ctor:"_Tuple2", _0:bs, _1:_L.Cons(lst._0,cs)} : _E.If('Line 134, Column 16')));}();
        case '[]':
          return {ctor:"_Tuple2", _0:_J.toList([]), _1:_J.toList([])};
      }_E.Case('Line 131, Column 5')}();});
  var unzip = function(pairs){
    return function(){
      switch (pairs.ctor) {
        case '::':
          switch (pairs._0.ctor) {
            case '_Tuple2':
              return function(){
                var _21 = unzip(pairs._1);
                var xs = function(){
                  switch (_21.ctor) {
                    case '_Tuple2':
                      return _21._0;
                  }_E.Case('Line 156, Column 33')}();
                var ys = function(){
                  switch (_21.ctor) {
                    case '_Tuple2':
                      return _21._1;
                  }_E.Case('Line 156, Column 33')}();
                return {ctor:"_Tuple2", _0:_L.Cons(pairs._0._0,xs), _1:_L.Cons(pairs._0._1,ys)};}();
          }break;
        case '[]':
          return {ctor:"_Tuple2", _0:_J.toList([]), _1:_J.toList([])};
      }_E.Case('Line 154, Column 3')}();};
  _op['++'] = Native.List.append;
  return elm.List = {
    _op : _op, 
    head : head, 
    tail : tail, 
    last : last, 
    isEmpty : isEmpty, 
    map : map, 
    foldl : foldl, 
    foldr : foldr, 
    foldl1 : foldl1, 
    foldr1 : foldr1, 
    scanl : scanl, 
    scanl1 : scanl1, 
    filter : filter, 
    length : length, 
    reverse : reverse, 
    all : all, 
    any : any, 
    and : and, 
    or : or, 
    concat : concat, 
    concatMap : concatMap, 
    sum : sum, 
    product : product, 
    maximum : maximum, 
    minimum : minimum, 
    partition : partition, 
    zip : zip, 
    zipWith : zipWith, 
    unzip : unzip, 
    split : split, 
    join : join, 
    intersperse : intersperse, 
    take : take, 
    drop : drop};};
Elm.Matrix2D = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.Matrix2D = Elm.Native.Matrix2D(elm);
  var _op = {};
  var rotation = Native.Matrix2D.rotation;
  var multiply = Native.Matrix2D.multiply;
  var matrix = Native.Matrix2D.matrix;
  var identity = Native.Matrix2D.identity;
  var Matrix2D = {ctor:"Matrix2D"};
  return elm.Matrix2D = {
    _op : _op, 
    identity : identity, 
    matrix : matrix, 
    rotation : rotation, 
    multiply : multiply, 
    Matrix2D : Matrix2D};};
Elm.Maybe = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var List = Elm.List(elm);
  var _op = {};
  var Nothing = {ctor:"Nothing"};
  var Just = function(a){
    return {ctor:"Just", _0:a};};
  var maybe = F3(function(b, f, m){
    return function(){
      switch (m.ctor) {
        case 'Just':
          return f(m._0);
        case 'Nothing':
          return b;
      }_E.Case('Line 14, Column 15')}();});
  var cons = F2(function(mx, xs){
    return A3(maybe, xs, function(x){
      return _L.Cons(x,xs);}, mx);});
  var justs = A2(List.foldr, cons, _J.toList([]));
  var isJust = A2(maybe, false, function(arg1){
    return function(){
      return true;}();});
  var isNothing = function($){
    return Basics.not(isJust($));};
  return elm.Maybe = {
    _op : _op, 
    maybe : maybe, 
    isJust : isJust, 
    isNothing : isNothing, 
    cons : cons, 
    justs : justs, 
    Just : Just, 
    Nothing : Nothing};};
Elm.Mouse = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Mouse = Elm.Native.Mouse(elm);
  var _op = {};
  var y = Native.Mouse.y;
  var x = Native.Mouse.x;
  var position = Native.Mouse.position;
  var isDown = Native.Mouse.isDown;
  var isClicked = Native.Mouse.isClicked;
  var clicks = Native.Mouse.clicks;
  return elm.Mouse = {
    _op : _op, 
    position : position, 
    x : x, 
    y : y, 
    isDown : isDown, 
    isClicked : isClicked, 
    clicks : clicks};};
Elm.Random = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Random = Elm.Native.Random(elm);
  var _op = {};
  var range = Native.Random.range;
  var float = Native.Random.float;
  return elm.Random = {
    _op : _op, 
    range : range, 
    float : float};};
Elm.Set = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Maybe = Elm.Maybe(elm);
  var Dict = Elm.Dict(elm);
  var List = Elm.List(elm);
  var _op = {};
  var union = Dict.union;
  var toList = Dict.keys;
  var singleton = function(k){
    return A2(Dict.singleton, k, {ctor:"_Tuple0"});};
  var remove = Dict.remove;
  var member = Dict.member;
  var intersect = Dict.intersect;
  var insert = function(k){
    return A2(Dict.insert, k, {ctor:"_Tuple0"});};
  var foldr = F3(function(f, b, s){
    return A3(Dict.foldr, F3(function(k, arg2, b){
      return function(){
        return A2(f, k, b);}();}), b, s);});
  var foldl = F3(function(f, b, s){
    return A3(Dict.foldl, F3(function(k, arg2, b){
      return function(){
        return A2(f, k, b);}();}), b, s);});
  var empty = Dict.empty;
  var fromList = function(xs){
    return A3(List.foldl, insert, empty, xs);};
  var map = F2(function(f, s){
    return fromList(A2(List.map, f, toList(s)));});
  var diff = Dict.diff;
  return elm.Set = {
    _op : _op, 
    empty : empty, 
    singleton : singleton, 
    insert : insert, 
    remove : remove, 
    member : member, 
    foldl : foldl, 
    foldr : foldr, 
    map : map, 
    union : union, 
    intersect : intersect, 
    diff : diff, 
    toList : toList, 
    fromList : fromList};};
Elm.Signal = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Native = Native || {};
  Native.Signal = Elm.Native.Signal(elm);
  var List = Elm.List(elm);
  var _op = {};
  _op['~'] = F2(function(sf, s){
    return A3(Native.Signal.lift2, F2(function(f, x){
      return f(x);}), sf, s);});
  var sampleOn = Native.Signal.sampleOn;
  var merges = Native.Signal.merges;
  var merge = Native.Signal.merge;
  var lift8 = Native.Signal.lift8;
  var lift7 = Native.Signal.lift7;
  var lift6 = Native.Signal.lift6;
  var lift5 = Native.Signal.lift5;
  var lift4 = Native.Signal.lift4;
  var lift3 = Native.Signal.lift3;
  var lift2 = Native.Signal.lift2;
  var lift = Native.Signal.lift;
  var keepWhen = Native.Signal.keepWhen;
  var keepIf = Native.Signal.keepIf;
  var foldp = Native.Signal.foldp;
  var dropWhen = Native.Signal.dropWhen;
  var dropRepeats = Native.Signal.dropRepeats;
  var dropIf = Native.Signal.dropIf;
  var countIf = Native.Signal.countIf;
  var count = Native.Signal.count;
  var constant = Native.Signal.constant;
  var combine = A2(List.foldr, Native.Signal.lift2(F2(function(x, y){
    return _L.Cons(x,y);})), Native.Signal.constant(_J.toList([])));
  var Signal = {ctor:"Signal"};
  _op['<~'] = F2(function(f, s){
    return A2(Native.Signal.lift, f, s);});
  return elm.Signal = {
    _op : _op, 
    constant : constant, 
    lift : lift, 
    lift2 : lift2, 
    lift3 : lift3, 
    lift4 : lift4, 
    lift5 : lift5, 
    lift6 : lift6, 
    lift7 : lift7, 
    lift8 : lift8, 
    foldp : foldp, 
    merge : merge, 
    merges : merges, 
    combine : combine, 
    count : count, 
    countIf : countIf, 
    keepIf : keepIf, 
    dropIf : dropIf, 
    keepWhen : keepWhen, 
    dropWhen : dropWhen, 
    dropRepeats : dropRepeats, 
    sampleOn : sampleOn, 
    Signal : Signal};};
Elm.Text = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Color = Elm.Color(elm);
  var Graphics = Graphics || {};
  Graphics.Element = Elm.Graphics.Element(elm);
  var Maybe = Elm.Maybe(elm);
  var JavaScript = Elm.JavaScript(elm);
  var Native = Native || {};
  Native.Text = Elm.Native.Text(elm);
  var _op = {};
  var underline = Native.Text.underline;
  var typeface = Native.Text.typeface;
  var toText = Native.Text.toText;
  var text = Native.Text.text;
  var strikeThrough = Native.Text.strikeThrough;
  var righted = Native.Text.righted;
  var plainText = Native.Text.plainText;
  var overline = Native.Text.overline;
  var monospace = Native.Text.monospace;
  var link = Native.Text.link;
  var justified = Native.Text.justified;
  var italic = Native.Text.italic;
  var height = Native.Text.height;
  var header = Native.Text.header;
  var color = Native.Text.color;
  var centered = Native.Text.centered;
  var bold = Native.Text.bold;
  var asText = Native.Text.asText;
  var Text = {ctor:"Text"};
  return elm.Text = {
    _op : _op, 
    toText : toText, 
    typeface : typeface, 
    monospace : monospace, 
    header : header, 
    link : link, 
    height : height, 
    color : color, 
    bold : bold, 
    italic : italic, 
    overline : overline, 
    underline : underline, 
    strikeThrough : strikeThrough, 
    justified : justified, 
    centered : centered, 
    righted : righted, 
    text : text, 
    plainText : plainText, 
    asText : asText, 
    Text : Text};};
Elm.Time = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Native = Native || {};
  Native.Time = Elm.Native.Time(elm);
  var Signal = Elm.Signal(elm);
  var _op = {};
  var timestamp = Native.Time.timestamp;
  var since = Native.Time.since;
  var millisecond = 1;
  var second = (1000*millisecond);
  var minute = (60*second);
  var inSeconds = function(t){
    return (t/second);};
  var inMinutes = function(t){
    return (t/minute);};
  var inMilliseconds = function(t){
    return t;};
  var hour = (60*minute);
  var inHours = function(t){
    return (t/hour);};
  var fpsWhen = Native.Time.fpsWhen;
  var fps = Native.Time.fps;
  var every = Native.Time.every;
  var delay = Native.Time.delay;
  return elm.Time = {
    _op : _op, 
    millisecond : millisecond, 
    second : second, 
    minute : minute, 
    hour : hour, 
    inMilliseconds : inMilliseconds, 
    inSeconds : inSeconds, 
    inMinutes : inMinutes, 
    inHours : inHours, 
    fps : fps, 
    fpsWhen : fpsWhen, 
    every : every, 
    since : since, 
    timestamp : timestamp, 
    delay : delay};};
Elm.Touch = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Touch = Elm.Native.Touch(elm);
  var Time = Elm.Time(elm);
  var _op = {};
  var touches = Native.Touch.touches;
  var taps = Native.Touch.taps;
  var Touch = F6(function(a, b, c, d, e, f){
    return {
      _:{
      },
      id:c,
      t0:f,
      x:a,
      x0:d,
      y:b,
      y0:e};});
  return elm.Touch = {
    _op : _op, 
    touches : touches, 
    taps : taps};};
Elm.WebSocket = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Basics = Elm.Basics(elm);
  var Native = Native || {};
  Native.WebSocket = Elm.Native.WebSocket(elm);
  var _op = {};
  var connect = Native.WebSocket.connect;
  return elm.WebSocket = {
    _op : _op, 
    connect : connect};};
Elm.Window = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Window = Elm.Native.Window(elm);
  var _op = {};
  var width = Native.Window.width;
  var height = Native.Window.height;
  var dimensions = Native.Window.dimensions;
  return elm.Window = {
    _op : _op, 
    dimensions : dimensions, 
    width : width, 
    height : height};};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var List = Elm.List(elm);
  var Either = Elm.Either(elm);
  var Matrix2D = Elm.Matrix2D(elm);
  var Native = Native || {};
  Native.Graphics = Native.Graphics || {};
  Native.Graphics.Collage = Elm.Native.Graphics.Collage(elm);
  var Graphics = Graphics || {};
  Graphics.Element = Elm.Graphics.Element(elm);
  var Color = Elm.Color(elm);
  var Maybe = Elm.Maybe(elm);
  var JavaScript = Elm.JavaScript(elm);
  var _op = {};
  var segment = F2(function(p1, p2){
    return _J.toList([p1,p2]);});
  var scale = F2(function(s, f){
    return _N.replace([['scale',(f.scale*s)]], f);});
  var rotate = F2(function(t, f){
    return _N.replace([['theta',(f.theta+t)]], f);});
  var rect = F2(function(w, h){
    return function(){
      var hw = (w/2);
      var hh = (h/2);
      return _J.toList([{ctor:"_Tuple2", _0:(0-hw), _1:(0-hh)},{ctor:"_Tuple2", _0:(0-hw), _1:hh},{ctor:"_Tuple2", _0:hw, _1:hh},{ctor:"_Tuple2", _0:hw, _1:(0-hh)}]);}();});
  var square = function(n){
    return A2(rect, n, n);};
  var polygon = function(points){
    return points;};
  var path = function(ps){
    return ps;};
  var oval = F2(function(w, h){
    return function(){
      var n = 50;
      var t = ((2*Basics.pi)/n);
      var hw = (w/2);
      var hh = (h/2);
      var f = function(i){
        return {ctor:"_Tuple2", _0:(hw*Basics.cos((t*i))), _1:(hh*Basics.sin((t*i)))};};
      return A2(List.map, f, _L.range(0,(n-1)));}();});
  var ngon = F2(function(n, r){
    return function(){
      var m = Basics.toFloat(n);
      var t = ((2*Basics.pi)/m);
      var f = function(i){
        return {ctor:"_Tuple2", _0:(r*Basics.cos((t*i))), _1:(r*Basics.sin((t*i)))};};
      return A2(List.map, f, _L.range(0,(m-1)));}();});
  var moveY = F2(function(y, f){
    return _N.replace([['y',(f.y+y)]], f);});
  var moveX = F2(function(x, f){
    return _N.replace([['x',(f.x+x)]], f);});
  var move = F2(function(arg2, f){
    return function(){
      switch (arg2.ctor) {
        case '_Tuple2':
          return _N.replace([['x',(f.x+arg2._0)],['y',(f.y+arg2._1)]], f);
      }_E.Case('Line 142, Column 20')}();});
  var form = function(f){
    return {
      _:{
      },
      alpha:1,
      form:f,
      scale:1,
      theta:0,
      x:0,
      y:0};};
  var collage = Native.Graphics.Collage.collage;
  var circle = function(r){
    return A2(oval, (2*r), (2*r));};
  var alpha = F2(function(a, f){
    return _N.replace([['alpha',a]], f);});
  var Texture = function(a){
    return {ctor:"Texture", _0:a};};
  var Solid = function(a){
    return {ctor:"Solid", _0:a};};
  var Smooth = {ctor:"Smooth"};
  var Sharp = function(a){
    return {ctor:"Sharp", _0:a};};
  var Round = {ctor:"Round"};
  var Padded = {ctor:"Padded"};
  var LineStyle = F6(function(a, b, c, d, e, f){
    return {
      _:{
      },
      cap:c,
      color:a,
      dashOffset:f,
      dashing:e,
      join:d,
      width:b};});
  var Grad = function(a){
    return {ctor:"Grad", _0:a};};
  var Form = F6(function(a, b, c, d, e, f){
    return {
      _:{
      },
      alpha:e,
      form:f,
      scale:b,
      theta:a,
      x:c,
      y:d};});
  var Flat = {ctor:"Flat"};
  var defaultLine = {
    _:{
    },
    cap:Flat,
    color:Color.black,
    dashOffset:0,
    dashing:_J.toList([]),
    join:Sharp(10),
    width:1};
  var dashed = function(clr){
    return _N.replace([['color',clr],['dashing',_J.toList([8,4])]], defaultLine);};
  var dotted = function(clr){
    return _N.replace([['color',clr],['dashing',_J.toList([3,3])]], defaultLine);};
  var solid = function(clr){
    return _N.replace([['color',clr]], defaultLine);};
  var FShape = F2(function(a, b){
    return {ctor:"FShape", _0:a, _1:b};});
  var fill = F2(function(style, shape){
    return form(A2(FShape, Either.Right(style), shape));});
  var filled = F2(function(color, shape){
    return A2(fill, Solid(color), shape);});
  var gradient = F2(function(grad, shape){
    return A2(fill, Grad(grad), shape);});
  var textured = F2(function(src, shape){
    return A2(fill, Texture(src), shape);});
  var outlined = F2(function(style, shape){
    return form(A2(FShape, Either.Left(style), shape));});
  var FPath = F2(function(a, b){
    return {ctor:"FPath", _0:a, _1:b};});
  var traced = F2(function(style, path){
    return form(A2(FPath, style, path));});
  var FImage = F4(function(a, b, c, d){
    return {ctor:"FImage", _0:a, _1:b, _2:c, _3:d};});
  var sprite = F4(function(w, h, pos, src){
    return form(A4(FImage, w, h, pos, src));});
  var FGroup = F2(function(a, b){
    return {ctor:"FGroup", _0:a, _1:b};});
  var group = function(fs){
    return form(A2(FGroup, Matrix2D.identity, fs));};
  var groupTransform = F2(function(matrix, fs){
    return form(A2(FGroup, matrix, fs));});
  var FElement = function(a){
    return {ctor:"FElement", _0:a};};
  var toForm = function(e){
    return form(FElement(e));};
  var Clipped = {ctor:"Clipped"};
  elm.Graphics = elm.Graphics || {};
  return elm.Graphics.Collage = {
    _op : _op, 
    defaultLine : defaultLine, 
    solid : solid, 
    dashed : dashed, 
    dotted : dotted, 
    form : form, 
    fill : fill, 
    filled : filled, 
    textured : textured, 
    gradient : gradient, 
    outlined : outlined, 
    traced : traced, 
    sprite : sprite, 
    toForm : toForm, 
    group : group, 
    groupTransform : groupTransform, 
    rotate : rotate, 
    scale : scale, 
    move : move, 
    moveX : moveX, 
    moveY : moveY, 
    alpha : alpha, 
    collage : collage, 
    path : path, 
    segment : segment, 
    polygon : polygon, 
    rect : rect, 
    square : square, 
    oval : oval, 
    circle : circle, 
    ngon : ngon, 
    Solid : Solid, 
    Texture : Texture, 
    Grad : Grad, 
    Flat : Flat, 
    Round : Round, 
    Padded : Padded, 
    Smooth : Smooth, 
    Sharp : Sharp, 
    Clipped : Clipped, 
    FPath : FPath, 
    FShape : FShape, 
    FImage : FImage, 
    FElement : FElement, 
    FGroup : FGroup};};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Native = Native || {};
  Native.Utils = Elm.Native.Utils(elm);
  var JavaScript = Elm.JavaScript(elm);
  var List = Elm.List(elm);
  var Color = Elm.Color(elm);
  var Maybe = Elm.Maybe(elm);
  var _op = {};
  var widthOf = function(e){
    return e.props.width;};
  var tag = F2(function(name, e){
    return function(){
      var p = e.props;
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['tag',JavaScript.fromString(name)]], p)};}();});
  var sizeOf = function(e){
    return {ctor:"_Tuple2", _0:e.props.width, _1:e.props.height};};
  var opacity = F2(function(o, e){
    return function(){
      var p = e.props;
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['opacity',o]], p)};}();});
  var markdown = Native.Utils.undefined;
  var link = F2(function(href, e){
    return function(){
      var p = e.props;
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['href',JavaScript.fromString(href)]], p)};}();});
  var heightOf = function(e){
    return e.props.height;};
  var emptyStr = JavaScript.fromString(_str(''));
  var color = F2(function(c, e){
    return function(){
      var p = e.props;
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['color',Maybe.Just(c)]], p)};}();});
  var Z = {ctor:"Z"};
  var middleAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:Z,
      vertical:Z,
      x:x,
      y:y};});
  var Tiled = {ctor:"Tiled"};
  var Spacer = {ctor:"Spacer"};
  var Relative = function(a){
    return {ctor:"Relative", _0:a};};
  var middle = {
    _:{
    },
    horizontal:Z,
    vertical:Z,
    x:Relative(0.5),
    y:Relative(0.5)};
  var relative = Relative;
  var RawHtml = function(a){
    return {ctor:"RawHtml", _0:a};};
  var Properties = F8(function(a, b, c, d, e, f, g, h){
    return {
      _:{
      },
      color:e,
      height:c,
      hover:h,
      href:f,
      id:a,
      opacity:d,
      tag:g,
      width:b};});
  var newElement = F3(function(w, h, e){
    return {
      _:{
      },
      element:e,
      props:A8(Properties, Native.Utils.guid({ctor:"_Tuple0"}), w, h, 1, Maybe.Nothing, emptyStr, emptyStr, {ctor:"_Tuple0"})};});
  var spacer = F2(function(w, h){
    return A3(newElement, w, h, Spacer);});
  var Position = F4(function(a, b, c, d){
    return {
      _:{
      },
      horizontal:a,
      vertical:b,
      x:c,
      y:d};});
  var Plain = {ctor:"Plain"};
  var P = {ctor:"P"};
  var midRightAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:P,
      vertical:Z,
      x:x,
      y:y};});
  var midTopAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:Z,
      vertical:P,
      x:x,
      y:y};});
  var topRightAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:P,
      vertical:P,
      x:x,
      y:y};});
  var N = {ctor:"N"};
  var bottomLeftAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:N,
      vertical:N,
      x:x,
      y:y};});
  var bottomRightAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:P,
      vertical:N,
      x:x,
      y:y};});
  var midBottomAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:Z,
      vertical:N,
      x:x,
      y:y};});
  var midLeftAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:N,
      vertical:Z,
      x:x,
      y:y};});
  var topLeftAt = F2(function(x, y){
    return {
      _:{
      },
      horizontal:N,
      vertical:P,
      x:x,
      y:y};});
  var Image = F4(function(a, b, c, d){
    return {ctor:"Image", _0:a, _1:b, _2:c, _3:d};});
  var height = F2(function(nh, e){
    return function(){
      var p = e.props;
      var props = function(){
        var case0 = e.element;
        switch (case0.ctor) {
          case 'Image':
            return _N.replace([['width',((case0._1/case0._2|0)*nh)]], p);
        }
        return p;}();
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['height',nh]], p)};}();});
  var image = F3(function(w, h, src){
    return A3(newElement, w, h, A4(Image, Plain, w, h, JavaScript.fromString(src)));});
  var tiledImage = F3(function(w, h, src){
    return A3(newElement, w, h, A4(Image, Tiled, w, h, JavaScript.fromString(src)));});
  var width = F2(function(nw, e){
    return function(){
      var p = e.props;
      var props = function(){
        var case5 = e.element;
        switch (case5.ctor) {
          case 'Image':
            return _N.replace([['height',((case5._2/case5._1|0)*nw)]], p);
          case 'RawHtml':
            return _N.replace([['height',function(){
              var _11 = A2(Native.Utils.htmlHeight, nw, case5._0);
              var h = function(){
                switch (_11.ctor) {
                  case '_Tuple2':
                    return _11._1;
                }_E.Case('Line 41, Column 77')}();
              var w = function(){
                switch (_11.ctor) {
                  case '_Tuple2':
                    return _11._0;
                }_E.Case('Line 41, Column 77')}();
              return h;}()]], p);
        }
        return p;}();
      return {
        _:{
        },
        element:e.element,
        props:_N.replace([['width',nw]], props)};}();});
  var size = F3(function(w, h, e){
    return A2(height, h, A2(width, w, e));});
  var Flow = F2(function(a, b){
    return {ctor:"Flow", _0:a, _1:b};});
  var Fitted = {ctor:"Fitted"};
  var fittedImage = F3(function(w, h, src){
    return A3(newElement, w, h, A4(Image, Fitted, w, h, JavaScript.fromString(src)));});
  var Element = F2(function(a, b){
    return {
      _:{
      },
      element:b,
      props:a};});
  var DUp = {ctor:"DUp"};
  var up = DUp;
  var DRight = {ctor:"DRight"};
  var right = DRight;
  var beside = F2(function(lft, rht){
    return A3(newElement, (widthOf(lft)+widthOf(rht)), A2(Basics.max, heightOf(lft), heightOf(rht)), A2(Flow, right, _J.toList([lft,rht])));});
  var DOut = {ctor:"DOut"};
  var layers = function(es){
    return function(){
      var ws = A2(List.map, widthOf, es);
      var hs = A2(List.map, heightOf, es);
      return A3(newElement, List.maximum(ws), List.maximum(hs), A2(Flow, DOut, es));}();};
  var outward = DOut;
  var DLeft = {ctor:"DLeft"};
  var left = DLeft;
  var DIn = {ctor:"DIn"};
  var inward = DIn;
  var DDown = {ctor:"DDown"};
  var above = F2(function(hi, lo){
    return A3(newElement, A2(Basics.max, widthOf(hi), widthOf(lo)), (heightOf(hi)+heightOf(lo)), A2(Flow, DDown, _J.toList([hi,lo])));});
  var below = F2(function(lo, hi){
    return A3(newElement, A2(Basics.max, widthOf(hi), widthOf(lo)), (heightOf(hi)+heightOf(lo)), A2(Flow, DDown, _J.toList([hi,lo])));});
  var down = DDown;
  var flow = F2(function(dir, es){
    return function(){
      var ws = A2(List.map, widthOf, es);
      var newFlow = F2(function(w, h){
        return A3(newElement, w, h, A2(Flow, dir, es));});
      var hs = A2(List.map, heightOf, es);
      return (_N.eq(es,_J.toList([])) ? A2(spacer, 0, 0) : (Basics.otherwise ? function(){
        switch (dir.ctor) {
          case 'DDown':
            return A2(newFlow, List.maximum(ws), List.sum(hs));
          case 'DIn':
            return A2(newFlow, List.maximum(ws), List.maximum(hs));
          case 'DLeft':
            return A2(newFlow, List.sum(ws), List.maximum(hs));
          case 'DOut':
            return A2(newFlow, List.maximum(ws), List.maximum(hs));
          case 'DRight':
            return A2(newFlow, List.sum(ws), List.maximum(hs));
          case 'DUp':
            return A2(newFlow, List.maximum(ws), List.sum(hs));
        }_E.Case('Line 156, Column 3')}() : _E.If('Line 155, Column 3')));}();});
  var Custom = {ctor:"Custom"};
  var Cropped = function(a){
    return {ctor:"Cropped", _0:a};};
  var croppedImage = F4(function(pos, w, h, src){
    return A3(newElement, w, h, A4(Image, Cropped(pos), w, h, JavaScript.fromString(src)));});
  var Container = F2(function(a, b){
    return {ctor:"Container", _0:a, _1:b};});
  var container = F4(function(w, h, pos, e){
    return A3(newElement, w, h, A2(Container, pos, e));});
  var Absolute = function(a){
    return {ctor:"Absolute", _0:a};};
  var absolute = Absolute;
  var midLeft = _N.replace([['horizontal',N],['x',Absolute(0)]], middle);
  var midRight = _N.replace([['horizontal',P]], midLeft);
  var midTop = _N.replace([['vertical',P],['y',Absolute(0)]], middle);
  var midBottom = _N.replace([['vertical',N]], midTop);
  var topLeft = {
    _:{
    },
    horizontal:N,
    vertical:P,
    x:Absolute(0),
    y:Absolute(0)};
  var bottomLeft = _N.replace([['vertical',N]], topLeft);
  var bottomRight = _N.replace([['horizontal',P]], bottomLeft);
  var topRight = _N.replace([['horizontal',P]], topLeft);
  elm.Graphics = elm.Graphics || {};
  return elm.Graphics.Element = {
    _op : _op, 
    widthOf : widthOf, 
    heightOf : heightOf, 
    sizeOf : sizeOf, 
    width : width, 
    height : height, 
    size : size, 
    opacity : opacity, 
    color : color, 
    tag : tag, 
    link : link, 
    emptyStr : emptyStr, 
    newElement : newElement, 
    image : image, 
    fittedImage : fittedImage, 
    croppedImage : croppedImage, 
    tiledImage : tiledImage, 
    markdown : markdown, 
    container : container, 
    spacer : spacer, 
    flow : flow, 
    above : above, 
    below : below, 
    beside : beside, 
    layers : layers, 
    absolute : absolute, 
    relative : relative, 
    middle : middle, 
    topLeft : topLeft, 
    topRight : topRight, 
    bottomLeft : bottomLeft, 
    bottomRight : bottomRight, 
    midLeft : midLeft, 
    midRight : midRight, 
    midTop : midTop, 
    midBottom : midBottom, 
    middleAt : middleAt, 
    topLeftAt : topLeftAt, 
    topRightAt : topRightAt, 
    bottomLeftAt : bottomLeftAt, 
    bottomRightAt : bottomRightAt, 
    midLeftAt : midLeftAt, 
    midRightAt : midRightAt, 
    midTopAt : midTopAt, 
    midBottomAt : midBottomAt, 
    up : up, 
    down : down, 
    left : left, 
    right : right, 
    inward : inward, 
    outward : outward, 
    Image : Image, 
    Container : Container, 
    Flow : Flow, 
    Spacer : Spacer, 
    RawHtml : RawHtml, 
    Custom : Custom, 
    Plain : Plain, 
    Fitted : Fitted, 
    Cropped : Cropped, 
    Tiled : Tiled, 
    P : P, 
    Z : Z, 
    N : N, 
    Absolute : Absolute, 
    Relative : Relative, 
    DUp : DUp, 
    DDown : DDown, 
    DLeft : DLeft, 
    DRight : DRight, 
    DIn : DIn, 
    DOut : DOut};};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Input = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var Basics = Elm.Basics(elm);
  var Signal = Elm.Signal(elm);
  var Native = Native || {};
  Native.Graphics = Native.Graphics || {};
  Native.Graphics.Input = Elm.Native.Graphics.Input(elm);
  var List = Elm.List(elm);
  var Graphics = Graphics || {};
  Graphics.Element = Elm.Graphics.Element(elm);
  var Color = Elm.Color(elm);
  var Maybe = Elm.Maybe(elm);
  var JavaScript = Elm.JavaScript(elm);
  var _op = {};
  var id = function(x){
    return x;};
  var hoverables = Native.Graphics.Input.hoverables;
  var hoverable = function(elem){
    return function(){
      var pool = hoverables(false);
      return {ctor:"_Tuple2", _0:A2(pool.hoverable, id, elem), _1:pool.events};}();};
  var fields = Native.Graphics.Input.fields;
  var emptyFieldState = {
    _:{
    },
    selectionEnd:0,
    selectionStart:0,
    string:_str('')};
  var field = function(placeHolder){
    return function(){
      var tfs = fields(emptyFieldState);
      var changes = Signal.dropRepeats(tfs.events);
      return {ctor:"_Tuple2", _0:A2(Signal.lift, A2(tfs.field, id, placeHolder), changes), _1:Signal.dropRepeats(A2(Signal.lift, function(_){
        return _.string;}, changes))};}();};
  var password = function(placeHolder){
    return function(){
      var tfs = Native.Graphics.Input.passwords(emptyFieldState);
      var changes = Signal.dropRepeats(tfs.events);
      return {ctor:"_Tuple2", _0:A2(Signal.lift, A2(tfs.field, id, placeHolder), changes), _1:Signal.dropRepeats(A2(Signal.lift, function(_){
        return _.string;}, changes))};}();};
  var email = function(placeHolder){
    return function(){
      var tfs = Native.Graphics.Input.emails(emptyFieldState);
      var changes = Signal.dropRepeats(tfs.events);
      return {ctor:"_Tuple2", _0:A2(Signal.lift, A2(tfs.field, id, placeHolder), changes), _1:Signal.dropRepeats(A2(Signal.lift, function(_){
        return _.string;}, changes))};}();};
  var dropDown = Native.Graphics.Input.dropDown;
  var stringDropDown = function(strs){
    return dropDown(A2(List.map, function(s){
      return {ctor:"_Tuple2", _0:s, _1:s};}, strs));};
  var customButtons = Native.Graphics.Input.customButtons;
  var customButton = F3(function(up, hover, down){
    return function(){
      var pool = customButtons({ctor:"_Tuple0"});
      return {ctor:"_Tuple2", _0:A4(pool.customButton, {ctor:"_Tuple0"}, up, hover, down), _1:pool.events};}();});
  var checkboxes = Native.Graphics.Input.checkboxes;
  var checkbox = function(b){
    return function(){
      var cbs = checkboxes(b);
      return {ctor:"_Tuple2", _0:A2(Signal.lift, cbs.checkbox(id), cbs.events), _1:cbs.events};}();};
  var buttons = Native.Graphics.Input.buttons;
  var button = function(txt){
    return function(){
      var pool = buttons({ctor:"_Tuple0"});
      return {ctor:"_Tuple2", _0:A2(pool.button, {ctor:"_Tuple0"}, txt), _1:pool.events};}();};
  var FieldState = F3(function(a, b, c){
    return {
      _:{
      },
      selectionEnd:c,
      selectionStart:b,
      string:a};});
  elm.Graphics = elm.Graphics || {};
  return elm.Graphics.Input = {
    _op : _op, 
    id : id, 
    buttons : buttons, 
    button : button, 
    customButtons : customButtons, 
    customButton : customButton, 
    checkboxes : checkboxes, 
    checkbox : checkbox, 
    hoverables : hoverables, 
    hoverable : hoverable, 
    fields : fields, 
    emptyFieldState : emptyFieldState, 
    field : field, 
    password : password, 
    email : email, 
    dropDown : dropDown, 
    stringDropDown : stringDropDown};};
Elm.JavaScript = Elm.JavaScript || {};
Elm.JavaScript.Experimental = function(elm){
  var N = Elm.Native, _N = N.Utils(elm), _L = N.List(elm), _E = N.Error(elm), _J = N.JavaScript(elm), _str = _J.toString;
  var JavaScript = Elm.JavaScript(elm);
  var Native = Native || {};
  Native.JavaScript = Elm.Native.JavaScript(elm);
  var _op = {};
  var toRecord = Native.JavaScript.toRecord;
  var fromRecord = Native.JavaScript.fromRecord;
  elm.JavaScript = elm.JavaScript || {};
  return elm.JavaScript.Experimental = {
    _op : _op, 
    toRecord : toRecord, 
    fromRecord : fromRecord};};(function() {

// Returns boolean indicating if the swap was successful.
// Requires that the two signal graphs have exactly the same
// structure.
ElmRuntime.swap = function(from, to) {
    function similar(nodeOld,nodeNew) {
        idOkay = nodeOld.id === nodeNew.id;
        lengthOkay = nodeOld.kids.length === nodeNew.kids.length;
        return idOkay && lengthOkay;
    }
    function swap(nodeOld,nodeNew) {
        nodeNew.value = nodeOld.value;
        return true;
    }
    var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
    if (canSwap) { depthFirstTraversals(swap, from.inputs, to.inputs); }
    from.node.parentNode.replaceChild(to.node, from.node);
    return canSwap;
}

// Returns false if the node operation f ever fails.
function depthFirstTraversals(f, queueOld, queueNew) {
    if (queueOld.length !== queueNew.length) return false;
    queueOld = queueOld.slice(0);
    queueNew = queueNew.slice(0);

    var seen = [];
    while (queueOld.length > 0 && queueNew.length > 0) {
        var nodeOld = queueOld.pop();
        var nodeNew = queueNew.pop();
        if (seen.indexOf(nodeOld.id) < 0) {
            if (!f(nodeOld, nodeNew)) return false;
            queueOld = queueOld.concat(nodeOld.kids);
            queueNew = queueNew.concat(nodeNew.kids);
            seen.push(nodeOld.id);
        }
    }
    return true;
}

}());

(function() {
'use strict';

Elm.fullscreen = function(module) {
    var style = document.createElement('style');
    style.type = 'text/css';
    style.innerHTML = "html,head,body { padding:0; margin:0; }" +
        "body { font-family: calibri, helvetica, arial, sans-serif; }";
    document.head.appendChild(style);
    var container = document.createElement('div');
    document.body.appendChild(container);
    return init(ElmRuntime.Display.FULLSCREEN, container, module);
};

Elm.domNode = function(container, module) {
    var tag = container.tagName;
    if (tag !== 'DIV') {
        throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
    } else if (container.hasChildNodes()) {
        throw new Error('Elm.node must be given an empty DIV. No children allowed!');
    }
    return init(ElmRuntime.Display.COMPONENT, container, module);
};

Elm.worker = function(module) {
    return init(ElmRuntime.Display.NONE, {}, module);
};

function init(display, container, module, moduleToReplace) {
  // defining state needed for an instance of the Elm RTS
  var inputs = [];

  function notify(id, v) {
      var timestep = Date.now();
      var changed = false;
      for (var i = inputs.length; i--; ) {
          // order is important here to avoid short-circuiting
          changed = inputs[i].recv(timestep, id, v) || changed;
      }
      return changed;
  }

  container.offsetX = 0;
  container.offsetY = 0;

  var listeners = [];
  function addListener(relevantInputs, domNode, eventName, func) {
      domNode.addEventListener(eventName, func);
      var listener = {
          relevantInputs: relevantInputs,
          domNode: domNode,
          eventName: eventName,
          func: func
      };
      listeners.push(listener);
  }

  // create the actual RTS. Any impure modules will attach themselves to this
  // object. This permits many Elm programs to be embedded per document.
  var elm = {
      notify:notify,
      node:container,
      display:display,
      id:ElmRuntime.guid(),
      addListener:addListener,
      inputs:inputs
  };

  // Set up methods to communicate with Elm program from JS.
  function send(name, value) {
      if (typeof value === 'undefined') return function(v) { return send(name,v); };
      var e = document.createEvent('Event');
      e.initEvent(name + '_' + elm.id, true, true);
      e.value = value;
      document.dispatchEvent(e);
  }
  function recv(name, handler) {
      document.addEventListener(name + '_' + elm.id, handler);
  }

  recv('log', function(e) {console.log(e.value)});
  recv('title', function(e) {document.title = e.value});
  recv('redirect', function(e) {
    if (e.value.length > 0) { window.location = e.value; }
  });

  function swap(newModule) {
      removeListeners(listeners);
      var div = document.createElement('div');
      var newElm = init(display, div, newModule, elm);
      inputs = [];
      // elm.send = newElm.send;
      // elm.recv = newElm.recv;
      // elm.swap = newElm.swap;
      return newElm;
  }

  var Module = module(elm);
  inputs = ElmRuntime.filterDeadInputs(inputs);
  filterListeners(inputs, listeners);
  if (display !== ElmRuntime.Display.NONE) {
      var graphicsNode = initGraphics(elm, Module);
  }
  if (typeof moduleToReplace !== 'undefined') {
      ElmRuntime.swap(moduleToReplace, elm);

      // rerender scene if graphics are enabled.
      if (typeof graphicsNode !== 'undefined') {
          graphicsNode.value = A2( Elm.Graphics.Element(elm).spacer, 0, 0 );
          graphicsNode.recv(0, true, 0);
      }
  }

  return { send:send, recv:recv, swap:swap };
};

function filterListeners(inputs, listeners) {
    loop:
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        for (var j = inputs.length; j--; ) {
            if (listener.relevantInputs.indexOf(inputs[j].id) >= 0) {
                continue loop;
            }
        }
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function removeListeners(listeners) {
    for (var i = listeners.length; i--; ) {
        var listener = listeners[i];
        listener.domNode.removeEventListener(listener.eventName, listener.func);
    }
}

function initGraphics(elm, Module) {
  if (!('main' in Module))
      throw new Error("'main' is missing! What do I display?!");

  var signalGraph = Module.main;

  // make sure the signal graph is actually a signal & extract the visual model
  var Signal = Elm.Signal(elm);
  if (!('recv' in signalGraph)) {
      signalGraph = Signal.constant(signalGraph);
  }
  var currentScene = signalGraph.value;
  
  // Add the currentScene to the DOM
  var Render = ElmRuntime.use(ElmRuntime.Render.Element);
  elm.node.appendChild(Render.render(currentScene));
  if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
  
  // set up updates so that the DOM is adjusted as necessary.
  function domUpdate(newScene, currentScene) {
      ElmRuntime.draw(function(_) {
              Render.update(elm.node.firstChild, currentScene, newScene);
              if (elm.Native.Window) elm.Native.Window.resizeIfNeeded();
          });
      return newScene;
  }
  return A3(Signal.foldp, F2(domUpdate), currentScene, signalGraph);
}

}());
(function() {
'use strict';

ElmRuntime.Display = { FULLSCREEN: 0, COMPONENT: 1, NONE: 2 };

ElmRuntime.counter = 0;
ElmRuntime.guid = function() { return ElmRuntime.counter++; }

ElmRuntime.use = function(M) {
    if (typeof M === 'function') M = M();
    return M;
};

function isAlive(input) {
    if (!('defaultNumberOfKids' in input)) return true;
    var len = input.kids.length;
    if (len === 0) return false;
    if (len > input.defaultNumberOfKids) return true;
    var alive = false;
    for (var i = len; i--; ) {
        alive = alive || isAlive(input.kids[i]);
    }
    return alive;
}

ElmRuntime.filterDeadInputs = function(inputs) {
    var temp = [];
    for (var i = inputs.length; i--; ) {
        if (isAlive(inputs[i])) temp.push(inputs[i]);
    }
    return temp;
};

// define the draw function
var vendors = ['ms', 'moz', 'webkit', 'o'];
for (var i = 0; i < vendors.length && !window.requestAnimationFrame; ++i) {
    window.requestAnimationFrame = window[vendors[i]+'RequestAnimationFrame'];
    window.cancelAnimationFrame  = window[vendors[i]+'CancelAnimationFrame'] ||
                                   window[vendors[i]+'CancelRequestAnimationFrame'];
}

if (window.requestAnimationFrame && window.cancelAnimationFrame) {
    var previous = 0;
    ElmRuntime.draw = function(callback) {
        window.cancelAnimationFrame(previous);
        previous = window.requestAnimationFrame(callback);
    };
} else {
    ElmRuntime.draw = function(callback) { callback(); };
}

}());

ElmRuntime.Render.Collage = function() {
'use strict';

var Render = ElmRuntime.use(ElmRuntime.Render.Element);
var Matrix = Elm.Matrix2D({});
var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement,
    extract = Utils.extract, fromList = Utils.fromList,
    fromString = Utils.fromString, addTransform = Utils.addTransform;

function trace(ctx, path) {
    var points = fromList(path);
    var i = points.length - 1;
    if (i <= 0) return;
    ctx.moveTo(points[i]._0, points[i]._1);
    while (i--) { ctx.lineTo(points[i]._0, points[i]._1); }
    if (path.closed) {
        i = points.length - 1;
        ctx.lineTo(points[i]._0, points[i]._1);
    }
}

function line(ctx,style,path) {
    style.dashing.ctor === 'Nil' ? trace(ctx, path) : customLineHelp(ctx, style, path);
    ctx.scale(1,-1);
    ctx.stroke();
}

function customLineHelp(ctx, style, path) {
    var points = fromList(path);
    if (path.closed) points.push(points[0]);
    var pattern = fromList(style.dashing);
    var i = points.length - 1;
    if (i <= 0) return;
    var x0 = points[i]._0, y0 = points[i]._1;
    var x1=0, y1=0, dx=0, dy=0, remaining=0, nx=0, ny=0;
    var pindex = 0, plen = pattern.length;
    var draw = true, segmentLength = pattern[0];
    ctx.moveTo(x0,y0);
    while (i--) {
        x1 = points[i]._0; y1 = points[i]._1;
        dx = x1 - x0; dy = y1 - y0;
        remaining = Math.sqrt(dx * dx + dy * dy);
        while (segmentLength <= remaining) {
            x0 += dx * segmentLength / remaining;
            y0 += dy * segmentLength / remaining;
            ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
            // update starting position
            dx = x1 - x0; dy = y1 - y0;
            remaining = Math.sqrt(dx * dx + dy * dy);
            // update pattern
            draw = !draw;
            pindex = (pindex + 1) % plen;
            segmentLength = pattern[pindex];
        }
        if (remaining > 0) {
            ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
            segmentLength -= remaining;
        }
        x0 = x1; y0 = y1;
    }
}

function drawLine(ctx, style, path) {
    ctx.lineWidth = style.width;
    var cap = style.cap.ctor;
    ctx.lineCap = cap === 'Flat' ? 'butt' :
                  cap === 'Round' ? 'round' : 'square';
    var join = style.join.ctor;
    ctx.lineJoin = join === 'Smooth' ? 'round' :
                   join === 'Sharp' ? 'miter' : 'bevel';
    ctx.miterLimit = style.join._0 || 10;
    ctx.strokeStyle = extract(style.color);
    return line(ctx, style, path);
}

function texture(redo, ctx, src) {
    var img = new Image();
    img.src = fromString(src);
    img.onload = redo;
    return ctx.createPattern(img, 'repeat');
}

function gradient(ctx, grad) {
  var g;
  var stops = [];
  if (grad.ctor === 'Linear') {
    var p0 = grad._0, p1 = grad._1;
    g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
    stops = fromList(grad._2);
  } else {
    var p0 = grad._0, p2 = grad._2;
    g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
    stops = fromList(grad._4);
  }
  var len = stops.length;
  for (var i = 0; i < len; ++i) {
    var stop = stops[i];
    g.addColorStop(stop._0, extract(stop._1));
  }
  return g;
}

function drawShape(redo, ctx, style, path) {
    trace(ctx, path);
    var sty = style.ctor;
    ctx.fillStyle =
        sty === 'Solid' ? extract(style._0) :
        sty === 'Texture' ? texture(redo, ctx, style._0) : gradient(ctx, style._0);
    ctx.scale(1,-1);
    ctx.fill();
}

function drawImage(redo, ctx, form) {
    var img = new Image();
    img.onload = redo;
    img.src = fromString(form._3);
    var w = form._0,
        h = form._1,
        pos = form._2,
        srcX = pos._0,
        srcY = pos._1,
        srcW = w,
        srcH = h,
        destX = -w/2,
        destY = -h/2,
        destW = w,
        destH = h;

    ctx.scale(1,-1);
    ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
}

function renderForm(redo, ctx, form) {
    ctx.save();
    var x = form.x, y = form.y, theta = form.theta, scale = form.scale;
    if (x !== 0 || y !== 0) ctx.translate(x, y);
    if (theta !== 0) ctx.rotate(theta);
    if (scale !== 1) ctx.scale(scale,scale);
    if (form.alpha !== 1) ctx.globalAlpha = ctx.globalAlpha * form.alpha;
    ctx.beginPath();
    var f = form.form;
    switch(f.ctor) {
    case 'FPath' : drawLine(ctx, f._0, f._1); break;
    case 'FImage': drawImage(redo, ctx, f); break;
    case 'FShape':
	if (f._0.ctor === 'Left') {
	    f._1.closed = true;
	    drawLine(ctx, f._0._0, f._1);
	} else {
            drawShape(redo, ctx, f._0._0, f._1);
        }
	break;
    }
    ctx.restore();
}

function formToMatrix(form) {
   var scale = form.scale;
   var matrix = A6( Matrix.matrix, scale, 0, 0, scale, form.x, form.y );

   var theta = form.theta
   if (theta !== 0)
       matrix = A2( Matrix.multiply, matrix, Matrix.rotation(theta) );

   return matrix;
}

function makeTransform(w, h, form, matrices) {
    var props = form.form._0.props;
    var m = A6( Matrix.matrix, 1, 0, 0, 1,
                (w - props.width)/2,
                (h - props.height)/2 );
    var len = matrices.length;
    for (var i = 0; i < len; ++i) { m = A2( Matrix.multiply, m, matrices[i] ); }
    m = A2( Matrix.multiply, m, formToMatrix(form) );

    return 'matrix(' +   m[0]  + ',' +   m[3]  + ',' +
                       (-m[1]) + ',' + (-m[4]) + ',' +
                         m[2]  + ',' +   m[5]  + ')';
}

function stepperHelp(list) {
    var arr = fromList(list);
    var i = 0;
    function peekNext() {
        return i < arr.length ? arr[i].form.ctor : '';
    }
    // assumes that there is a next element
    function next() {
        var out = arr[i];
        ++i;
        return out;
    }
    return { peekNext:peekNext, next:next };
}

function stepper(forms) {
    var ps = [stepperHelp(forms)];
    var matrices = [];
    var alphas = [];
    function peekNext() {
        var len = ps.length;
        var formType = '';
        for (var i = 0; i < len; ++i ) {
            if (formType = ps[i].peekNext()) return formType;
        }
        return '';
    }
    // assumes that there is a next element
    function next(ctx) {
        while (!ps[0].peekNext()) {
            ps.shift();
            matrices.pop();
            alphas.shift();
            if (ctx) { ctx.restore(); }
        }
        var out = ps[0].next();
        var f = out.form;
        if (f.ctor === 'FGroup') {
            ps.unshift(stepperHelp(f._1));
            var m = A2(Matrix.multiply, f._0, formToMatrix(out));
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
            matrices.push(m);

            var alpha = (alphas[0] || 1) * out.alpha;
            alphas.unshift(alpha);
            ctx.globalAlpha = alpha;
        }
        return out;
    }
    function transforms() { return matrices; }
    function alpha() { return alphas[0] || 1; }
    return { peekNext:peekNext, next:next, transforms:transforms, alpha:alpha };
}

function makeCanvas(w,h) {
    var canvas = newElement('canvas');
    canvas.style.width  = w + 'px';
    canvas.style.height = h + 'px';
    canvas.style.display = "block";
    canvas.style.position = "absolute";
    canvas.width  = w;
    canvas.height = h;
    return canvas;
}

function render(model) {
    var div = newElement('div');
    update(div, model, model);
    return div;
}

function updateTracker(w,h,div) {
    var kids = div.childNodes;
    var i = 0;
    function transform(transforms, ctx) {
        ctx.translate(w/2, h/2);
        ctx.scale(1,-1);
        var len = transforms.length;
        for (var i = 0; i < len; ++i) {
            var m = transforms[i];
            ctx.save();
            ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
        }
        return ctx;
    }
    function getContext(transforms) {
        while (i < kids.length) {
            var node = kids[i];
            if (node.getContext) {
                node.width = w;
                node.height = h;
                node.style.width = w + 'px';
                node.style.height = h + 'px';
                ++i;
                return transform(transforms, node.getContext('2d'));
            }
            div.removeChild(node);
        }
        var canvas = makeCanvas(w,h);
        div.appendChild(canvas);
        // we have added a new node, so we must step our position
        ++i;
        return transform(transforms, canvas.getContext('2d'));
    }
    function element(matrices, alpha, form) {
        var container = kids[i];
        if (!container || container.getContext) {
            container = newElement('div');
            container.style.overflow = 'hidden';
            container.style.position = 'absolute';
            addTransform(container.style, 'scaleY(-1)');
            
            var kid = kids[i];
            kid ? div.insertBefore(container, kid)
                : div.appendChild(container);
        }
        // we have added a new node, so we must step our position
        ++i;

        container.style.width = w + 'px';
        container.style.height = h + 'px';
        container.style.opacity = alpha * form.alpha;

        var elem = form.form._0;
        var node = container.firstChild;
        if (node) {
            Render.update(node, node.oldElement, elem);
            node = container.firstChild;
        } else {
            node = Render.render(elem);
            container.appendChild(node);
        }
        node.oldElement = elem;
        addTransform(node.style, makeTransform(w, h, form, matrices));
    }
    function clearRest() {
        while (i < kids.length) {
            div.removeChild(kids[i]);
        }
    }
    return { getContext:getContext, element:element, clearRest:clearRest };
}


function update(div, _, model) {
    var w = model.w;
    var h = model.h;
    div.style.width = w + 'px';
    div.style.height = h + 'px';
    if (model.forms.ctor === 'Nil') {
        while (div.hasChildNodes()) {
            div.removeChild(div.lastChild);
        }
    }
    var stpr = stepper(model.forms);
    var tracker = updateTracker(w,h,div);
    var ctx = null;
    var formType = '';

    while (formType = stpr.peekNext()) {
        if (ctx === null && formType !== 'FElement') {
            ctx = tracker.getContext(stpr.transforms());
            ctx.globalAlpha = stpr.alpha();
        }
        var form = stpr.next(ctx);
        if (formType === 'FElement') {
            tracker.element(stpr.transforms(), stpr.alpha(), form);
            ctx = null;
        } else if (formType !== 'FGroup') {
            renderForm(function() { update(div, model, model); }, ctx, form);
        }
    }
    tracker.clearRest();
    return false;
}

return { render:render, update:update };

};

ElmRuntime.Render.Element = function() {
'use strict';

var Utils = ElmRuntime.use(ElmRuntime.Render.Utils);
var newElement = Utils.newElement, extract = Utils.extract,
    addTransform = Utils.addTransform, removeTransform = Utils.removeTransform,
    fromList = Utils.fromList, eq = Utils.eq;

function setProps(props, e) {
    e.style.width  = (props.width |0) + 'px';
    e.style.height = (props.height|0) + 'px';
    if (props.opacity !== 1) { e.style.opacity = props.opacity; }
    if (props.color.ctor === 'Just') {
        e.style.backgroundColor = extract(props.color._0);
    }
    if (props.tag !== '') { e.id = props.tag; }
    if (props.href !== '') {
        var a = newElement('a');
        a.href = props.href;
        a.style.width = '100%';
        a.style.height = '100%';
        a.style.top = 0;
        a.style.left = 0;
        a.style.display = 'block';
        e.appendChild(a);
    }
    if (props.hover.ctor !== '_Tuple0') {
        var overCount = 0;
        e.addEventListener('mouseover', function() {
            if (overCount++ > 0) return;
            props.hover(true);
        });
        e.addEventListener('mouseout', function(evt) {
            if (e.contains(evt.toElement || evt.relatedTarget)) return;
            overCount = 0;
            props.hover(false);
        });
    }
    return e;
}

function image(props, img) {
    switch (img._0.ctor) {
    case 'Plain':   return plainImage(img._3);
    case 'Fitted':  return fittedImage(props.width, props.height, img._3);
    case 'Cropped': return croppedImage(img,props.width,props.height,img._3);
    case 'Tiled':   return tiledImage(img._3);
    }
}

function plainImage(src) {
    var img = newElement('img');
    img.src = src;
    img.name = src;
    img.style.display = "block";
    return img;
}

function tiledImage(src) {
    var div = newElement('div');
    div.style.backgroundImage = 'url(' + src + ')';
    return div;
}

function fittedImage(w, h, src) {
    var div = newElement('div');
    div.style.background = 'url(' + src + ') no-repeat center';
    div.style.webkitBackgroundSize = 'cover';
    div.style.MozBackgroundSize = 'cover';
    div.style.OBackgroundSize = 'cover';
    div.style.backgroundSize = 'cover';
    return div;
}

function croppedImage(elem, w, h, src) {
    var pos = elem._0._0;
    var e = newElement('div');
    e.style.overflow = "hidden";

    var img = newElement('img');
    img.onload = function() {
        var sw = w / elem._1, sh = h / elem._2;
        img.style.width = ((this.width * sw)|0) + 'px';
        img.style.height = ((this.height * sh)|0) + 'px';
        img.style.marginLeft = ((- pos._0 * sw)|0) + 'px';
        img.style.marginTop = ((- pos._1 * sh)|0) + 'px';
    };
    img.src = src;
    img.name = src;
    e.appendChild(img);
    return e;
}

function goIn(e) { e.style.position = 'absolute'; return e; }
function goDown(e) { return e }
function goRight(e) { e.style.styleFloat = e.style.cssFloat = "left"; return e; }
function flowWith(f, array) {
    var container = newElement('div');
    for (var i = array.length; i--; ) {
        container.appendChild(f(render(array[i])));
    }
    return container;
}

function flow(dir,elist) {
    var array = fromList(elist);
    switch(dir.ctor) {
    case "DDown":  array.reverse();
    case "DUp":    return flowWith(goDown,array);
    case "DRight": array.reverse();
    case "DLeft":  return flowWith(goRight,array);
    case "DOut":   array.reverse();
    case "DIn":    return flowWith(goIn,array);
    }
}

function toPos(pos) {
    switch(pos.ctor) {
    case "Absolute": return  pos._0 + "px";
    case "Relative": return (pos._0 * 100) + "%";
    }
}

function setPos(pos,w,h,e) {
    e.style.position = 'absolute';
    e.style.margin = 'auto';
    var transform = '';
    switch(pos.horizontal.ctor) {
    case 'P': e.style.right = toPos(pos.x); break;
    case 'Z': transform = 'translateX(' + ((-w/2)|0) + 'px) ';
    case 'N': e.style.left = toPos(pos.x); break;
    }
    switch(pos.vertical.ctor) {
    case 'N': e.style.bottom = toPos(pos.y); break;
    case 'Z': transform += 'translateY(' + ((-h/2)|0) + 'px)';
    case 'P': e.style.top = toPos(pos.y); break;
    }
    if (transform !== '') addTransform(e.style, transform);
    return e;
}

function container(pos,elem) {
    var e = render(elem);
    setPos(pos, elem.props.width, elem.props.height, e);
    var div = newElement('div');
    div.style.position = 'relative';
    div.style.overflow = 'hidden';
    div.appendChild(e);
    return div;
}

function rawHtml(html) {
    var e = newElement('div');
    e.innerHTML = html;
    return e;
}

function render(elem) { return setProps(elem.props, makeElement(elem)); }
function makeElement(e) {
    var elem = e.element;
    switch(elem.ctor) {
    case 'Image':     return image(e.props, elem);
    case 'Flow':      return flow(elem._0, elem._1);
    case 'Container': return container(elem._0, elem._1);
    case 'Spacer':    return newElement('div');
    case 'RawHtml':   return rawHtml(elem._0);
    case 'Custom':    return elem.render(elem.model);
    }
}

function update(node, curr, next) {
    if (node.tagName === 'A') { node = node.firstChild; }
    if (curr.props.id === next.props.id) return updateProps(node, curr, next);
    if (curr.element.ctor !== next.element.ctor) {
        node.parentNode.replaceChild(render(next),node);
        return true;
    }
    var nextE = next.element, currE = curr.element;
    switch(nextE.ctor) {
    case "Spacer": break;
    case "RawHtml":
        if (nextE._0 !== currE._0) node.innerHTML = nextE._0;
        break;
    case "Image":
        if (nextE._0.ctor === 'Plain') {
            if (nextE._3 !== currE._3) node.src = nextE._3;
        } else if (!eq(nextE,currE) ||
                   next.props.width !== curr.props.width ||
                   next.props.height !== curr.props.height) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        break;
    case "Flow":
        var arr = fromList(nextE._1);
        for (var i = arr.length; i--; ) { arr[i] = arr[i].element.ctor; }
        if (nextE._0.ctor !== currE._0.ctor) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var nexts = fromList(nextE._1);
        var kids = node.childNodes;
        if (nexts.length !== kids.length) {
            node.parentNode.replaceChild(render(next),node);
            return true;
        }
        var currs = fromList(currE._1);
        var goDir = function(x) { return x; };
        switch(nextE._0.ctor) {
        case "DDown":  case "DUp":   goDir = goDown; break;
        case "DRight": case "DLeft": goDir = goRight; break;
        case "DOut":   case "DIn":   goDir = goIn; break;
        }
        for (var i = kids.length; i-- ;) {
            update(kids[i],currs[i],nexts[i]);
            goDir(kids[i]);
        }
        break;
    case "Container":
        var inner = node.firstChild;
        if (!update(inner, currE._1, nextE._1)) {
            if (nextE._0.horizontal.ctor !== currE._0.horizontal.ctor) {
                inner.style.left = inner.style.right = 'none';
                removeTransform(inner.style);
            }
            if (nextE._0.vertical.ctor !== currE._0.vertical.ctor) {
                inner.style.top = inner.style.bottom = 'none';
                removeTransform(inner.style);
            }
        }
        setPos(nextE._0, nextE._1.props.width, nextE._1.props.height, inner);
        break;
    case "Custom":
        if (currE.type === nextE.type) {
            var done = nextE.update(node, currE.model, nextE.model);
            if (done) return;
        } else {
            return node.parentNode.replaceChild(render(next), node);
        }
    }
    updateProps(node, curr, next);
}

function updateProps(node, curr, next) {
    var props = next.props, currP = curr.props, e = node;
    if (props.width !== currP.width)   e.style.width  = (props.width |0) + 'px';
    if (props.height !== currP.height) e.style.height = (props.height|0) + 'px';
    if (props.opacity !== 1 && props.opacity !== currP.opacity) {
        e.style.opacity = props.opacity;
    }
    var nextColor = (props.color.ctor === 'Just' ?
                     extract(props.color._0) : 'transparent');
    if (e.style.backgroundColor !== nextColor) {
        e.style.backgroundColor = nextColor;
    }
    if (props.tag !== currP.tag) { e.id = props.tag; }
    if (props.href !== currP.href) {
        if (currP.href === '') {
            var a = newElement('a');
            a.href = props.href;
            a.appendChild(e);
            e.parentNode.replaceChild(a,e);
        } else {
            node.parentNode.href = props.href;
        }
    }
}

return { render:render, update:update };

};
ElmRuntime.Render.Utils = function() {
'use strict';

function newElement(elementType) {
    var e = document.createElement(elementType);    
    e.style.padding = "0";
    e.style.margin = "0";
    return e;
}

function addTo(container, elem) {
    container.appendChild(elem);
}

function extract(c) {
    if (c._3 === 1) { return 'rgb(' + c._0 + ',' + c._1 + ',' + c._2 + ')'; }
    return 'rgba(' + c._0 + ',' + c._1 + ',' + c._2 + ',' + c._3 + ')';
}

function addTransform(style, trans) {
  style.transform       = trans;
  style.msTransform     = trans;
  style.MozTransform    = trans;
  style.webkitTransform = trans;
  style.OTransform      = trans;
}

function removeTransform(style) {
  style.transform       = 'none';
  style.msTransform     = 'none';
  style.MozTransform    = 'none';
  style.webkitTransform = 'none';
  style.OTransform      = 'none';
}

var List = Elm.Native.List({});

return {addTo:addTo,
	newElement:newElement,
	extract : extract,
	fromList: List.toArray,
	fromString: function(s) { return List.toArray(s).join(''); },
	toString: List.fromArray,
	eq: Elm.Native.Utils({}).eq,
	addTransform: addTransform,
	removeTransform: removeTransform
	};
};
