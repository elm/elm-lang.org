
function p(x,y) { return { fst: x, snd: y }; }

var pairs = [ p(/\\/g    , "\u03BB" ),
	      p(/->/g    , "\u2192" ),
	      p(/\*/g    , "\u00B7" ),
	      p(/<=/g    , "\u2264" ),
	      p(/>=/g    , "\u2265" ),
	      p(/==/g    , "\u2261" ),
	      p(/\/=/g   , "\u2262" ),
	      p(/\^2/g   , "\u00B2" ),
	      // p(/::/g    , "\u2237" ),
	      p(/\'a/g   , "\u03B1" ),
	      p(/\'b/g   , "\u03B2" ),
	      p(/\'\'\'/g, "\u2034" ),
	      p(/\'\'/g  , "\u2033" ),
	      p(/\'/g    , "\u2032" ),
	      p(/_1/g    , "\u2081" ),
	      p(/_2/g    , "\u2082" ),
	      p(/ \. /g  , " \u2218 "),
	      p(/\$/g    , "\u22EE" ),
	      p(/ pi /g  , " \u03C0 "),
	      p(/infinity/g   , "\u221E") ];

function rewrite(str) {
    for (var p in pairs) {
	str = str.replace(pairs[p].fst, pairs[p].snd);
    }
    return str;
}