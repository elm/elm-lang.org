#!/bin/bash

set -e


## MAKE EXAMPLE HTML


# ARGS:
#   $1 = _site/examples/NAME.html
#   $2 = <title>
#   $3 = NAME
#   $4 = code
#
function makeExampleHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <title>$2 example | Elm</title>
  <link rel="stylesheet" rel="preload" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Courier+Prime&display=swap">
  <link rel="stylesheet" href="/assets/editor.css"/>
</head>

<body>
<svg xmlns="http://www.w3.org/2000/svg">
  <symbol id="logo" viewBox="-300 -300 600 600" fill="currentColor">
    <g transform="scale(1 -1)">
    <polygon points="-280,-90 0,190 280,-90" transform="translate(0 -210) rotate(0)"></polygon>
      <polygon points="-280,-90 0,190 280,-90" transform="translate(-210 0) rotate(-90)"></polygon>
      <polygon points="-198,-66 0,132 198,-66" transform="translate(207 207) rotate(-45)"></polygon>
      <polygon points="-130,0 0,-130 130,0 0,130" transform="translate(150 0) rotate(0)"></polygon>
      <polygon points="-191,61 69,61 191,-61 -69,-61" transform="translate(-89 239) rotate(0)"></polygon>
      <polygon points="-130,-44 0,86  130,-44" transform="translate(0 106) rotate(-180)"></polygon>
      <polygon points="-130,-44 0,86  130,-44" transform="translate(256 -150) rotate(-270)"></polygon>
    </g>
  </symbol>
</svg>

<main id="main"></main>
<textarea id="original" style="display:none;">$(cat $4)</textarea>

<script src="/assets/editor.js"></script>
<script src="/assets/code-editor.js"></script>
<script src="/assets/editor-navigation.js"></script>
<script>
  window.addEventListener('load', function() {
    var originalCode = document.getElementById('original').textContent;
    main = Elm.Main.init({
      node: document.getElementById('main'),
      flags: { original: originalCode, name: "$3" }
    });

    main.ports.submitSource.subscribe(function(source) {
      var editorNode = document.getElementById('editor');
      var codeNode = document.getElementById('code');
      codeNode.value = source;
      editorNode.submit();
    });

    window.addEventListener("message", gotErrors, false);

    function gotErrors(event) {
      // TODO if (event.origin !== "https://worker.elm-lang.org") return;
      if (event.data == "SUCCESS") {
        main.ports.gotSuccess.send(null);
      } else {
        var message = JSON.parse(event.data);
        main.ports.gotErrors.send(message);
      }
    }

  });
</script>

</body>

</html>

EOF

}



## DOWNLOAD BINARIES

PATH=$(pwd)/node_modules/.bin:$PATH

if ! [ -x "$(command -v elm)" ]; then
  npm install elm@latest-0.19.1
fi
if ! [ -x "$(command -v uglifyjs)" ]; then
  npm install uglify-js
fi


## GENERATE HTML


mkdir -p _site
mkdir -p _temp

## static

cp -r static/* _site/

## editor

# if ! [ -f _site/assets/editor.js ]; then
  echo "EDITOR"
  cat editor/cm/lib/codemirror.js editor/cm/lib/active-line.js editor/cm/mode/elm.js | uglifyjs -o _site/assets/editor.js
  cat editor/code-editor.js editor/column-divider.js > _site/assets/code-editor.js
  cat editor/cm/lib/codemirror.css editor/editor.css > _site/assets/editor.css
  (cd editor ; elm make src/Main.elm --output=elm.js)
  cat editor/elm.js > _site/assets/editor-navigation.js
  # uglifyjs editor/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle -o _site/assets/editor-navigation.js
  rm editor/elm.js
# fi


## examples

echo "EXAMPLES: share"

# if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]; then
#     echo "Cached: $elm"
# else
    rm -f elm-stuff/*/Main.elm*
    elm make examples/share.elm --output=_site/examples/_compiled/share.html > /dev/null
    cat examples/share.elm | makeExampleHtml "_site/examples/share.html" "share" "share"
# fi

## try

echo "" | makeExampleHtml _site/try.html "Try Elm!" _try
cp editor/splash.html _site/examples/_compiled/_try.html


## REMOVE TEMP FILES

rm -rf _temp
