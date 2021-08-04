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
<!-- ICONS -->
<svg xmlns="http://www.w3.org/2000/svg" style="display: none;">
  <symbol id="moon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path>
  </symbol>
  <symbol id="sun" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <circle cx="12" cy="12" r="5"/><line x1="12" y1="1" x2="12" y2="3"/>
    <line x1="12" y1="21" x2="12" y2="23"/><line x1="4.22" y1="4.22" x2="5.64" y2="5.64"/>
    <line x1="18.36" y1="18.36" x2="19.78" y2="19.78"/>
    <line x1="1" y1="12" x2="3" y2="12"/>
    <line x1="21" y1="12" x2="23" y2="12"/>
    <line x1="4.22" y1="19.78" x2="5.64" y2="18.36"/>
    <line x1="18.36" y1="5.64" x2="19.78" y2="4.22"/>
  </symbol>
  <symbol id="refresh" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <polyline points="1 4 1 10 7 10"></polyline>
    <polyline points="23 20 23 14 17 14"></polyline>
    <path d="M20.49 9A9 9 0 0 0 5.64 5.64L1 10m22 4l-4.64 4.36A9 9 0 0 1 3.51 15"></path>
  </symbol>
  </symbol id="x" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <line x1="18" y1="6" x2="6" y2="18"></line>
    <line x1="6" y1="6" x2="18" y2="18"></line>
  </symbol>
  <svg id="checkmark" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-check">
    <polyline points="20 6 9 17 4 12"/>
  </svg>
  <symbol id="up" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <polyline points="18 15 12 9 6 15"/>
  </symbol>
  <symbol id="down" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <polyline points="6 9 12 15 18 9"/>
  </symbol>
  <symbol id="package" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <line x1="16.5" y1="9.4" x2="7.5" y2="4.21"/><path d="M21 16V8a2 2 0 0 0-1-1.73l-7-4a2 2 0 0 0-2 0l-7 4A2 2 0 0 0 3 8v8a2 2 0 0 0 1 1.73l7 4a2 2 0 0 0 2 0l7-4A2 2 0 0 0 21 16z"/>
    <polyline points="3.27 6.96 12 12.01 20.73 6.96"/>
    <line x1="12" y1="22.08" x2="12" y2="12"/>
  </symbol>
  <symbol id="send" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <line x1="22" y1="2" x2="11" y2="13"/>
    <polygon points="22 2 15 22 11 13 2 9 22 2"/>
  </symbol>
  <symbol id="chain" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"">
    <path d="M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"/>
    <path d="M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"/>
  </symbol>
  <symbol id="code-tags" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <polyline points="16 18 22 12 16 6"/>
    <polyline points="8 6 2 12 8 18"/>
  </symbol>
</svg>

<!-- NAVIGATION -->
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
      var message = JSON.parse(event.data);
      main.ports.gotErrors.send(message);
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
  cat editor/code-editor.js editor/split-page.js > _site/assets/code-editor.js
  cat editor/cm/lib/codemirror.css editor/editor.css > _site/assets/editor.css
  (cd editor ; elm make src/Main.elm --output=elm.js)
  cat editor/elm.js > _site/assets/editor-navigation.js
  # uglifyjs editor/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle -o _site/assets/editor-navigation.js
  rm editor/elm.js
# fi


## examples

echo "EXAMPLES: buttons"

# if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]; then
#     echo "Cached: $elm"
# else
    rm -f elm-stuff/*/Main.elm*
    elm make examples/buttons.elm --output=_site/examples/_compiled/buttons.html > /dev/null
    cat examples/buttons.elm | makeExampleHtml "_site/examples/buttons.html" "buttons" "buttons"
# fi

## try

echo "" | makeExampleHtml _site/try.html "Try Elm!" _try
cp editor/splash.html _site/examples/_compiled/_try.html


## REMOVE TEMP FILES

rm -rf _temp
