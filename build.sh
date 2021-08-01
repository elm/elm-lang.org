#!/bin/bash

set -e



## MAKE PAGE HTML


function makePageHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html lang="en">

<head>
  <title>$2</title>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="A delightful language with friendly error messages, great performance, small assets, and no runtime exceptions.">
  <meta name=”robots” content="index, follow">
  <link rel="shortcut icon" sizes="16x16 32x32 48x48 64x64 128x128 256x256" href="/favicon.ico">
  <link rel="stylesheet" rel="preload" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Courier+Prime&display=swap">
  <link rel="stylesheet" href="/assets/style.css">
  <link rel="stylesheet" href="/assets/highlight/styles/default.css">
  <script src="/assets/highlight/highlight.pack.js"></script>
</head>

<body>

<script type="text/javascript">
$(cat $3)
var app = Elm.Main.init({ flags: { width: window.innerWidth, height : window.innerHeight } });
</script>

</body>
</html>
EOF

}



## MAKE EXAMPLE HTML


# ARGS:
#   $1 = _site/examples/NAME.html
#   $2 = <title>
#   $3 = NAME
#   $4 = code
#
function makeExampleHtml {
  cat <<EOF > $1
<html>

<head>
  <meta charset="UTF-8">
  <title>Elm | $2 example</title>
  <link rel="stylesheet" rel="preload" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Courier+Prime&display=swap">
  <link rel="stylesheet" href="/assets/editor.css"/>
</head>

<body>
<!-- ICONS -->
<svg xmlns="http://www.w3.org/2000/svg" style="display: none;">
  <symbol id="moon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <path d="M21 12.79A9 9 0 1 1 11.21 3 7 7 0 0 0 21 12.79z"></path>
  </symbol>
  <symbol id="refresh" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
    <polyline points="1 4 1 10 7 10"></polyline>
    <polyline points="23 20 23 14 17 14"></polyline>
    <path d="M20.49 9A9 9 0 0 0 5.64 5.64L1 10m22 4l-4.64 4.36A9 9 0 0 1 3.51 15"></path>
  </symbol>
</svg>

<!-- NAVIGATION -->
<nav id="navigation">
  <div class="hint">More Examples <a href="/examples" target="_blank">Here</a></div>
  <button alt="Compile your code (Ctrl-Enter)" onclick="compile()">
    <svg class="icon"><use xlink:href="#refresh"></svg>
    Compile
  </button>
  <button alt="Switch the color scheme" onclick="lights()">
    <svg class="icon"><use xlink:href="#moon"></svg>
    Lights
  </button>
</nav>

<!-- EDITOR -->
<form id="editor" action="https://worker.elm-lang.org/compile" method="post" enctype="multipart/form-data" target="output">
  <textarea id="code" name="code" style="display:none;">$(cat $4)</textarea>
</form>

<!-- RESULT -->
<div id="divider"></div>
<iframe id="output" name="output" src="/examples/_compiled/$3.html"></iframe>

<script src="/assets/editor.js"></script>
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

## pages

echo "PAGES"
for elm in $(find pages -type f -name "*.elm")
do
    subpath="${elm#pages/}"
    name="${subpath%.elm}"
    js="_temp/$name.js"
    html="_site/$name.html"

    if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]; then
        echo "Cached: $elm"
    else
        echo "Compiling: $elm"
        mkdir -p $(dirname $js)
        mkdir -p $(dirname $html)
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --optimize --output=$js > /dev/null
        uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
          | uglifyjs --mangle \
          | makePageHtml $html $name
        # elm make $elm --output=$js > /dev/null
        # cat $js | makePageHtml $html $name
    fi
done

## editor

if ! [ -f _site/assets/editor.js ]; then
  echo "EDITOR"
  cat editor/cm/lib/codemirror.js editor/cm/lib/active-line.js editor/cm/mode/elm.js editor/editor.js | uglifyjs -o _site/assets/editor.js
  cat editor/cm/lib/codemirror.css editor/editor.css > _site/assets/editor.css
  (cd editor ; elm make src/Main.elm --optimize --output=elm.js)
  uglifyjs editor/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle -o _site/assets/editor-hints.js
  rm editor/elm.js
fi

## examples

echo "EXAMPLES"
for elm in $(find examples -type f -name "*.elm")
do
    subpath="${elm#examples/}"
    name="${subpath%.elm}"
    html="_site/examples/$name.html"

    if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]; then
        echo "Cached: $elm"
    else
        echo "Compiling: $elm"
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --output=_site/examples/_compiled/$name.html > /dev/null
        cat $elm | makeExampleHtml $html $name $name
    fi
done

## try

echo "" | makeExampleHtml _site/try.html "Try Elm!" _try
cp editor/splash.html _site/examples/_compiled/_try.html


## REMOVE TEMP FILES

rm -rf _temp
