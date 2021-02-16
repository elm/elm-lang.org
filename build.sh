#!/bin/bash

set -e



## MAKE PAGE HTML


function makePageHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html>

<head>
  <title>$2</title>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="A delightful language with friendly error messages, great performance, small assets, and no runtime exceptions.">
  <meta name=”robots” content="index, follow">
  <link rel="shortcut icon" sizes="16x16 32x32 48x48 64x64 128x128 256x256" href="/favicon.ico">
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Source+Code+Pro">
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
  <title>$2</title>
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=Source+Code+Pro"/>
  <link rel="stylesheet" href="/assets/editor.css"/>
</head>

<body>
<form id="editor" action="https://worker.elm-lang.org/compile" method="post" enctype="multipart/form-data" target="output">
  <div id="options">
    <div class="hint">More Examples <a href="/examples" target="_blank">Here</a></div>
    <div class="button blue" title="Compile your code (Ctrl-Enter)" onclick="compile()">Compile<span></span></div>
    <div class="button gray" title="Switch the color scheme" onclick="lights()">Lights<span></span></div>
  </div>
  <textarea id="code" name="code" style="display:none;">$(cat $4)</textarea>
</form>
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
  cat editor/cm/lib/codemirror.js editor/cm/mode/elm.js editor/editor.js | uglifyjs -o _site/assets/editor.js
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
