#!/bin/bash

set -e -o pipefail
shopt -s nullglob



## CHECK ARGUMENTS


if [ $# -ne 1 ]; then
    printf "expecting one argument to ./build.sh like this:\n\n    ./build.sh prod\n    ./build.sh dev\n\n"
    exit 1
fi


case $1 in
    prod)
        echo "Running a PROD build.";
        is_prod () { return 0; } ;;
    dev)
        echo "Running a DEV build.";
        is_prod () { return 1; } ;;
    *)
        printf "expecting one argument to ./build.sh like this:\n\n    ./build.sh prod\n    ./build.sh dev\n\n";
        exit 1 ;;
esac



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
  <meta name="robots" content="index, follow">
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
#   $4 = dependencies.json
#   $5 = code
#
function makeExampleHtml {
    cat <<EOF > $1
<!DOCTYPE HTML>
<html lang="en">

<head>
  <meta charset="UTF-8">
  <title>$2</title>
  <link rel="shortcut icon" sizes="16x16 32x32 48x48 64x64 128x128 256x256" href="/favicon.ico">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="description" content="Try out Elm: A delightful language with friendly error messages, great performance, small assets, and no runtime exceptions.">
  <link rel="stylesheet" rel="preload" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Courier+Prime&display=swap">
  <link rel="stylesheet" href="/assets/editor-styles.css"/>
</head>

<body>
<svg xmlns="http://www.w3.org/2000/svg" style="display:none;">
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
<textarea id="original" style="display:none;">$(cat $5)</textarea>
<script src="/assets/editor-codemirror.js"></script>
<script src="/assets/editor-custom-elements.js"></script>
<script src="/assets/editor-elm.js"></script>
<script>
  window.addEventListener('load', function() {
    var originalCode = document.getElementById('original').textContent;
    main = Elm.Page.Editor.init({
      node: document.getElementById('main'),
      flags: {
        name: "$3",
        width: window.innerWidth,
        height: window.innerHeight,
        original: document.getElementById('original').textContent,
        dependencies: $(cat $4)
      }
    });

    main.ports.submitSource.subscribe(function(source) {
      var editorNode = document.getElementById('editor');
      var codeNode = document.getElementById('code');
      codeNode.value = source;
      editorNode.submit();
    });

    window.addEventListener("message", gotErrors, false);

    function gotErrors(event) {
      if (event.origin !== "https://elm.studio" && event.origin !== "https://social.elm.studio") return;
      if (event.data == "SUCCESS") {
        main.ports.gotSuccess.send(null);
      } else {
        var message = JSON.parse(event.data);
        main.ports.gotErrors.send(message);
      }
    }

    window.addEventListener("error", function (e) {
      main.ports.gotJsError.send(e.error.message);
      return false;
    });

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

        if is_prod
        then
            elm make $elm --optimize --output=$js > /dev/null
            uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
              | uglifyjs --mangle \
              | makePageHtml $html $name
        else
            elm make $elm --output=$js > /dev/null
            cat $js | makePageHtml $html $name
        fi
    fi
done

## editor

if ! [ -f _site/assets/editor-codemirror.js ] || ! [ -f _site/assets/editor-elm.js ] || ! [ -f _site/assets/editor-custom-elements.js ]; then
  echo "EDITOR"
  # code mirror
  cat editor/cm/lib/codemirror.js \
      editor/cm/mode/elm.js \
      editor/cm/addon/edit/closebrackets.js \
      editor/cm/addon/edit/matchbrackets.js \
      editor/cm/addon/comment/comment.js \
      editor/cm/addon/search/searchcursor.js \
      editor/cm/addon/search/search.js \
      editor/cm/addon/dialog/dialog.js \
      editor/cm/lib/active-line.js \
      editor/cm/addon/dialog/dialog.js \
      editor/cm/keymap/sublime.js \
      | uglifyjs -o _site/assets/editor-codemirror.js

  # custom elements
  cat editor/code-editor.js editor/column-divider.js | uglifyjs -o _site/assets/editor-custom-elements.js

  # styles
  cat editor/cm/lib/codemirror.css editor/editor.css > _site/assets/editor-styles.css

  # elm
  (cd editor ; elm make src/Page/Editor.elm --optimize --output=elm.js)
  uglifyjs editor/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle -o _site/assets/editor-elm.js
  rm editor/elm.js
fi

## examples

echo "EXAMPLES"
for elm in $(find examples -type f -name "*.elm")
do
    deps="${elm%.elm}.json"
    subpath="${elm#examples/}"
    name="${subpath%.elm}"
    html="_site/examples/$name.html"

    if [ -f $html ] && [ $(date -r $elm +%s) -le $(date -r $html +%s) ]; then
        echo "Cached: $elm"
    else
        echo "Compiling: $elm"
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --output=_site/examples/_compiled/$name.html > /dev/null
        cat $elm | makeExampleHtml $html $name $name $deps
    fi
done

## try

echo "" | makeExampleHtml _site/try.html "Try Elm!" _try "examples/try.json"
cp editor/splash.html _site/examples/_compiled/_try.html


## REMOVE TEMP FILES

rm -rf _temp
