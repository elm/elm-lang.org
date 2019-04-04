#!/bin/bash

set -e



## MAKE PAGE HTML


function makePageHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>$2</title>
  <link rel="shortcut icon" sizes="16x16 32x32 48x48 64x64 128x128 256x256" href="/favicon.ico">
  <link rel="stylesheet" href="https://fonts.googleapis.com/css?family=IBM+Plex+Sans|Source+Code+Pro">
  <link rel="stylesheet" href="/assets/style.css">
  <link rel="stylesheet" href="/assets/highlight/styles/default.css">
  <script src="/assets/highlight/highlight.pack.js"></script>
</head>

<body>

<script type="text/javascript">
$(cat $3)
var app = Elm.Main.init();
</script>

</body>
</html>
EOF

}



## DOWNLOAD BINARIES


if ! [ -x "$(command -v elm)" ]
then
  npm install elm
  PATH=$(pwd)/node_modules/elm/bin:$PATH
fi
if ! [ -x "$(command -v uglifyjs)" ]
then
  npm install uglify-js
  PATH=$(pwd)/node_modules/uglify-js/bin:$PATH
fi



## GENERATE HTML


mkdir -p _site
mkdir -p _temp

## static

cp -r static/* _site/

## pages

for elm in $(find pages -type f -name "*.elm")
do
    subpath="${elm#pages/}"
    name="${subpath%.elm}"

    js="_temp/$name.js"
    html="_site/$name.html"

    mkdir -p $(dirname $js)
    mkdir -p $(dirname $html)

    if [ -f $html ] && [ $(date -r $elm +%s) -eq $(date -r $html +%s) ]
    then
        echo "Cached: $elm"
    else
        echo "Compiling: $elm"
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --optimize --output=$js > /dev/null
        uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
         | uglifyjs --mangle \
         | makePageHtml $html $name
        # elm make $elm --output=$js > /dev/null
        # cat $js | makePageHtml $html $name
        touch -r $elm $html
    fi
done



## REMOVE TEMP FILES

rm -rf _temp
