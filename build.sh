#!/bin/bash

set -e

function makeHtml {
  cat <<EOF > $2
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>$3</title>
  <link rel="shortcut icon" sizes="16x16 32x32 48x48 64x64 128x128 256x256" href="/favicon.ico">
  <link rel="stylesheet" href="/assets/style.css?v=4">
  <link rel="stylesheet" href="/assets/highlight/styles/default.css">
  <script src="/assets/highlight/highlight.pack.js"></script>
</head>

<body>

<script type="text/javascript">
$(cat $1)
var app = Elm.Main.init();
</script>

</body>
</html>
EOF

}


## DOWNLOAD ELM BINARY

if [ ! -f bin/elm ]; then
  curl $ELM_URL | tar xz
  chmod 755 elm
  mkdir bin
  mv elm bin/
fi
PATH=$(pwd)/bin:$PATH


## GENERATE HTML

cp -r static _site

mkdir -p _temp

for elm in $(find src/pages -type f -name "*.elm")
do
    subpath="${elm#src/pages/}"
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
        # TODO minify the JavaScript
        makeHtml $js $html $name
        touch -r $elm $html
    fi
done

rm -rf _temp