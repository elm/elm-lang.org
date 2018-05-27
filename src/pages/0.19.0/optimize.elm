import Skeleton


main =
  Skeleton.hint "How to optimize Elm code" """

When you are serving a website, there are two kinds of optimizations you want to do:

1. **Asset Size** &mdash; How can we send as few bits as possible?
2. **Performance** &mdash; How can those bits run as quickly as possible?

It turns out that Elm does really well on both! We have [very small assets](TODO) and [very fast code](/blog/blazing-fast-html-round-two) when compared to the popular alternatives.

Okay, but how do we get those numbers?


## Instructions

Step one is to compile with the `--optimize` flag. This does things like shortening record field names and unboxing values.

Step two is to call `uglifyjs` with a bunch of special flags. The flags unlock a bunch of optimizations that are unreliable in normal JS code, but work fine for us because Elm does not have side-effects! You can install `uglifyjs` with `npm install uglifyjs -g` if you need it.

So here is how I would optimize `src/Main.elm` with two terminal commands:

```bash
elm make src/Main.elm --optimize --output=elm.js
uglifyjs elm.js --compress 'TODO' | uglifyjs --mangle --output=elm.min.js
```

After this you will have an `elm.js` and a significantly smaller `elm.min.js` file!

**Note:** `uglifyjs` is called twice there. First to `--compress` and second to `--mangle`. This is necessary! Otherwise `uglifyjs` will ignore our `pure_funcs` flag.


## Scripts

It is hard to remember all that, so it is probably a good idea to write a script that does it.

I would maybe want to run `./optimize.sh src/Main.elm` and get out `elm.js` and `elm.min.js`, so on Mac or Linux, I would make a script called `optimize.sh` like this:

```bash
#!/bin/sh

set -e

js="elm.js"
min="elm.min.js"

elm make --optimize --output=$js $@

uglifyjs $js --compress 'TODO' | uglifyjs --mangle --output=$min

echo "Initial size: $(cat $js | wc -c) bytes  ($js)"
echo "Minified size:$(cat $min | wc -c) bytes  ($min)"
echo "Gzipped size: $(cat $min | gzip -c | wc -c) bytes"
```

It also prints out all the asset sizes for you! Your server should be configured to gzip the assets it sends, so the last line is telling you how many bytes would _actually_ get sent to the user.

Again, the important commands are `elm` and `uglifyjs` which work on any platform, so it should not be too tough to do something similar on Windows.


"""