
import Website.Skeleton
import Website.ColorScheme

title = constant (JavaScript.castStringToJSString "Upgrade Time: Elm 0.6")
foreign export jsevent "elm_title"
  title :: Signal JSString


sideBySide wid e1 e2 =
  let w = wid `div` 2
      h = max (heightOf e1) (heightOf e2)
      arrow = text . Text.height 3 . Text.color accent1 . toText $ "&rarr;"
  in  layers [ container wid h middle arrow
             , flow right [ container w h middle e1
                          , container w h middle e2
                          ]
             ]

intro = [markdown|

# Time and Syntax (v0.6)

The most obvious changes in [Elm](/) 0.6 are whitespace sensitivity and the addition
of many useful time signals such as `(fps :: Number -> Signal Time)` which make
it much easier to make rich animations that work on many devices.

This release also includes a [`Date` library][date] and supports
[HSV colors][hsv] in the [`Color` library][color].

  [date]: / "Date library"
  [hsv]: http://en.wikipedia.org/wiki/HSL_and_HSV "HSV Colors"
  [color]: /docs/Graphics/Color.elm "Color library"

### Syntax

Elm&rsquo;s use of curly braces and semi-colons added quite a bit of visual
noise to let- and case-expressions. Elm now supports whitespace sensitivity,
so you can leave out the `{;;;}` as long as you indent each term properly.
|]

guards = [markdown|
The new guarded definitions are a great way to avoid ugly nested if-expressions.
Say you want to use the up (40) and down (38) arrow keys to control a value.
|]

infixes = [markdown|
You can also define custom infix operators. For now, all user defined infix operators
are ___-associative and have the highest precedence, meaning that they bind tighter than
any predefined operators. Furthermore, you cannot override predefined infix operators.
|]

time = [markdown|

### Time

The time library has been completely rewritten with a much stronger focus on creating
highly interactive animations. Highlights from the new API include:

    fps     :: Number -> Signal Time
    fpsWhen :: Number -> Signal Bool -> Signal Time

    delay :: Time -> Signal a -> Signal a
    since :: Time -> Signal a -> Signal Bool

    timestamp :: Signal a -> Signal (Time,a)
    timeOf    :: Signal a -> Signal Time

The knowledge gained by writing real Elm programs such as [Pong][pong] was
the basis of these additions, and the new functions actually make
[the code for Pong][code] significantly more clear and concise.

  [pong]: / "Pong in Elm"
  [code]: / "Source for Pong"

You can see the entire library [here].

  [here]: / "Time Library"

### HSV Colors

|]

rest = [markdown|

### Date


### Internal Improvements

- No more namespace pollution in JavaScript when importing Elm libraries.
  Previously, some Elm functions would leak out into the global JavaScript
  namespace.
- Type checking should be marginally faster.
|]

scene t w = 
  let cs = colorCycle t in
  flow down [ width w intro
            , sideBySide w beforeLet afterLet
            , sideBySide w beforeCase afterCase
            , width w guards
            , sideBySide w beforeIf afterIf
            , width w infixes
            , sideBySide w beforeInfix afterInfix
            , width w time
            , container w (heightOf cs) middle cs
            , width w rest
            ]

beforeIf = [markdown|
    toDirection k y =
        if k == 40 then y+1 else
        if k == 38 then y-1 else y
|]

afterIf = [markdown|
    toDirection k y
        | k == 40   = y + 1
        | k == 38   = y - 1
        | otherwise = y
|]

beforeLet = [markdown|
    let { xs = ...
        ; ys = ...
        }
    in  zipWith max xs ys
|]

afterLet = [markdown|
    let xs = ...
        ys = ...
    in  zipWith max xs ys
|]
beforeCase = [markdown|
    case httpResponse of
      { Success s   -> ...
      ; Waiting     -> ...
      ; Failure _ _ -> ...
      }
|]
afterCase = [markdown|
    case httpResponse of
        Success s   -> ...
        Waiting     -> ...
        Failure _ _ -> ...
|]
beforeInfix = [markdown|
    vplus (x,y) (x',y') =
        (x + x', y + y')

    v = a `vplus` b `vplus` c
|]
afterInfix = [markdown|
    (x,y) -+- (x',y') =
        (x + x', y + y')

    v = a -+- b -+- c
|]

(box,checked) = Input.checkbox True

colorCycle t =
  let toPos t   = (150 + 100 * cos (pi*t/180), 150 + 100 * sin (pi*t/180))
      toDot r t = filled (hsv t 1 1) $ circle r (toPos t)
      t1 = (t / 100) `mod` 360
      t2 = (180 + t / 100) `mod` 360
  in  flow down [ collage 300 300 $
                          toDot 20 t1 : 
                          toDot 20 t2 : map (toDot 10 . (*) 30) [0..11]
                , container 300 40 middle $ plainText "On / Off  " `beside` box ]

times = foldp (+) 0 (30 `fpsWhen` checked)

main = lift2 skeleton (lift scene times) Window.width

