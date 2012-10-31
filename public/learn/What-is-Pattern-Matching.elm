import Website.Skeleton
import Website.ColorScheme


---- Text of the page: all written in Markdown ----

what = [markdown|

## What is &ldquo;Pattern Matching&rdquo;?

The term "pattern matching" is used in a different way by the functional
programming community. In the non-functional world, it often refers to
finding patterns within strings (we call that &ldquo;using regular expressions&rdquo;
or &ldquo;parsing&rdquo;). For us it is a way to break apart an algebraic data type (ADT).
Here is an example:

        data Color = Blue | Red

So we have defined the type "Color" and it has two possible values "Blue" and "Red".
So when we pattern match like this:

        toString clr =
            case clr of
              { Blue -> "Blue"
              ; Red  -> "Red" }

The case-expression is saying, &ldquo;look at the structure of `clr`. If it is `Blue`, do this.
If it is `Red`, do that.&rdquo; So `(toString Blue)` evaluates to `"Blue"` and `(toString Red)`
evaluates to `"Red"`.

Here is a more complicated example:

        data Partial a = Result a | Error String

So this creates a type called `Partial` that takes a type as an argument. We can create
`(Partial Int)` or `(Partial String)` or whatever.
The values that have the type `(Partial Int)` would look like this: 

- `Result 4`
- `Error "Integer overflow"`
- `Result 42`

All of these have the same type, so they can be used in all the same places.
So when we pattern match we can define what we want to do in each case.

        displayDoubled partial =
            case partial of
              { Result n -> asText (2 * n)
              ; Error err -> plainText ("An error occurred: " ++ err) }

Then we can run expressions like this:

- `(displayDoubled (Result 5))` which displays the number `10` on the screen.
- `(displayDoubled (Result 2))` which displays `4`.
- `(displayDoubled (Error "division by zero"))` which displays &ldquo;An error occurred: division by zero&rdquo;

The `Partial` type is meant to model [partial functions][partial].
An example of a partial function is `sqrt` which is undefined for all negative values.
Normally `sqrt` would just throw an error if it gets a bad input and the whole program
would stop. We could write a `safeSqrt` that returns `(Error "sqrt undefined on negative numbers")`
when called on negative numbers.

  [partial]: http://en.wikipedia.org/wiki/Partial_function "Partial Functions"


|]


---- Putting it all together into a single page ----


main = lift (skeleton (\w -> width w what)) Window.width


---- Setting the title of the page to be prettier ----

titles = lift JavaScript.castStringToJSString (constant "What is Pattern Matching?")
foreign export jsevent "elm_title"
  titles :: Signal JSString