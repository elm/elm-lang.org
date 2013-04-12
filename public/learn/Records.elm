
import Website.Skeleton
import Website.ColorScheme
import Window as Window
import JavaScript as JS
import Graphics.Text as Text

title = constant (JS.fromString "Elm 0.7 - Extensible Records")
foreign export jsevent "elm_title"
  title : Signal JSString

intro w = width w [markdown|

<style type="text/css">
p { text-align: justify }
h3 { padding-top: 1em; }
pre {
 background-color: rgb(245,245,245);
 margin: 0 30px;
 padding: 4px 10px;
 border-left: solid 2px rgb(96,181,204);
}
</style>

# Extensible Records

Records are a labeled data structure. They provide a lightweight representation
for complex data. You can think of records in Elm as similar to objects in
JavaScript or Java or records in OCaml or Haskell, but only in a fairly general
sense. Each comparison has its limitations and qualifications, so clinging to
them will eventually hinder your understanding.

**Related resources:** If you just want to get a reminder of the syntax for
records in Elm, [see here][syntax]. There is also some [analysis
of the expressiveness of Elm&rsquo;s records][v7].
I also highly recommend reading [the formal description][records]
of the semantics and types of extensible records. The paper is very well-written
and quite accessible even if you are new to reading academic literature.

 [syntax]: /learn/Syntax.elm#records "Record Syntax"
 [v7]: /blog/announce/version-0.7.elm "Record Announcement"
 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"


The structure of the following document is as follows:

- [Comparison with Objects](#comparison-of-records-and-objects),
  important if you have an OO background
- [What is a Record?](#what-is-a-record)
- [Record Access](#access)
- [Pattern Matching for Records](#pattern-matching)
- [Updating Fields](#updating-records)
- [Adding, Deleting, and Renaming Fields](#adding-deleting-and-renaming-fields)
- [Polymorphic Fields](#polymorphic-fields)
- [Records in Types](#records-in-types)

Then we will use [a larger example](#records-for-games) to compare Elm&rsquo;s
record system to using Objects in JavaScript or records in GHC Haskell.

### Comparison of Records and Objects

Records in Elm are quite similar to objects in JavaScript. The major differences
are that with records:

- You cannot ask for a field that does not exist.
- No field will ever be undefined or null.
- You cannot create recursive records with a `this` or `self` keyword.

I highly encourage a strict separation of data and logic, and as far as
I can tell, the ability to say `this` is primarily used to break this
separation. This is a systematic problem in Object Oriented languages
that I would like to avoid.

Records also support &ldquo;[structural typing][st]&rdquo; which allows subtyping
based on structure. Languages like Java have more restrictive subtyping that only
recognizes relationships that are explicitly declared. Records in Elm can be
used in any situation as long as the necessary fields exist and have the correct
type.

 [st]: http://en.wikipedia.org/wiki/Structural_type_system "Structural Types"

### What is a Record?

A record is a lightweight labeled data structure. For instance, if we
wanted to represent a point we just create a record with an x and y field:

    { x = 3, y = 4 }

Just like tuples, a record can hold values with different types, so
we can represent a book like this:

    { title  = "Steppenwolf"
    , author = "Hesse"
    , pages  = 237 }

As we will soon see, it is also possible to access, add, remove, rename,
and update fields. We will use the following records to define the rest
of the record operations:

    point2D = { x = 0, y = 0 }

    point3D = { x = 3, y = 4, z = 12 }

    bill  = { name = "Gates", age = 57 }
    steve = { name = "Jobs" , age = 56 }
    larry = { name = "Page" , age = 39 }

    people = [ bill, steve, larry ]

### Access

There are a number of ways to access records:

|]

access w = evaluate w
  [ ("point3.z", "12")
  , ("bill.name", "\"Gates\"")
  , (".name bill", "\"Gates\"")
  , ("map .age people", "[57,56,39]") ]

postAccess = [markdown|
The first way to access records is fairly standard, appearing in many
languages, from JavaScript to OCaml. No spaces are permitted on either
side of the `(.)` with this method.

The second way to access records is with a special function `.name` that
is equivalent to `(\\r -> r.name)`, making things a bit more concise.

The only requirement is that the accessor is used on a record that actually
has that field, the other fields in the record do not matter. So it is
perfectly acceptable to say:

    .x point2D == 0
    .x point3D == 3
    .x {x = 4} == 4

in a single file because each of these records has a field `x`.
Note that none these accessors pollute the global namespace.
You can still have a value `x` independent of the `(.x)` accessor.

### Pattern Matching

It is also possible to pattern match on records:

    dist {x,y} = sqrt (x^2 + y^2)

    under50 {age} = age < 50

The first function takes any record that has both an `x` and `y` field and
computes the distance to the origin. The second takes any record that has an
`age` and determines if it is less than 50. We can use these functions as follows:

|]

matches w = evaluate w
  [ ("dist point2D", "0")
  , ("dist point3D", "5")
  , ("under50 bill", "False")
  , ("any (map under50 people)", "True") ]

postMatches = [markdown|
These patterns can appear in let expressions `(let {x,y} = ... in ...)`
and lambda expressions `(\\{x,y} -> ...)` as well.

### Updating Records

It is often useful to &ldquo;update&rdquo; the values in a record.

|]

updating w = evaluate w
  [ ("{ point2D | y <- 1 }", "{ x = 0, y = 1 }")
  , ("{ point3D | x <- 0, y <- 0 }", "{ x = 0, y = 0, z = 12 }")
  , ("{ steve | name <- \"Wozniak\" }", "{ name = \"Wozniak\", age = 56 }")
  ]

postUpdating w = width w [markdown|
You can update as many fields as you want, separating each update by a comma.
You can even change the type of value in a field. Say the user inputs a bunch
of personal data producing a record. It would be nice to convert some of the
strings into numbers if possible. This is no problem:

    rawInput = { name    = "Tom"
               , country = "Finland"
               , age     = "34"
               , height  = "1.9" }

    prettify person =
        { person | age    <- readInt   person.age
                 , height <- readFloat person.height }

    input = prettify rawInput

We started with a record in which `(person.age : String)`, providing little
information about the validity of the input. The result is that
`(person.age : Maybe Int)`, fully capturing the type of input we are dealing
with and whether or not it is valid.

The update functions allow you to write fairly elaborate update functions
with little trouble.

### Adding, Deleting, and Renaming Fields

Record fields can be added and deleted with following syntax:
|]

rest w = evaluate w
  [ ("{ point3D - z }", "{ x = 3, y = 4 }")
  , ("{ bill - age }", "{ name = \"Gates\" }")
  , ("{ point2D | z = 0 }", "{ x = 0, y = 0, z = 0 }")
  , ("{ bill | height = 1.77 }", "{ name   = \"Gates\"\n, age    = 57\n, height = 1.77 }")
 ]

postRest w = width w [markdown|
This actually means you can have multiple fields with the same name in a record,
the latest field taking precedence over the earlier ones. Check out
[this paper][records] for more information on this.

 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"
We can combine the add and delete operations to rename fields.

    renameName person = { person - name | surname = person.name }
<br/>
|]

rename w = evaluate w
  [ ("renameName bill", "{ surname = \"Gates\"\n, age     = 57 }") ]

postRename w = width w [markdown|

We can also derive record updates with field addition and removal:
|]

replace w = evaluate w
  [ ("{ point2D - x | x = 1 }", "{ x = 1, y = 0 }") ]

postReplace w = width w [markdown|
The field update syntax is just a prettier way to write this!

### Polymorphic Fields

Elm allows [polymorphism][poly] within records, so a record field can
hold a polymorphic function like list append `(++)`. For example:

 [poly]: http://en.wikipedia.org/wiki/Parametric_polymorphism "Parametric Polymorphism"

    lib = { id x = x
          , flip f x y = f y x }

    group = { op a b = a ++ b
            , zero = [] }

The `lib` record holds an `id` function which takes a value and returns exactly
the same value and the `flip` function which switches the order of arguments to
a function. Both are polymorphic because they can work with values with many
different types.

The `group` record holds an `op` function that appends lists and a `zero` value
that represents an empty list.
|]

poly w = evaluate w
  [ ("lib.id 42", "42")
  , ("lib.id 'b'", "'b'")
  , ("lib.flip (++) \"ab\" \"cd\"", "\"cdab\"")
  , ("lib.flip (::) [] 3", "[3]")
  , ("group.op \"Hello\" group.zero", "\"Hello\"")
  , ("group.op [1,2] [3,4]", "[1,2,3,4]")
  ]

postPoly w = width w [markdown|
I suspect that this can be used for some really cool stuff! It should
make it possible to gain some of the flexibility of first-class modules
and typeclasses, as described in [this announcement]

### Records in Types

You can create [algebraic data types][adt] that contain records. For example,
maybe you want to add locations to arbitrary values. Maybe some locations
are exact and some are relative to their container:

    data Located a = Absolute { x : Int  , y : Int   } a
                   | Relative { x : Float, y : Float } a

    v1 = Absolute { x = 3, y = 4 } "This can be any value"

    v2 = Relative { x = 0.5, y = 0.3 } v1

You can also have polymorphism within the record itself. Maybe you want to
represent a [binary tree][tree] with records:

    data Tree a = Empty | Node { value : a
                               , left  : Tree a
                               , right : Tree a }

You cannot use records to pattern match in case-expressions though, so this
approach may not be best if you need to match on tree structures more than
one level deep. So unfortunately this example may be more illustrative than
practical!

 [adt]: /learn/Pattern-Matching.elm "ADTs"
 [tree]: http://en.wikipedia.org/wiki/Binary_tree "Binary Tree"

<br/>

<h1 id="records-for-games"><div style="text-align:center">Records for Games
<div style="font-size:0.5em;font-weight:normal">*Comparing Elm, JavaScript, and Haskell*</div></div>
</h1>

We will now examine an example that is extremely useful for
creating complex games and interactions. We will also use this example
to make some comparisons between Elm, JavaScript, and Haskell.

Say we want to represent objects in space which have a position, angle,
and velocity. This can include anything from spaceships to asteroids to
planets, each with a few unique fields.

First we will create a spaceship:

    spaceship = { x        = 0
                , y        = 0
                , angle    = pi / 7
                , velocity = 5
                , health   = 100
                , fuel     = 100
                , image    = "/spaceship.gif"
                }

Now we can define a `step` function that can move a spaceship forward for
a given timestep `t`.

    step t obj =
        let {x,y,angle,velocity} = obj in
        { obj | x <- x + t * velocity * cos angle
              , y <- y + t * velocity * sin angle }

We now have a `step` function that can update *any* object that has
a position, velocity, and angle. So when we create an `asteroid`, it
can use the exact same step function even though it does not have exactly
the same fields as `spaceship`.

    asteroid = { x        = 100
               , y        = 200
               , angle    = pi / 3
               , velocity = 9
               , radius   = 15
               , image    = "/asteroid.gif"
               }

    spaceship' = step 1 spaceship
    asteroid'  = step 1 asteroid

This gives much of the flexibility of dynamic languages like JavaScript,
but with the added benefits of being purely functional and strongly/statically
typed.

**Elm vs JavaScript:** In JavaScript you could write roughly the same code.
The `step` function would be a bit uglier without the pattern matching on
records, but you get the same level of flexibility. The trouble is that
you have no guarantee that the function will be used correctly. Anyone
can pass in an improperly formatted object and your code would break.
You could manually check that the incoming objects are properly formatted:

    if (!obj.hasOwnProperty("x") ||
        !obj.hasOwnProperty("y") ||
        !obj.hasOwnProperty("angle") ||
        !obj.hasOwnProperty("velocity")) {
      throw "Error that I now have to catch somewhere else.";
    }

But now you have six lines of hideous code *and* you have to catch an error
somewhere else in your code. Perhaps you could leave this check out and
assume that no one will ever make a mistake ever... Maybe you could add a bunch
of tests...

None of these &ldquo;solutions&rdquo; are particularly satisfying, and each
causes extra work whether it is catching errors, testing, or debugging when someone
inevitably calls the function improperly. This is the kind of issue that makes
programs frail and difficult to maintain.

Fortunately, these trade-offs are simply not an issue in Elm.
Elm will statically determine that these fields are all present at compile time,
avoiding the runtime check and completely and ruling out any possibility of
using an undefined field.

Elm rules out an entire pandoras box of issues, allowing you to focus on the
parts of programming that actually matter.

**Elm vs GHC Haskell:** In GHC Haskell, the example above is simply not
possible without being heavily refactored. And no matter how you refactor,
you would eventually need a different `step` function for each type of space object.

GHC Haskell&rsquo;s record system uses accessors that pollute the
global namespace, meaning that you cannot have two different records with the
same field in a single file. So you would not even be able to use `spaceship`
and `asteroid` in the same file! Here is a minimal program that is rejected
by GHC 7.4:

    data Spaceship = Spaceship { x:Int }
    data Asteroid  = Asteroid  { x:Int }

You could separate out the position, angle, and velocity and write a `step`
function for just that data. So now you can share the logic of `step`, but you
will still need to create a `stepSpaceship` and `stepAsteroid` for each object.

The Hugs compiler for Haskell has a much nicer record system called Trex, but as
far as I can tell, this is not standard in Haskell and not a lot of people use Hugs.
Trex is still slightly less expressive than the Elm&lsquo;s records, and
[this paper][records] discusses the differences.

 [records]: http://research.microsoft.com/pubs/65409/scopedlabels.pdf "Extensible Records"

|]

evaluate wid pairs =
  let elem = text . monospace . toText
      both f (a,b) = (f a, f b)
      w = wid `div` 2
      line (a,b) = let h = 10 + maximum [ heightOf a, heightOf b ]
                   in  flow right [ container w h middle a
                                  , container w h middle b ]
      es = map (line . both elem) pairs
      arrow = text . Text.height 3 . Text.color accent1 . toText $ "&rArr;"
      h = sum $ map heightOf es
  in  layers [ container wid h middle arrow
             , flow down es
             ]

content w = 
  flow down
    [ intro w
--    , access w
    , width w postAccess
--    , matches w
    , width w postMatches
--    , updating w
    , postUpdating w
--    , rest w
    , postRest w
--    , rename w
    , postRename w
--    , replace w
    , postReplace w
--    , poly w
    , postPoly w
    ]

main = lift (skeleton (content . min 600)) Window.width


