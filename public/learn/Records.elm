
import JavaScript as JS
import Website.ColorScheme (accent1)
import Website.Skeleton (skeleton)
import Window

title = constant (JS.fromString "Elm 0.7 - Extensible Records")
foreign export jsevent "title"
  title : Signal JS.JSString

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
table.sourceCode, tr.sourceCode, td.lineNumbers, td.sourceCode {
  margin: 0; padding: 0; vertical-align: baseline; border: none; }
table.sourceCode { width: 100%; background-color: #f8f8f8; }
td.lineNumbers { text-align: right; padding-right: 4px; padding-left: 4px; color: #aaaaaa; border-right: 1px solid #aaaaaa; }
td.sourceCode { padding-left: 5px; }
pre, code { background-color: #f8f8f8; }
code > span.kw { color: #204a87; font-weight: bold; }
code > span.dt { color: #204a87; }
code > span.dv { color: #0000cf; }
code > span.bn { color: #0000cf; }
code > span.fl { color: #0000cf; }
code > span.ch { color: #4e9a06; }
code > span.st { color: #4e9a06; }
code > span.co { color: #8f5902; font-style: italic; }
code > span.ot { color: #8f5902; }
code > span.al { color: #ef2929; }
code > span.fu { color: #000000; }
code > span.er { font-weight: bold; }
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
- [Record Types](#record-types)

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

```haskell
{ x=3, y=4 }
```

Just like tuples, a record can hold values with different types, so
we can represent a book like this:


```haskell
{ title="Steppenwolf", author="Hesse", pages=237 }
```

As we will soon see, it is also possible to access, add, remove, rename,
and update fields. We will use the following records to define the rest
of the record operations:

```haskell
point2D = { x=0, y=0 }

point3D = { x=3, y=4, z=12 }

bill  = { name="Gates", age=57 }
steve = { name="Jobs" , age=56 }
larry = { name="Page" , age=39 }

people = [ bill, steve, larry ]
```

### Access

There are a number of ways to access records:

|]

access w = evaluate w
  [ ("point3D.z", "12")
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

```haskell
.x point2D == 0
.x point3D == 3
.x {x=4}   == 4
```

in a single file because each of these records has a field `x`.
Note that none these accessors pollute the global namespace.
You can still have a value `x` independent of the `(.x)` accessor.

### Pattern Matching

It is also possible to pattern match on records:

```haskell
dist {x,y} = sqrt (x^2 + y^2)

under50 {age} = age < 50
```

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
These patterns can appear in let expressions, lambda expressions,
and case expressions.

### Updating Records

It is often useful to &ldquo;update&rdquo; the values in a record.

|]

updating w = evaluate w
  [ ("{ point2D | y <- 1 }", "{ x=0, y=1 }")
  , ("{ point3D | x <- 0, y <- 0 }", "{ x=0, y=0, z=12 }")
  , ("{ steve | name <- \"Wozniak\" }", "{ name=\"Wozniak\", age=56 }")
  ]

postUpdating w = width w [markdown|
You can update as many fields as you want, separating each update by a comma.
You can even change the type of value in a field. Say the user inputs a bunch
of personal data producing a record. It would be nice to convert some of the
strings into numbers if possible. This is no problem:

```haskell
rawInput = { name="Tom", country="Finland",
             age="34", height="1.9" }

prettify person =
    { person | age    <- readInt   person.age
             , height <- readFloat person.height }

input = prettify rawInput
```

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
  [ ("{ point3D - z }", "{ x=3, y=4 }")
  , ("{ bill - age }", "{ name=\"Gates\" }")
  , ("{ point2D | z = 0 }", "{ x=0, y=0, z=0 }")
  , ("{ bill | height = 1.77 }", "{ name=\"Gates\",\n  age=57,\n  height=1.77 }")
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
  [ ("renameName bill", "{ surname=\"Gates\", age=57 }") ]

postRename w = width w [markdown|

We can also derive record updates with field addition and removal:
|]

replace w = evaluate w
  [ ("{ point2D - x | x = 1 }", "{ x=1, y=0 }") ]

postReplace w = width w [markdown|
The field update syntax is just a prettier way to write this!

### Polymorphic Fields

Elm allows [polymorphism][poly] within records, so a record field can
hold a polymorphic function like list append `(++)`. For example:

 [poly]: http://en.wikipedia.org/wiki/Parametric_polymorphism "Parametric Polymorphism"

```haskell
lib = { id x = x
      , flip f x y = f y x }

group = { zero = []
        , op a b = a ++ b }
```

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
  , ("lib.flip (::) [2,3] 1", "[1,2,3]")
  , ("group.op \"Hello\" group.zero", "\"Hello\"")
  , ("group.op [1,2] [3,4]", "[1,2,3,4]")
  ]

postPoly w = width w [markdown|
I suspect that this can be used for some really cool stuff! It should
make it possible to gain some of the flexibility of first-class modules
and typeclasses, as described in
[this announcement](/blog/announce/version-0.7.elm).

### Record Types

A record type looks very similar to actual records. Say we wanted to work
with points that have an `x` and `y` field. We could add type annotations
as follows:

```haskell
origin : { x:Float, y:Float }
origin = { x=0, y=0 }
```

We can also use type aliases to make things much more concise.

```haskell
type Point = { x:Float, y:Float }

hypotenuse : Point -> Float
hypotenuse {x,y} = sqrt (x^2 + y^2)
```

You can also define extensible records. This is generally recommended because
it makes your functions more reusable:

```haskell
type Positioned a = { a | x:Float, y:Float }
type Named      a = { a | name:String }
type Moving     a = { a | velocity:Float, angle:Float }
```

This syntax is just like the syntax for record extension, indicating that
`Positioned a` is a record with at least an `x` and `y` field, etc.

This means you can define records that have any subsection of these fields.
For example, 

```haskell
lady : Named { age:Int }
lady = { name="Lois Lane", age=31 }

dude : Named (Moving (Positioned {}))
dude = { x=0, y=0, name="Clark Kent",
         velocity=42, angle = degrees 30 }
```

Then we can make functions that only require some of those fields:

```haskell
getName : Named a -> String
getName {name} = name

names : [String]
names = [ getName dude, getName lady ]

getPos : Positioned a -> (Float,Float)
getPos {x,y} = (x,y)

positions : [(Float,Float)]
positions = [ getPos origin, getPos dude ]
```

The `getName` function works on anything with an `name` field, so it can
be used on both `lady` and `dude`. Same for `getPos` which can work on
anything with `x` and `y` fields.

So you can write small orthogonal functions that work with a wide variety of
records. You get much of the freedom of a dynamically
typed language, but the type checker will make sure that these functions are
used safely!

|]

evaluate wid pairs =
  let f (a,b) =
        let w = wid `div` 2
            c = text . monospace <| toText a
            d = text . monospace <| toText b
            h = 10 + max (heightOf c) (heightOf d)
        in  flow right [ container w h middle c
                       , container w h middle d ]
  in
  let es = map f pairs
      arrow = text . Text.height 3 . Text.color accent1 . toText <| "&rArr;"
      h = sum <| map heightOf es
  in  layers [ container wid h middle arrow
             , flow down es ]

content w = 
  flow down
    [ intro w
    , access w
    , width w postAccess
    , matches w
    , width w postMatches
    , updating w
    , postUpdating w
    , rest w
    , postRest w
    , rename w
    , postRename w
    , replace w
    , postReplace w
    , poly w
    , postPoly w
    ]

main = lift (skeleton (content . min 600)) Window.width


