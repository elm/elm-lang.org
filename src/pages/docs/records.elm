import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


(=>) = (,)


main =
  Blog.docs
    "Records"
    [ Center.markdown "600px" content ]

content = """

The overall goal of records is to organize related data in a way that is super
easy to access and update. They kind of fill the role of objects in JavaScript
or Java, but with some key distinctions.

The structure of the following document is as follows:

- [Comparison with Objects](#comparison-of-records-and-objects)
- [What is a Record?](#what-is-a-record-)
- [Record Access](#access)
- [Pattern Matching for Records](#pattern-matching)
- [Updating Fields](#updating-records)
- [Record Types](#record-types)

## Comparison of Records and Objects

Records in Elm are quite similar to objects in JavaScript. The major differences
are that with records:

- You cannot ask for a field that does not exist.
- No field will ever be `undefined` or `null`.
- You cannot create recursive records with a `this` or `self` keyword.

I highly encourage a strict separation of data and logic, and as far as I can
tell, the ability to say `this` is primarily used to break this separation.
This is a systemic problem in Object Oriented languages that I would like to
avoid.

It is also important to note that many languages try to use objects for
*everything*. Records fill a much more limited role in Elm. If you want
modularity, you use modules. If you want something that seems like subtyping,
you probably want [union types](/guide/model-the-problem). So if you find
yourself struggling with records to make them act like objects, keep in mind
that there is probably a different tool for the job.


## What is a Record?

A record is a lightweight labeled data structure. For instance, if we wanted
to represent a point we just create a record with an `x` and `y` field:

```elm
{ x = 3, y = 4 }
```

Just like tuples, a record can hold values with different types, so we can
represent a book like this:

```elm
{ title = "Steppenwolf", author = "Hesse", pages = 237 }
```

As we will soon see, it is also possible to access and update these fields. We
will use the following records to define the rest of the record operations:

```elm
-- POINTS

point2D =
  { x = 0
  , y = 0
  }

point3D =
  { x = 3
  , y = 4
  , z = 12
  }


-- PEOPLE

bill =
  { name = "Gates"
  , age = 57
  }

steve =
  { name = "Jobs"
  , age = 56
  }

larry =
  { name = "Page"
  , age = 39
  }

people =
  [ bill
  , steve
  , larry
  ]
```

## Access

There are a number of ways to access records:

```elm
point3D.z             -- 12
bill.name             -- "Gates"
.name bill            -- "Gates"
List.map .age people  -- [57,56,39]
```

The first way to access records is fairly standard, appearing in many
languages, from JavaScript to OCaml. No spaces are permitted on either
side of the dot with this method.

The second way to access records is with a special function `.name` that
is equivalent to `(\\r -> r.name)`, making things a bit more concise.

The only requirement is that the accessor is used on a record that actually
has that field, the other fields in the record do not matter. So it is
perfectly acceptable to say:

```elm
.x point2D     -- 0
.x point3D     -- 3
.x { x = 4 }   -- 4
```

No matter the shape of the record, the function `.x` will work as long as the
record has field `x`.


## Pattern Matching

It is also possible to pattern match on records. The following `hypotenuse`
function will &ldquo;destructure&rdquo; its argument. It will require that
whenever `hypotenuse` is called, the argument has at least an `x` and `y` field
that are floats.

```elm
hypotenuse {x,y} =
  sqrt (x^2 + y^2)

-- hypotenuse point2D == 0
-- hypotenuse point3D == 5
```

So it can be used on both `point2D` and `point3D` no problem.

Here is another example that just figures out if the `age` field is less
than 50. Again, it works on any record that has an `age` field no matter what
other fields are in there.

```elm
under50 {age} =
  age < 50

-- under50 bill            == False
-- List.any under50 people == True
```

Patterns for destructuring records can appear in let expressions, lambda
expressions, and case expressions. Anywhere that patterns are allowed, you can
do this.


## Updating Records

It is often useful to &ldquo;update&rdquo; the values in a record.

```elm
{ point2D | y = 1 }           -- { x=0, y=1 }
{ point3D | x = 0, y = 0 }    -- { x=0, y=0, z=12 }
{ steve | name = "Wozniak" }  -- { name="Wozniak", age=56 }
```

You can update as many fields as you want, separating each update by a comma.
You can even change the type of value in a field. Say the user inputs a bunch
of personal data producing a record. It would be nice to convert some of the
strings into numbers if possible. This is no problem:

```elm
rawInput =
  { name = "Tom"
  , country = "Finland"
  , age = "34"
  , height = "1.9"
  }

prettify person =
  { person |
      age = String.toInt person.age,
      height = String.toFloat person.height
  }

input =
  prettify rawInput
```

We started with a record in which `(person.age : String)`, providing little
information about the validity of the input. The result is that
`(person.age : Result String Int)`, fully capturing the type of input we are
dealing with and whether or not it is valid.

The update functions allow you to write fairly elaborate update functions
with little trouble.


## Record Types

A record type looks very similar to actual records. Say we wanted to work
with points that have an `x` and `y` field. We could add type annotations
as follows:

```elm
origin : { x : Float, y : Float }
origin =
  { x = 0
  , y = 0
  }
```

We can also use type aliases to make things much more concise.

```elm
type alias Point =
  { x : Float
  , y : Float
  }

hypotenuse : Point -> Float
hypotenuse {x,y} =
  sqrt (x^2 + y^2)
```

You can also define extensible records. This use has not come up much in
practice so far, but it is pretty cool nonetheless.

```elm
type alias Positioned a =
  { a | x : Float, y : Float }

type alias Named a =
  { a | name : String }

type alias Moving a =
  { a | velocity : Float, angle : Float }
```

This syntax is defining types that have *at least* certain fields, but may have
others as well. So `Positioned a` is a record with at least an `x` and `y`
field.

This means you can define records that have any subsection of these fields.
For example,

```elm
lady : Named { age:Int }
lady =
  { name = "Lois Lane"
  , age = 31
  }

dude : Named (Moving (Positioned {}))
dude =
  { x = 0
  , y = 0
  , name = "Clark Kent"
  , velocity = 42
  , angle = degrees 30
  }
```

Then we can make functions that only require some of those fields:

```elm
getName : Named a -> String
getName {name} =
  name

names : List String
names =
  [ getName dude, getName lady ]

getPos : Positioned a -> (Float,Float)
getPos {x,y} =
  (x,y)

positions : List (Float,Float)
positions =
  [ getPos origin, getPos dude ]
```

The `getName` function works on anything with an `name` field, so it can
be used on both `lady` and `dude`. Same for `getPos` which can work on
anything with `x` and `y` fields.

So you can write small orthogonal functions that work with a wide variety of
records. You get much of the freedom of a dynamically
typed language, but the type checker will make sure that these functions are
used safely!


"""
