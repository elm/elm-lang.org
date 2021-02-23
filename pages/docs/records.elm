import Html exposing (..)
import Html.Attributes exposing (..)

import Skeleton
import Center


main =
  Skeleton.docs
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
- [Large Records](#large-records)

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
you probably want [custom types](https://guide.elm-lang.org/types/custom_types.html).
So if you find yourself struggling with records to make them act like objects,
keep in mind that there is probably a different tool for the job.


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
{ point2D | y = 1 }           -- { x = 0, y = 1 }
{ point3D | x = 0, y = 0 }    -- { x = 0, y = 0, z = 12 }
{ steve | name = "Wozniak" }  -- { name = "Wozniak", age = 56 }
```

Updates always produce a _new_ record.


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

Record type aliases are extremely common in Elm code!


## Large Records

People coming from JavaScript tend to (1) overuse records and (2) have a habit of breaking
records into smaller records and to distribute them among a bunch of different files. These
are both traps in Elm!

Large records are no problem in Elm. Records can often have ten or twenty or more fields.
The thing to focus on is the relationships between fields. Joël Quenneville recognized two
common pitfalls for people new to Elm records:

- [Overuse of `Bool` fields](https://thoughtbot.com/blog/booleans-and-enums).
- [Overuse of `Maybe` fields](https://thoughtbot.com/blog/modeling-with-union-types)

In both cases, the problems arise from imprecise data modeling. Two or three fields are
used to model a single concept. Rather than trying to shuffle record fields around, the
best path is often to create a [custom type](https://guide.elm-lang.org/types/custom_types.html)
to model the possible situations more precisely. Richard Feldman called this
[Making Impossible States Impossible](https://youtu.be/IcgmSRJHu_8). When you
discover that two or three fields can be better represented by a single custom type,
it does not just make the record nicer. All the code that uses this data structure
will fit together nicer as well!

So a record that feels "too large" is often a signal that there are relationships between
fields that are not modeled explicitly. Instead of shuffling fields around, try to find
a custom type that makes these relationships explicit!

And if you are feeling the urge to break files into smaller parts too, start by checking
out some additional resources on how to grow Elm code:

- [Modules](https://guide.elm-lang.org/webapps/modules.html) and
- [Code Structure](https://guide.elm-lang.org/webapps/structure.html)
- [The Life of a File](https://youtu.be/XpDsk374LDE)
- [Scaling Elm Apps](https://youtu.be/DoA4Txr4GUs)

I hope these resources will help you develop an instinct for when to reach for records vs
when to reach for custom types and modules! The skillful use of custom types seems to come
with experience, so do not feel discouraged if it is not intuitive at first. I have been
programming in languages like Elm for more than a decade and still struggle to get certain
custom types *just* right!

I found that a great way to improve is to share your scenario with others and ask for help,
especially if you find someone with good instincts! It seems like people slowly build a
mental catalog of techniques as they discover nice custom types, and their instincts slowly
improve as they try things out in their own code. It will come with time, and I hope the
resources here are a good start! 

"""
