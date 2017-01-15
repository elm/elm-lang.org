import Html exposing (..)

import Blog
import Center


main =
  Blog.docs
    "Style Guide"
    [ Center.markdown "600px" content
    ]


content = """

**Goal:** a consistent style that is easy to read and produces clean diffs.
This means trading aggressively compact code for regularity and ease of
modification.

## Line Length

Keep it under 80 characters. Going over is not the end of the world, but
consider refactoring before you decide a line really must be longer. Long lines
can be a good sign that it is time to break things up and refactor.


## Variables

**Be Descriptive.** One character abbreviations are rarely acceptable,
especially not as arguments for top-level function declarations where you have
no real context about what they are.

**Qualify variables.** Always prefer qualified names. `Set.union` is always
preferable to `union`. In large files and in large projects, it becomes very
very difficult to figure out where variables came from without this.


## Declarations

Always have type annotations on top-level definitions.

Always have 2 empty lines between top-level declarations.

Always bring the body of the declaration down one line.

### Good

```elm
homeDirectory : String
homeDirectory =
  "/root/files"


evaluate : Boolean -> Bool
evaluate boolean =
  case boolean of
    Literal bool ->
        bool

    Not b ->
        not (evaluate b)

    And b b_ ->
        evaluate b && evaluate b_

    Or b b_ ->
        evaluate b || evaluate b_
```

Now imagine one of the cases in `evaluate` becomes drastically more
complicated. Nothing needs to be reformatted so the diff will be minimal and
the result will still look quite nice.


### Bad

```elm
homeDirectory = "/root/files"

eval boolean = case boolean of
    Literal bool -> bool
    Not b        -> not (eval b)
    And b b_     -> eval b && eval b_
    Or b b_      -> eval b || eval b_
```

We saved vertical lines here, but at the cost of regularity and ease of
modification. If `Literal` ever becomes longer, all arrows must move. If any
branch gets too long, everything needs to come down a line anyway.

Having `case` appear *later* than the actual cases is strongly discouraged. It
should serve as a context clue that makes glancing through code easy, but when
indented in crazy ways, it becomes more difficult to glance through.


## Types

Do not be a maniac with indentation. Simplicity will be better in the long run.


### Good

```elm
type Boolean
    = Literal Bool
    | Not Boolean
    | And Boolean Boolean
    | Or Boolean Boolean


type alias Circle =
    { x : Float
    , y : Float
    , radius : Float
    }


type alias Graph =
    List (Int, List Int)
```

### Bad

```elm
type Boolean = Literal Bool
             | Not Boolean
             | And Boolean Boolean
             | Or Boolean Boolean

type alias Circle = {
    x      : Float,
    y      : Float,
    radius : Float
}

type alias Graph = List (Int, List Int)
```

Changing the name `Boolean` ever will change the indentation on all subsequent
lines. This leads to messy diffs and provides no concrete value.

If we ever add a new field to `Circle` that is longer than `radius` we have to
change the indentation of all lines, leading to a bad diff. Furthermore, ending
lines with a comma makes diffs messier because adding a field must change two
lines instead of one.

If we change the name of the type alias `Graph` it'll be less clear if
everything is on the same line. Did we change the name or the meaning?
Furthermore, if the type being aliased ever becomes too long, it will need to
move down a line anyway.

"""
