# Data Structures

## Lists

One of the most common data structures in Elm is the list. Lists can hold
many values, and those values must all have the same type. For example,

```haskell
numbers : List Int
numbers = [1,2,3]
```

This could be read &ldquo;`numbers` has type list of ints.&rdquo; If you
wanted to represent a list of names:

```haskell
names : List String
names = ["Alice", "Bob", "Chuck"]
```

Again, the key thing is that all elements of the list have exactly the
same type.


## Tuples

Tuples are another useful data structure. A tuple can hold a fixed number of
values, and each value can have any type. The most common use is for
representing a point:

```haskell
point : (Int,Int)
point =
  (3,4)
```

This pair of integers is the most basic tuple. Tuples are mainly for grouping
information, so you could use them to represent a book, holding the title,
author, and number of pages.

```haskell
book : (String,String,Int)
book =
  ("Demian","Hesse",176)
```

This illustrates that you can hold many different values, each with a different
type. When the data structure becomes more complicated or specific, 
it is often best to use records instead tuples.


## Records

Elm also has [records][records] which let you have more structured
values. Say you want to make a list of high quality books. We can put
them in a record that has a title, author, and number of pages:

```haskell
book1 : { title : String, author : String, pages : Int }
book1 =
  { title = "Demian"
  , author = "Hesse"
  , pages = 176
  }
```

We can use type aliases to make things a bit clearer.

```haskell
type alias Book = { title : String, author : String, pages : Int }

book2 : Book
book2 =
  { title = "Magister Ludi"
  , author = "Hesse"
  , pages = 558
  }

books : List Book
books =
  [ book1, book2 ]
```

In the tuple version of the book, it was unclear from the type
which `String` was the title and which was the author. You would have to
read some code or do some experiment to figure it out. With records, it is
totally clear and extracting a title is as simple as saying `book2.title`.

## Union Types
