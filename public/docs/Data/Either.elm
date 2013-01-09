
import Website.Docs (createDocs2)

def =
  [ ("data Either a b = Left a | Right b", "", [markdown|
Represents any data that can take two different types.

This can also be used for error handling `(Either String a)` where error
messages are stored on the left, and the correct values (&ldquo;right&rdquo; values) are stored on the right.
|])
  ]

basics =
  [ ("either", "(a -> c) -> (b -> c) -> Either a b -> c", [markdown|
Apply the first function to a `Left` and the second function to a `Right`.
This allows the extraction of a value from an `Either`.
|])
  , ("isLeft", "Either a b -> Bool", [markdown|True if the value is a `Left`.|])
  , ("isRight", "Either a b -> Bool", [markdown|True if the value is a `Right`.|])
  ]

lists =
  [ ("lefts", "[Either a b] -> [a]", [markdown|
Keep only the values held in `Left` values.|])
  , ("rights", "[Either a b] -> [b]", [markdown|
Keep only the values held in `Right` values.|])
  , ("partition", "[Either a b] -> ([a],[b])", [markdown|
Split into two lists, lefts on the left and rights on the right. So we
have the equivalence: `(partition es == (lefts es, rights es))`
|])
  ]

categories = [ ("Definition", def)
             , ("Basics", basics)
             , ("With Lists", lists) ]

intro = [markdown||]

main = createDocs2 "Either" intro categories
