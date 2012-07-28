
import Website.Docs (createDocs)


math =
  [ ("(+),(-),(*),(^)", "Number -> Number -> Number", "Functions for basic mathematics.")
  , ("(/)", "Float -> Float -> Float", "Division.")
  , ("div", "Int -> Int -> Int", "Integer division, remainder is discarded.")
  , ("rem", "Int -> Int -> Int", "Finds the remainder after dividing one number by another. (4 `rem` 3) == 1.")
  , ("mod", "Int -> Int -> Int", "Perform modular arithmetic. (-2 `mod` 5) == 3")
  , ("sin, cos, tan, asin, acos, atan", "Number -> Number", "Basic functions of trigonometry.")
  , ("sqrt", "Number -> Number", "Take the square root of a number.")
  , ("abs", "Number -> Number", "Take the absolute value of a number.")
  , ("logBase", "Number -> Number -> Number", "Calculate the logarithm of a number with a given base: logBase 10 100 == 2.")
  , ("min, max", "Number -> Number -> Number", "Given two numbers, returns the smaller (or greater respectively).")
  , ("clamp", "Number -> Number -> Number -> Number", "Clamps a number within a given range, so (clamp 100 200 x) is 200 for x >= 200, 100 for x <= 100, and x for any 100 < x < 200 ")
  , ("pi", "Float", "An approximation of pi.")
  , ("e", "Float", "An approximation of e.")
  ]

convert =
  [ ("round", "Float -> Int", "Round a number to the nearest integer.")
  , ("truncate", "Float -> Int", "Truncate a decimal number, rounding towards zero.")
  , ("floor", "Float -> Int", "Floor function, rounding down.")
  , ("ceiling", "Float -> Int", "Ceiling function, rounding up.")
  , ("toFloat", "Int -> Float", "Convert an integer into a float.")
  ]

funcs =
  [ ("(.)", "(b -> c) -> (a -> b) -> (a -> c)", "Function composition: f . g == (\\x -> f (g x))")
  , ("($)", "(a -> b) -> a -> b", "Function application (f $ x == f x). This function is useful for avoiding parenthesis. Consider the following code to create a text element: (text (monospace (toText \"code\"))). This can also be written as (text . monospace $ toText \"code\").")
  , ("id", "a -> a", "Given a value, returns exactly the same value.")
  , ("flip", "(a -> b -> c) -> (b -> a -> c)", "Flips the order of the first two arguments to a function.")
  ]

categories = 
  [ ("Mathematics", math)
  , ("Number Conversions", convert)
  , ("Function Helpers", funcs)
  ]

main = createDocs "Prelude" categories