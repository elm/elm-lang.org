{-----------------------------------------------------------------

Overview: This code demonstrates a more advanced use of algebraic
data types. Here we are creating an abstract representation of
Boolean expressions and defining an evaluation strategy. These
tasks are the first steps towards writing an interpreter for a
programming language. We will see the following functions:

  Expr - an algebraic data type that represents
    simple boolean expressions.

  eval - recursively computes the value of a given
    boolean expression (Expr).

Finally, we will see these functions in action with some examples.

-----------------------------------------------------------------}

data Expr = T | F | Not Expr | And Expr Expr | Or Expr Expr

eval expr =
  case expr of
    T -> True
    F -> False
    Not e -> not (eval e)
    And e1 e2 -> eval e1 && eval e2
    Or  e1 e2 -> eval e1 || eval e2

e1 = T
e2 = And T F
e3 = Or e1 e2
e4 = And (Not e2) e1

main = flow down <| map display [ e1, e2, e3, e4 ]

display e =
  text . monospace . toText <| show (eval e) ++ " &lArr; " ++ show e