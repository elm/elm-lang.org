
{-----------------------------------------------------------------

Overview: The "Maybe" algebraic data type can either have a value
(Just 4) or it can have no value (Nothing). "Nothing" is sort of
like saying the value is undefined.

This allows us to turn partial functions (functions that are
undefined for some inputs) into total functions (defined on all
inputs).

For instance, log(n) is undefined for all n <= 0, so it is a
partial function. The type of this partial function would be:

     log : Number -> Number

Which is not actually true! The "log" function can also produce
errors or "null" values, but this does not appear in the type.
Using "Maybe" fixes this issue, giving us a total function:

    safeLog : Number -> Maybe Number

From the type, we know that "safeLog" may not produce a number,
but it still always produces a value, even if it is "Nothing"!

-----------------------------------------------------------------}


data Maybe a = Just a | Nothing

safeLog n = if n <= 0 then Nothing else Just (logBase 10 n)
safeHead xs = case xs of
                h::t -> Just h
                []   -> Nothing




main = flow down [ display "safeLog" safeLog 100,
                   display "safeLog" safeLog -1,
                   display "safeHead" safeHead [2,3,5,7,11],
                   display "safeHead" safeHead [] ]

display name f v =
  text . monospace . toText <|
  show (f v) ++ " &lArr; " ++ name ++ " " ++ show v