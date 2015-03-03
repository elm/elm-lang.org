
{----------------------------------------------------------------

Overview:
  Function application has a special symbol (<|) which allows us
  to write way fewer parenthesis. That's really the whole point
  of this function.

  When we say (f <| x), it is exactly equivalent to (f x). Nothing
  surprising here. When we say (f <| x + 1), it is the same as
  saying (f (x + 1)) only we didn't need the parenthesis.

----------------------------------------------------------------}

import Text exposing (asText)


main =
  asText <| 3 + 2