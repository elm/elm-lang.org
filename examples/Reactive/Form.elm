
isEmpty xs = case xs of { [] -> True ; _ -> False }
when bool value = if bool then Just value else Nothing
catMaybes = List.foldr (\opt es -> case opt of { Just e -> e:es ; Nothing -> es }) []
listify = List.foldr (lift2 (:)) (constant [])
eq a b = List.length a == List.length b && List.and (List.zipWith (==) a b)

check (signal, pred, msg) = lift (\x -> when (pred x) msg) signal
checks = lift catMaybes . listify . List.map check

field txt fld =
  constant . width 400 . flow right $
  [ size 120 30 . box 6 $ plainText txt, size 200 30 . box 5 . size 180 20 $ fld ]

form (firstBox, firstName) (lastBox, lastName) (emailBox, email) (remailBox, remail) =
  let requirements = [ (firstName, isEmpty, "First name required.")
                     , ( lastName, isEmpty,  "Last name required.")
                     , ( email, isEmpty, "Must enter your email address.")
                     , (remail, isEmpty, "Must re-enter your email address.")
                     , (lift2 eq email remail, not, "Email addresses do not match.")
                     ]
  in  lift (flow down) . listify $
        [ field "First Name:" firstBox
        , field "Last Name:" lastBox
        , field "Your Email:" emailBox
        , field "Re-enter Email:" remailBox
        , lift (flow down . List.map (text . Text.color red . toText)) (checks requirements)
        ]

main = form 
  (Input.textField "First Name")
  (Input.textField "Last Name")
  (Input.textField "Your Email")
  (Input.textField "Re-enter Email")