
import Signal.HTTP (gets)
import Signal.Input (textField)


valid s = length s == 5 && onlyDigits s
onlyDigits = let digits = ['0','1','2','3','4','5','6','7','8','9'] in
             forall (\c -> exists ((==)c) digits)

url = "http://zip.elevenbasetwo.com/v2/US/"

component (fld, txt) =
  let { toUrl s = if valid s then Just (url ++ s) else Nothing
      ; input = plainText "Enter a US Zip Code:" `beside` fld
      ; address = lift display . gets . lift toUrl $ txt
      }
  in  lift (above input) address

display response = 
  (case response of
   { Just address -> asText address
   ; Nothing -> plainText "Not a valid address. Maybe try 12345."
   })

main = component (textField "")