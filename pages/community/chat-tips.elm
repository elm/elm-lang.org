import Html exposing (..)
import Html.Attributes exposing (..)
import String

import Center
import Skeleton


main =
  Skeleton.skeleton "Elm - Chat Tips" Skeleton.Community
    [ Center.markdown "600px" content
    ]

content = """

# Chat Tips

Some folks coming to chat are really frustrated. Maybe they just spent 4hrs trying to debug something. Imagine how you feel when you spend 4hrs on something annoying!

It is not possible to give hugs online, so here are some tips on how to **be supportive** in addition to helping with technical problems.

* * *

✅ **Acknowledge their frustration**

🐥: I am trying to do X.<br>
🐨: Ah, that can be really tricky. I will try to help!

❌ **Deny their experience**

🐥: I am trying to do X.<br>
🐨: Oh, that is easy/obvious/simple!

* * *

✅ **Ask Questions / learn about their situation**

🐥: I am trying to do X.<br>
🐨: Ah, that can be really tricky. I will try to help!<br>
🐨: What resources have you been looking at so far?<br>
🐥: I read about it at https://guide.elm-lang.org/<br>
🐥: But now I am using USER/PROJECT now, and having trouble understanding how `func` can do X.

❌ **Give Orders / disregard their situation**

🐥: I am trying to do X.<br>
🐨: Ah, that can be really tricky. I will try to help!<br>
🐨: You should read The Official Guide. It has a section about this.<br>
🐥: I read it, but I am still having trouble with X. That is why I am asking here!

* * *

✅ **Offer support**

🐥: I am going to try X and see what happens.<br>
🐨: I'll be around for the next half hour, so ping me if you run into something else! Hope it works!

❌ **Move on immediately**

🐥: I am going to try X and see what happens.<br>
🐨: I think 🐙 can just `List.foldl` over it. It is tail-recursive.

* * *

More generally, here are some exercises that might help:

- Try thinking of times when you were stuck on something and started asking strangers for help online. Was there anything frustrating about those interactions? Were there things you wished people had done different? Can you make those changes in your own behavior?

- If you meet someone learning Elm in real life, ask them if they have had any frustrating online interactions. What happened? What was frustrating about it? Is it possible to share their story with the relevant channel without revealing their identity? Maybe sharing their story will help prevent similar frustrating interactions in the future. Try to advocate on their behalf!

Hopefully these are helpful recommendations!

"""
