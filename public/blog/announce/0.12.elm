
import Website.Skeleton (skeleton)
import Window
import JavaScript as JS

port title : String
port title = "Elm 0.12 - Ports"

main = lift (skeleton everything) Window.dimensions

everything wid =
    let w = min 600 wid
    in  width w intro

intro = [markdown|

<style type="text/css">
p { text-align: justify }
pre { background-color: white;
      padding: 10px;
      border: 1px solid rgb(216, 221, 225);
      border-radius: 4px;
}
code > span.kw { color: #268BD2; }
code > span.dt { color: #268BD2; }
code > span.dv, code > span.bn, code > span.fl { color: #D33682; }
code > span.ch { color: #DC322F; }
code > span.st { color: #2AA198; }
code > span.co { color: #93A1A1; }
code > span.ot { color: #A57800; }
code > span.al { color: #CB4B16; font-weight: bold; }
code > span.fu { color: #268BD2; }
code > span.re { }
code > span.er { color: #D30102; font-weight: bold; }
</style>

<h1><div style="text-align:center">Elm 0.11.2
<div style="padding-top:4px;font-size:0.5em;font-weight:normal">*Simplifying User Input*</div></div>
</h1>

## Accidentally well-architected code

Due to the design of FRP in Elm, Elm programs *always* break into four distinct
parts:

  1. Input &mdash; events from “the world” (keyboard, mouse, touch, time, etc.)

  2. Model &mdash; a full representation of our Elm component.

  3. Update &mdash; functions for updating our model based on inputs.

  4. Display &mdash; functions describing the user’s view of the model.

The key take away here is not that this is a good archetecture for designing a
GUI or that you should always try to split your projects into these parts. I
mean, those are true things, but the important take away is that **the design of
FRP in Elm *forces* you to to write code with this architecture**. I often find
myself at the end of a messy hacking / prototyping session with Elm code that
is mysteriously well-architected. I know I did not plan ahead or have a deep
understanding of how the different parts of my program would eventually fit
together, yet the input, model, update, and display are all neatly separated in
my rough draft. This ultimately comes down to the specific design choices made
in Elm's formulation of FRP. The fact that programs tend to be easy and pleasant
to write makes the &ldquo;accidentally well-architected&rdquo; property one of
the most important parts of Elm. You are not paying more in pain or frustration
to get a better result.

I bring this up specifically because I wanted to keep &ldquo;accidentally
well-architected&rdquo; property when introducing typical user input elements
like buttons, checkboxes, and text fields. As people who have used MVC
frameworks will know, UI elements seem to inherently ruin our nice separation
of concerns: by introducing a text field, we suddenly have inputs and model in
our view! Until now, UI elements in Elm have used a rather convoluted and
confusing API to keep the clean division between inputs, model, update, and
display. This release finally introduces UI elements that are easy to use *and* 
keep the &ldquo;accidentally well-architected&rdquo; property of Elm!

## Making inputs explicit

This release introduces the concept of an `Input` that UI elements can report to.
When I type in a text field or click a button, that event will always be sent to
an `Input`. This makes our inputs explicit again, decoupling our input and view.

|]
