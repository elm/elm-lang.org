import Html exposing (..)
import Html.Attributes exposing (..)

import Blog
import Center


(=>) = (,)


main =
  Blog.blog
    "How to Use Elm at Work"
    ""
    Blog.evan
    (Blog.Date 2016 7 11)
    [ Center.markdown "600px" start
    ]


start = """

Last week I released [`react-elm-components`](https://www.npmjs.com/package/react-elm-components), making it simple to write a small part of your React app in Elm. If you are using React and JSX, it looks something like this:

<a href="https://github.com/evancz/react-elm-components/tree/master/example"
  style="text-decoration: none"
  title="Check out the full Emoji Chat Room example.">
```javascript
import Elm from 'react-elm-components';
import {Chat} from '../dist/elm/chatroom.js';

function render() {
    return <Elm src={Chat} />;
}
```
</a>

I created this package to help folks use Elm successfully at work. Tons of folks are trying out Elm these days, and as they share their experiences, I noticed that everyone who successfully introduces Elm at work tells exactly the same story. **They introduce Elm gradually into an existing JS project.** I want to make this path as easy as possible.

The rest of this blog post outlines the typical success story in detail. What strategies work? What are some common mistakes?

> **Note:** The `react-elm-components` module is under 20 lines of JS, mostly React-related. So using the same strategy with Angular, Ember, etc. should be pretty simple. More about that [here](https://github.com/evancz/react-elm-components#angular-ember-etc).


## Gradual Introduction

Every company that uses Elm in production follows a similar path:

  1. **Learn** &mdash; Someone on the team starts looking into Elm. They build something on their own. They get familiar with Elm. They become the *advocate*. They &ldquo;own&rdquo; the remaining steps.

  2. **Experiment** &mdash; That person writes one little thing in Elm at work to see how it goes. They do not have a massive migration plan or anything. They just [do it](https://youtu.be/ZXsQAXx_ao0).

  3. **Evaluate** &mdash; If that experiment goes well, they do more. And if it does not go well, they revert it and go back to the other stuff.

  4. **Repeat** &mdash; The person who introduced the original code helps everyone on the team learn Elm as it is used in more stuff.

Again, this is *the* success story. I hear it again and again from folks using Elm in production. I do not hear other stories.

**This process works because it minimizes risk.** Having an advocate means you have someone on the team who knows Elm really well. They can answer questions quickly in-house. Furthermore, you are trying something *small* in Elm. Maybe it is whipped up in a single day even. Point is, you are not pouring time and money into a project that may not work out. This means trying out Elm is really not a big commitment at all. Big upside, very little downside.


## Fix a Problem

This broad outline is great, but it is important to be strategic in choosing your first project. Do not just write Elm code because you like it. **Fix a problem.**

There are two stories you can tell after rewriting code:

  - &ldquo;You know that crazy code that everyone fears touching? It is simpler now and I resolved some bugs.&rdquo;

  - &ldquo;You know that code that was totally fine? It is different now.&rdquo;

You want to tell the first story. In the ideal telling, the new version is better because of a particular detail about Elm. Maybe that is immutability or The Elm Architecture or friendly error messages. Not only is it better, but it is better in a way that can only be achieved with Elm!

I tried to illustrate this in [the Emoji Chat Room example](https://github.com/evancz/react-elm-components/tree/master/example) that comes with [`react-elm-components`](https://www.npmjs.com/package/react-elm-components). The Emoji picker is some off-the-shelf React component, and the chat room is a custom Elm program. We chose this split because (1) we cannot get the chat part off-the-shelf with the style we want and (2) websockets are a pleasure to work with in Elm. In other words, we chose to use Elm in a situation where it really shines!


## The Checklist

The previous sections give you a flavor of what to do, but I wanted to make a checklist to make it more concrete. **If you want to use Elm at work, make sure you are doing the following things:**

  - **Have an advocate** &mdash; There should be one person on your team who has experience with Elm on a hobby project. They drive the initiative and help team members with anything they run into.

  - **Start Small** &mdash; It is best to introduce Elm gradually. Choose a small part of an existing project. Your first work project in Elm should not be a green-field project or high-stakes rewrite.

  - **Fix a Problem** &mdash; Use Elm where it will really shine. Elm may be nicer for all sorts of stuff, but the best story comes when you resolve issues that hurt your team every day.

  - **Write Elm Code** &mdash; You can talk a decision like this to death. What about this? What about that? Talking abstractly is a waste of everyone&rsquo;s time. Write an actual chunk of Elm code. Evaluate it against an actual chunk of JavaScript code. Talk concretely.

Hopefully that helps you out! I would love to hear how it goes, so definitely share your story as a blog post or through one of [the community forums](http://elm-lang.org/community). The community is really friendly and will definitely benefit from your experience, whether it works out or not. And who knows, maybe we can help out in some way!


## Thanks

Huge thank you to [Richard](https://twitter.com/rtfeldman) who got the `react-elm-components` example set up and looking good!

"""