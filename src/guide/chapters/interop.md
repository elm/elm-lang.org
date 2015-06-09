
# Interop

Interop is extremely important for any language that compiles to JavaScript. To be viable in a commercial setting, you need to be able to reuse helpful libraries and tools to get things done quickly. Elm has two primary approaches for interop:

  * [Ports](#ports) &mdash; ports allow you to send messages to and from Elm. You can send messages out to JavaScript, do all sorts of crazy JavaScript stuff, and then send the results back into Elm to update your application. This is the recommended route for normal users.

  * [Tasks](#tasks) &mdash; tasks let you wrap up any crazy JavaScript API in a way that will play nice with Elm. Generally speaking, this tool is for experienced library designers. When wrapping JS APIs it is important to design things such that your new API maintains all the guarantees that Elm is built on: immutability, managed effects, statelessness, etc.

## HTML Embedding

## Ports

## Tasks
