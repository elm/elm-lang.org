import Markdown
import Website.Message (report)

main = report <| Markdown.toElement """

# Online Editor

Write and compile code online!

  * <a href="/edit/examples/Elements/HelloWorld.elm" target="_top">Hello World!</a>
  * <a href="/edit/examples/Reactive/Position.elm" target="_top">Mouse</a>
  * <a href="/edit/examples/Intermediate/Clock.elm" target="_top">Clock</a>

For more guidance check out:

  * <a href="/Examples.elm" target="_top">More examples</a>
  * <a href="/Learn.elm" target="_top">Learning resources</a>

"""
