import Markdown
import Message exposing (report)

main = report <| Markdown.toElement """

# Online Editor

Write and compile code online!

  * <a href="/examples/HelloWorld" target="_top">Hello World!</a>
  * <a href="/examples/Position" target="_top">Mouse</a>
  * <a href="/examples/Clock" target="_top">Clock</a>

For more guidance check out:

  * <a href="/examples" target="_top">More examples</a>
  * <a href="/docs" target="_top">Learning resources</a>


Here are the example programs for 0.15:

  * <a href="/examples/zip-codes" target="_top">Zip Code Lookup</a>
  * <a href="/examples/flickr" target="_top">Flickr Search</a>

"""
