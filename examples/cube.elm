-- Render a spinning cube.
--
-- Dependencies:
--   elm install elm-explorations/linear-algebra
--   elm install elm-explorations/webgl
--

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import WebGL



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  Float


init : () -> (Model, Cmd Msg)
init () =
  ( 0, Cmd.none )



-- UPDATE


type Msg
  = TimeDelta Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg angle =
  case msg of
    TimeDelta dt ->
      ( angle + dt / 5000, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta



-- VIEW


view : Model -> Html Msg
view angle =
  WebGL.toHtml
    [ width 400, height 400, style "display" "block"
    ]
    [ WebGL.entity vertexShader fragmentShader cubeMesh (uniforms angle)
    ]


type alias Uniforms =
  { rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  }


uniforms : Float -> Uniforms
uniforms angle =
  { rotation =
      Mat4.mul
        (Mat4.makeRotate (3 * angle) (vec3 0 1 0))
        (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
  , perspective = Mat4.makePerspective 45 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  }



-- MESH


type alias Vertex =
  { color : Vec3
  , position : Vec3
  }


cubeMesh : WebGL.Mesh Vertex
cubeMesh =
  let
    rft = vec3 1 1 1
    lft = vec3 -1 1 1
    lbt = vec3 -1 -1 1
    rbt = vec3 1 -1 1
    rbb = vec3 1 -1 -1
    rfb = vec3 1 1 -1
    lfb = vec3 -1 1 -1
    lbb = vec3 -1 -1 -1
  in
  WebGL.triangles <| List.concat <|
    [ face (vec3 115 210 22 ) rft rfb rbb rbt -- green
    , face (vec3 52  101 164) rft rfb lfb lft -- blue
    , face (vec3 237 212 0  ) rft lft lbt rbt -- yellow
    , face (vec3 204 0   0  ) rfb lfb lbb rbb -- red
    , face (vec3 117 80  123) lft lfb lbb lbt -- purple
    , face (vec3 245 121 0  ) rbt rbb lbb lbt -- orange
    ]


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
  let
    vertex position =
      Vertex (Vec3.scale (1 / 255) color) position
  in
  [ ( vertex a, vertex b, vertex c )
  , ( vertex c, vertex d, vertex a )
  ]



-- SHADERS


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 rotation;
    varying vec3 vcolor;
    void main () {
        gl_Position = perspective * camera * rotation * vec4(position, 1.0);
        vcolor = color;
    }
  |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;
    varying vec3 vcolor;
    void main () {
        gl_FragColor = 0.8 * vec4(vcolor, 1.0);
    }
  |]
