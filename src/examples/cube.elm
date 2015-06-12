import Color exposing (..)
import Graphics.Element exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import Time exposing (..)
import WebGL exposing (..)


-- SIGNALS

main : Signal Element
main =
  Signal.map (webgl (400,400)) (Signal.map scene angle)


angle : Signal Float
angle =
  Signal.foldp (\dt theta -> theta + dt / 5000) 0 (fps 25)


-- MESHES - create a cube in which each vertex has a position and color

type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cube : List (Triangle Vertex)
cube =
  let
    rft = vec3  1  1  1   -- right, front, top
    lft = vec3 -1  1  1   -- left,  front, top
    lbt = vec3 -1 -1  1
    rbt = vec3  1 -1  1
    rbb = vec3  1 -1 -1
    rfb = vec3  1  1 -1
    lfb = vec3 -1  1 -1
    lbb = vec3 -1 -1 -1
  in
    List.concat
      [ face green  rft rfb rbb rbt   -- right
      , face blue   rft rfb lfb lft   -- front
      , face yellow rft lft lbt rbt   -- top
      , face red    rfb lfb lbb rbb   -- bottom
      , face purple lft lfb lbb lbt   -- left
      , face orange rbt rbb lbb lbt   -- back
      ]


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
face rawColor a b c d =
  let
    color =
      let c = toRgb rawColor in
      vec3
          (toFloat c.red / 255)
          (toFloat c.green / 255)
          (toFloat c.blue / 255)

    vertex position =
        Vertex color position
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]


-- VIEW

scene : Float -> List Entity
scene angle =
  [ entity vertexShader fragmentShader cube (uniforms angle) ]


uniforms : Float -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms t =
  { rotation = mul (makeRotate (3*t) (vec3 0 1 0)) (makeRotate (2*t) (vec3 1 0 0))
  , perspective = makePerspective 45 1 0.01 100
  , camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  , shade = 0.8
  }


-- SHADERS

vertexShader : Shader { attr | position:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glsl|

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


fragmentShader : Shader {} { u | shade:Float } { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
