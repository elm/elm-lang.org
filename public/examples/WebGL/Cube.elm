
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Create a cube in which each vertex has a position and color

type Vertex = { color:Vec3, position:Vec3 }

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> [Triangle Vertex]
face color a b c d =
  let toV3 color =
        let c = toRgb color
        in  v3 (toFloat c.red / 255) (toFloat c.green / 255) (toFloat c.blue / 255)
      p = Vertex (toV3 color)
  in
      [ (p a, p b, p c), (p c, p d, p a) ]

cube : [Triangle Vertex]
cube =
  let rft = v3  1  1  1   -- right, front, top
      lft = v3 -1  1  1   -- left,  front, top
      lbt = v3 -1 -1  1
      rbt = v3  1 -1  1
      rbb = v3  1 -1 -1
      rfb = v3  1  1 -1
      lfb = v3 -1  1 -1
      lbb = v3 -1 -1 -1
  in
      concat [ face green  rft rfb rbb rbt   -- right
             , face blue   rft rfb lfb lft   -- front
             , face yellow rft lft lbt rbt   -- top
             , face red    rfb lfb lbb rbb   -- bottom
             , face purple lft lfb lbb lbt   -- left
             , face orange rbt rbb lbb lbt   -- back
             ]

-- Create the scene

main : Signal Element
main = webgl (400,400) <~ lift scene angle

angle : Signal Float
angle = foldp (\dt theta -> theta + dt / 5000) 0 (fps 25)

scene : Float -> [Entity]
scene angle =
    [ entity vertexShader fragmentShader cube (uniforms angle) ]

uniforms : Float -> { rotation:Mat4, perspective:Mat4, camera:Mat4, shade:Float }
uniforms t =
    { rotation = mul (makeRotate (3*t) (v3 0 1 0)) (makeRotate (2*t) (v3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (v3 0 0 5) (v3 0 0 0) (v3 0 1 0)
    , shade = 0.8
    }

-- Shaders

vertexShader : Shader { attr | position:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [shader|

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
fragmentShader = [shader|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
