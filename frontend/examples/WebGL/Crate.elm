
import Http (..)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Matrix4 (..)
import Graphics.WebGL (..)

-- Define the mesh for a crate
crate : [Triangle { pos:Vec3, coord:Vec3 }]
crate =
    concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle { pos:Vec3, coord:Vec3 }]
rotatedFace (angleX,angleY) = 
    let x = makeRotate (degrees angleX) (vec3 1 0 0)
        y = makeRotate (degrees angleY) (vec3 0 1 0)
        t = x `mul` y `mul` makeTranslate (vec3 0 0 1)
    in
        map (mapTriangle (\x -> {x | pos <- transform t x.pos })) face

face : [Triangle { pos:Vec3, coord:Vec3 }]
face =
    let topLeft     = { pos = vec3 -1  1 0, coord = vec3 0 1 0 }
        topRight    = { pos = vec3  1  1 0, coord = vec3 1 1 0 }
        bottomLeft  = { pos = vec3 -1 -1 0, coord = vec3 0 0 0 }
        bottomRight = { pos = vec3  1 -1 0, coord = vec3 1 0 0 }
    in
        [ (topLeft,topRight,bottomLeft)
        , (bottomLeft,topRight,bottomRight)
        ]

-- View
view : Float -> Mat4
view angle =
    foldr1 mul
        [ perspective
        , camera
        , makeRotate (3*angle) (vec3 0 1 0)
        , makeRotate (2*angle) (vec3 1 0 0)
        ]

perspective : Mat4
perspective = makePerspective 45 1 0.01 100

camera : Mat4
camera = makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)

-- Putting it together
main : Signal Element
main =
    let texture = loadTexture "/texture/woodCrate.jpg"
    in
        webgl (400,400) <~ lift2 scene texture (lift view angle)

angle : Signal Float
angle =
    foldp (\dt theta -> theta + dt / 10000) 0 (fps 25)

scene : Response Texture -> Mat4 -> [Entity]
scene response view =
    case response of
        Waiting     -> []
        Failure _ _ -> []
        Success tex -> [entity vertexShader fragmentShader crate { crate = tex, view = view }]

-- Shaders
vertexShader : Shader { pos:Vec3, coord:Vec3 } { u | view:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 pos;
attribute vec3 coord;
uniform mat4 view;
varying vec2 vcoord;

void main () {
  gl_Position = view * vec4(pos, 1.0);
  vcoord = coord.xy;
}

|]

fragmentShader : Shader {} { u | crate:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
