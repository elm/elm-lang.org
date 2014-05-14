
import Graphics.WebGL (..)
import Http (..)
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Vector3 as V3
import Math.Matrix4 (..)
import Mouse
import Window

-- Define the mesh for a crate
type Vertex = { position:Vec3, coord:Vec3 }

face : [Triangle Vertex]
face = rotatedSquare (0,0)

sides : [Triangle Vertex]
sides = concatMap rotatedSquare [ (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedSquare : (Float,Float) -> [Triangle Vertex]
rotatedSquare (angleX,angleY) = 
  let x = makeRotate (degrees angleX) i
      y = makeRotate (degrees angleY) j
      t = x `mul` y
  in
      map (mapTriangle (\v -> {v | position <- transform t v.position })) square

square : [Triangle Vertex]
square =
  let topLeft     = Vertex (v3 -1 1  1) (v3 0 1 0)
      topRight    = Vertex (v3  1 1  1) (v3 1 1 0)
      bottomLeft  = Vertex (v3 -1 1 -1) (v3 0 0 0)
      bottomRight = Vertex (v3  1 1 -1) (v3 1 0 0)
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]

-- View
view : (Int,Int) -> (Int,Int) -> Mat4
view (w',h') (x',y') =
  let w = toFloat w'
      h = toFloat h'
      x = toFloat x'
      y = toFloat y'

      distance = 10

      eyeX = distance * (x - w/2) / w
      eyeY = distance * (y - h/2) / h
      eye = V3.scale (V3.normalize (v3 eyeX distance eyeY)) distance
  in
      foldr1 mul [ makePerspective 45 (w/h) 0.01 100
                 , makeLookAt eye (v3 0 0 0) k
                 ]

-- Putting it together
main : Signal Element
main =
    let faceTexture = loadTexture "/texture/thwomp_face.jpg"
        sideTexture = loadTexture "/texture/thwomp_side.jpg"
        viewMatrix  = lift2 view Window.dimensions Mouse.position
    in
        lift4 (scene face sides) Window.dimensions faceTexture sideTexture viewMatrix

scene : [Triangle Vertex] -> [Triangle Vertex] -> (Int,Int) ->
        Response Texture -> Response Texture -> Mat4 -> Element
scene mesh1 mesh2 dimensions texture1 texture2 view =
    webgl dimensions (toEntity mesh1 texture1 view ++ toEntity mesh2 texture2 view)

toEntity : [Triangle Vertex] -> Response Texture -> Mat4 -> [Entity]
toEntity mesh response view =
  case response of
    Waiting     -> []
    Failure _ _ -> []
    Success texture -> 
        [ entity vertexShader fragmentShader mesh { texture=texture, view=view } ]

-- Shaders
vertexShader : Shader { position:Vec3, coord:Vec3 } { u | view:Mat4 } { vcoord:Vec2 }
vertexShader = [glShader|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 view;
varying vec2 vcoord;
void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]

fragmentShader : Shader {} { u | texture:Texture } { vcoord:Vec2 }
fragmentShader = [glShader|

precision mediump float;
uniform sampler2D texture;
varying vec2 vcoord;
void main () {
  gl_FragColor = texture2D(texture, vcoord);
}

|]
