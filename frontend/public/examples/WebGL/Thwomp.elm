-- Thanks to The PaperNES Guy for the texture:
-- http://the-papernes-guy.deviantart.com/art/Thwomps-Thwomps-Thwomps-186879685

import Graphics.Element (..)
import Http (..)
import List
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Vector3 as V3
import Math.Matrix4 (..)
import Mouse
import Signal
import WebGL (..)
import Window


-- SIGNALS

main : Signal Element
main =
    let faceTexture = loadTexture "/texture/thwomp_face.jpg"
        sideTexture = loadTexture "/texture/thwomp_side.jpg"
        perspectiveMatrix  = Signal.map2 perspective Window.dimensions Mouse.position
    in
        Signal.map4 (view face sides) Window.dimensions faceTexture sideTexture perspectiveMatrix


-- MESHES - define the mesh for a Thwomp's face

type alias Vertex =
    { position : Vec3, coord : Vec3 }


face : List (Triangle Vertex)
face =
    rotatedSquare (0,0)


sides : List (Triangle Vertex)
sides =
    List.concatMap rotatedSquare [ (90,0), (180,0), (270,0), (0,90), (0,-90) ]


rotatedSquare : (Float,Float) -> List (Triangle Vertex)
rotatedSquare (angleXZ,angleYZ) =
    let x = makeRotate (degrees angleXZ) j
        y = makeRotate (degrees angleYZ) i
        t = x `mul` y
    in
        List.map (mapTriangle (\v -> {v | position <- transform t v.position })) square


square : List (Triangle Vertex)
square =
    let topLeft     = Vertex (vec3 -1  1 1) (vec3 0 1 0)
        topRight    = Vertex (vec3  1  1 1) (vec3 1 1 0)
        bottomLeft  = Vertex (vec3 -1 -1 1) (vec3 0 0 0)
        bottomRight = Vertex (vec3  1 -1 1) (vec3 1 0 0)
    in
        [ (topLeft,topRight,bottomLeft)
        , (bottomLeft,topRight,bottomRight)
        ]


-- VIEW

perspective : (Int,Int) -> (Int,Int) -> Mat4
perspective (w',h') (x',y') =
    let w = toFloat w'
        h = toFloat h'
        x = toFloat x'
        y = toFloat y'

        distance = 6

        eyeX = distance * (w/2 - x) / w
        eyeY = distance * (y - h/2) / h
        eye = V3.scale distance (V3.normalize (vec3 eyeX eyeY distance))
    in
        mul (makePerspective 45 (w/h) 0.01 100)
            (makeLookAt eye (vec3 0 0 0) j)


view : List (Triangle Vertex)
    -> List (Triangle Vertex)
    -> (Int,Int)
    -> Response Texture
    -> Response Texture
    -> Mat4
    -> Element
view mesh1 mesh2 dimensions texture1 texture2 perspective =
    webgl dimensions
        (toEntity mesh1 texture1 perspective ++ toEntity mesh2 texture2 perspective)


toEntity : List (Triangle Vertex) -> Response Texture -> Mat4 -> List Entity
toEntity mesh response perspective =
    case response of
        Waiting     -> []
        Failure _ _ -> []
        Success texture -> 
            [ entity vertexShader fragmentShader mesh { texture=texture, perspective=perspective } ]


-- SHADERS

vertexShader : Shader { position:Vec3, coord:Vec3 } { u | perspective:Mat4 } { vcoord:Vec2 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 perspective;
varying vec2 vcoord;

void main () {
  gl_Position = perspective * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]


fragmentShader : Shader {} { u | texture:Texture } { vcoord:Vec2 }
fragmentShader = [glsl|

precision mediump float;
uniform sampler2D texture;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(texture, vcoord);
}

|]
