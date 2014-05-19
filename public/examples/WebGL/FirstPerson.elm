-- Try adding the ability to crouch or to land on top of the crate.

import Http (..)
import Keyboard
import Math.Vector2 (Vec2)
import Math.Vector3 (..)
import Math.Vector3 as V3
import Math.Matrix4 (..)
import Graphics.WebGL (..)
import Window

type Person = { position:Vec3, velocity:Vec3 }

eyeLevel : Float
eyeLevel = 2

defaultPerson : Person
defaultPerson =
  { position = v3 0 eyeLevel -10, velocity = v3 0 0 0 }

walk : { x:Int, y:Int } -> Person -> Person
walk directions person =
  if getY person.position > eyeLevel then person else
    let vx = toFloat -directions.x
        vz = toFloat  directions.y
    in
        { person | velocity <- v3 vx (getY person.velocity) vz }

jump : Bool -> Person -> Person
jump isJumping person =
  if not isJumping || getY person.position > eyeLevel then person else
    let (vx,_,vz) = toTuple person.velocity
    in
        { person | velocity <- v3 vx 2 vz }

physics : Float -> Person -> Person
physics dt person =
    let position = person.position `add` V3.scale dt person.velocity
        (x,y,z) = toTuple position
    in
        { person | position <- if y < eyeLevel then v3 x eyeLevel z else position }

gravity : Float -> Person -> Person
gravity dt person =
  if getY person.position <= eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity <- v3 v.x (v.y - 2 * dt) v.z }

step : Inputs -> Person -> Person
step (isJumping, directions, dt) person =
    physics dt (gravity dt (jump isJumping (walk directions person)))

-- View
view : (Int,Int) -> Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` k) j)

-- Putting it together
main : Signal Element
main =
  let person = foldp step defaultPerson inputs
      entities = world <~ loadTexture "/texture/woodCrate.jpg"
                        ~ lift2 view Window.dimensions person
  in  lift2 scene Window.dimensions entities

scene : (Int,Int) -> [Entity] -> Element
scene (w,h) entities =
    layers [ webgl (w,h) entities
           , container w 100 (midLeftAt (absolute 40) (relative 0.5)) . plainText <|
               "Walk around with a first person perspective.\n" ++
               "Arrows keys to move, space bar to jump."
           ]

type Inputs = (Bool, {x:Int, y:Int}, Float)

inputs : Signal Inputs
inputs =
  let dt = lift (\t -> t/500) (fps 25)
  in  sampleOn dt <| (,,) <~ Keyboard.space ~ Keyboard.arrows ~ dt

world : Response Texture -> Mat4 -> [Entity]
world response view =
  case response of
    Waiting     -> []
    Failure _ _ -> []
    Success tex -> [entity vertexShader fragmentShader crate { crate=tex, view=view }]

-- Define the mesh for a crate
type Vertex = { position:Vec3, coord:Vec3 }

crate : [Triangle Vertex]
crate = concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle Vertex]
rotatedFace (angleXZ,angleYZ) =
  let x = makeRotate (degrees angleXZ) j
      y = makeRotate (degrees angleYZ) i
      t = x `mul` y
  in
      map (mapTriangle (\v -> {v | position <- transform t v.position })) face

face : [Triangle Vertex]
face =
  let topLeft     = Vertex (v3 -1  1 1) (v3 0 1 0)
      topRight    = Vertex (v3  1  1 1) (v3 1 1 0)
      bottomLeft  = Vertex (v3 -1 -1 1) (v3 0 0 0)
      bottomRight = Vertex (v3  1 -1 1) (v3 1 0 0)
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]

-- Shaders
vertexShader : Shader { position:Vec3, coord:Vec3 } { u | view:Mat4 } { vcoord:Vec2 }
vertexShader = [shader|

attribute vec3 position;
attribute vec3 coord;
uniform mat4 view;
varying vec2 vcoord;

void main () {
  gl_Position = view * vec4(position, 1.0);
  vcoord = coord.xy;
}

|]

fragmentShader : Shader {} { u | crate:Texture } { vcoord:Vec2 }
fragmentShader = [shader|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;

void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
