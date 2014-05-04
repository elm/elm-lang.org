
module FirstPerson where

import Debug
import Http (..)
import Keyboard
import MJS (..)
import Graphics.WebGL (..)

type Person = { position:V3, velocity:V3 }

eyeLevel : Float
eyeLevel = 1

defaultPerson : Person
defaultPerson =
  { position = v3 0 -6 eyeLevel, velocity = v3 0 0 0 }

walk : { x:Int, y:Int } -> Person -> Person
walk directions person =
  let (_,_,z) = toTuple3 person.position in
  if z > eyeLevel then person else
    let vx = toFloat directions.x
        vy = toFloat directions.y
        (_,_,vz) = toTuple3 person.velocity
    in
        { person | velocity <- v3 vx vy vz }

jump : Bool -> Person -> Person
jump isJumping person =
  let (_,_,z) = toTuple3 person.position in
  if not isJumping || z > eyeLevel then person else
    let (vx,vy,_) = toTuple3 person.velocity
    in
        { person | velocity <- v3 vx vy 2 }

physics : Float -> Person -> Person
physics dt person =
    let position = person.position `add` scale person.velocity dt
        (x,y,z)  = toTuple3 position
    in
        { person | position <- if z < eyeLevel then v3 x y eyeLevel else position }

gravity : Float -> Person -> Person
gravity dt person =
    let position = position
        ( _, _, z) = toTuple3 person.position
        (vx,vy,vz) = toTuple3 person.velocity
    in
        if z <= eyeLevel then person else
          { person | velocity <- v3 vx vy (vz - 2 * dt) }

step : Inputs -> Person -> Person
step (isJumping, directions, dt) person =
    person
      |> walk directions
      |> jump isJumping
      |> gravity dt
      |> physics dt

-- View
view : Person -> M4x4
view person =
    perspective `mul` makeLookAt person.position (person.position `add` v3 0 1 0) (v3 0 0 1)

perspective : M4x4
perspective = makePerspective 45 1 0.01 100

-- Putting it together
main : Signal Element
main =
  let person = foldp step defaultPerson inputs
      world = lift2 worldModel (loadTex "woodCrate.jpg") (lift view person)
  in  lift2 scene person world

scene person world =
    flow down [ asText (toTuple3 person.position)
              , asText (toTuple3 person.velocity)
              , webgl (400,400) world
              ]

type Inputs = (Bool, {x:Int, y:Int}, Float)

inputs : Signal Inputs
inputs =
  let dt = lift (\t -> t/500) (fps 25)
  in  sampleOn dt <| (,,) <~ Keyboard.space ~ Keyboard.arrows ~ dt

worldModel : Response Texture -> M4x4 -> [Model]
worldModel response view =
  case response of
    Waiting     -> []
    Failure _ _ -> []
    Success tex -> [model vertexShader fragmentShader crate { crate = tex, view = view }]

-- Define the mesh for a crate
crate : [Triangle { pos:V3, coord:V3 }]
crate = concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle { pos:V3, coord:V3 }]
rotatedFace (angleX,angleY) = 
  let x = makeRotate (degrees angleX) (v3 1 0 0)
      y = makeRotate (degrees angleY) (v3 0 1 0)
      t = x `mul` y `mul` makeTranslate (v3 0 0 1)
  in
      map (mapTriangle (\x -> {x | pos <- mul4x4 t x.pos })) face

face : [Triangle { pos:V3, coord:V3 }]
face =
  let topLeft     = { pos = v3 -1  1 0, coord = v3 0 1 0 }
      topRight    = { pos = v3  1  1 0, coord = v3 1 1 0 }
      bottomLeft  = { pos = v3 -1 -1 0, coord = v3 0 0 0 }
      bottomRight = { pos = v3  1 -1 0, coord = v3 1 0 0 }
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]

-- Shaders
vertexShader : Shader { pos:V3, coord:V3 } { u | view:M4x4 } { vcoord:V2 }
vertexShader = [glShader|

attribute vec3 pos;
attribute vec3 coord;
uniform mat4 view;
varying vec2 vcoord;
void main () {
  gl_Position = view * vec4(pos, 1.0);
  vcoord = coord.xy;
}

|]

fragmentShader : Shader {} { u | crate:Texture } { vcoord:V2 }
fragmentShader = [glShader|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;
void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
