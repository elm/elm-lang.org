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
  { position = v3 0 -10 eyeLevel, velocity = v3 0 0 0 }

walk : { x:Int, y:Int } -> Person -> Person
walk directions person =
  if getZ person.position > eyeLevel then person else
    let vx = toFloat directions.x
        vy = toFloat directions.y
    in
        { person | velocity <- v3 vx vy (getZ person.velocity) }

jump : Bool -> Person -> Person
jump isJumping person =
  if not isJumping || getZ person.position > eyeLevel then person else
    let (vx,vy,_) = toTuple person.velocity
    in
        { person | velocity <- v3 vx vy 2 }

physics : Float -> Person -> Person
physics dt person =
    let position = person.position `add` V3.scale person.velocity dt
        (x,y,z) = toTuple position
    in
        { person | position <- if z < eyeLevel then v3 x y eyeLevel else position }

gravity : Float -> Person -> Person
gravity dt person =
  if getZ person.position <= eyeLevel then person else
    let v = toRecord person.velocity
    in
        { person | velocity <- v3 v.x v.y (v.z - 2 * dt) }

step : Inputs -> Person -> Person
step (isJumping, directions, dt) person =
    physics dt (gravity dt (jump isJumping (walk directions person)))

-- View
view : (Int,Int) -> Person -> Mat4
view (w,h) person =
    mul (makePerspective 45 (toFloat w / toFloat h) 0.01 100)
        (makeLookAt person.position (person.position `add` j) k)

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
crate : [Triangle { pos:Vec3, coord:Vec3 }]
crate = concatMap rotatedFace [ (0,0), (90,0), (180,0), (270,0), (0,90), (0,-90) ]

rotatedFace : (Float,Float) -> [Triangle { pos:Vec3, coord:Vec3 }]
rotatedFace (angleX,angleY) = 
  let x = makeRotate (degrees angleX) (v3 1 0 0)
      y = makeRotate (degrees angleY) (v3 0 1 0)
      t = x `mul` y `mul` makeTranslate (v3 0 0 1)
  in
      map (mapTriangle (\x -> {x | pos <- transform t x.pos })) face

face : [Triangle { pos:Vec3, coord:Vec3 }]
face =
  let topLeft     = { pos = v3 -1  1 0, coord = v3 0 1 0 }
      topRight    = { pos = v3  1  1 0, coord = v3 1 1 0 }
      bottomLeft  = { pos = v3 -1 -1 0, coord = v3 0 0 0 }
      bottomRight = { pos = v3  1 -1 0, coord = v3 1 0 0 }
  in
      [ (topLeft,topRight,bottomLeft), (bottomLeft,topRight,bottomRight) ]

-- Shaders
vertexShader : Shader { pos:Vec3, coord:Vec3 } { u | view:Mat4 } { vcoord:Vec2 }
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

fragmentShader : Shader {} { u | crate:Texture } { vcoord:Vec2 }
fragmentShader = [glShader|

precision mediump float;
uniform sampler2D crate;
varying vec2 vcoord;
void main () {
  gl_FragColor = texture2D(crate, vcoord);
}

|]
